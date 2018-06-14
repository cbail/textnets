# The PrepTextSent function adds sentiment analysis to the standard PrepText function.
# This allows creating text networks with edge polarity (i.e. positive or negative ties).
# Unlike the regular PrepText function, this one always return nouns and proper nouns only
# and, accordingly, gets rid of the `pos` and `remove_stop_words` arguments.
# Instead you will need to specify `sentiment_lexicon` which currently only supports afinn and bing
# from the tidytext package. The remaining arguments mirror the regular PrepText function.

PrepTextSent <- function(textdata, groupvar, textvar, node_type=c("groups", "words"),
                         tokenizer = c("words", "tweets"),
                         language = "english", 
                         remove_numbers = NULL, compound_nouns = FALSE,
                         sentiment_lexicon = c("afinn", "bing"),
                         udmodel_lang = NULL,
                         ...){
  
  # remove non-UTF8 characters
  textdata[[textvar]] <- iconv(textdata[[textvar]], "ASCII", "UTF-8",sub='')
  
  # remove emojis, symbols, and meta characters from tweets
  if (tokenizer=="tweets") {
    textdata[[textvar]] <- gsub("&amp;|&lt;|&gt;|RT", "", textdata[[textvar]])
    if (!is.null(remove_numbers) && isTRUE(remove_numbers)) { # && evaluates arg two only if arg one is true
      textdata[[textvar]]<-gsub("\\b\\d+\\b", "",textdata[[textvar]])
    }
  }
  
  if(is.null(udmodel_lang)){
    # udpipe setup
    # download udpipe language model
    lang_mod <- udpipe_download_model(language = language)
    # set up udpipe language model for pos tagging
    udmodel_lang <- udpipe_load_model(file = lang_mod$file_model)
  }
  
  ## DEFAULT: ANNOTATE WORDS NOT COMPOUND NOUNS
  if (isFALSE(compound_nouns)){
    
    # split up text data into sentences
    textdata <- as_tibble(textdata) %>%
      unnest_tokens_(output = "sentences", input = textvar, token = "sentences")
    
    # create sentence id
    textdata <- textdata %>%
      group_by_(groupvar) %>%
      mutate(sid = row_number())
    
    # split up sentences into words and add term id
    textdata_tokens <- unnest_tokens(textdata,
                                     output = "word", input = "sentences",
                                     token = tokenizer, drop = FALSE, ...) %>%
      group_by(sid) %>%
      mutate(tid = row_number())
    
    # get part of speech with udpipe
    # annotate for pos only w/ pre-tokenized data
    # following: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html#annotate_your_text
    textdata_pos <- as.data.frame(udpipe_annotate(udmodel_lang, x = textdata_tokens$word,
                                                  tagger = "default", parser = "none",
                                                  tokenizer = "vertical"))
    
    # combine part of speech and textdata
    textdata <- bind_cols(textdata_tokens, textdata_pos[, c("upos", "lemma")])
    
    # get sentiments for words
    if (length(sentiment_lexicon)>1){
      warning(paste0("You did not specify a sentiment lexicon. Function defaults to afinn"))
      sentiment_lexicon = "afinn"
    }
    if(sentiment_lexicon=="afinn"){
      textdata <- textdata %>%
        left_join(get_sentiments(sentiment_lexicon), by = c("lemma" = "word"))
    } else if (sentiment_lexicon=="bing"){
      textdata <- textdata %>%
        left_join(get_sentiments("bing"), by = c("lemma" = "word")) %>%
        mutate(score = ifelse(sentiment=="negative", -1,
                              ifelse(sentiment=="positive", 1,
                                     NA)))
    } else stop("You did not specify a supported sentiment lexicon.")
  }
  
  ## IF SPECIFIED: ANNOTATE WORDS AND COMPOUND NOUNS
  if (isTRUE(compound_nouns)){
    # split up documents into words
    textdata_tokens <- unnest_tokens_(textdata,
                                      output = "word", input = textvar,
                                      token = tokenizer, drop = FALSE, 
                                      strip_punct = FALSE, ...)
    
    # then we prepare the tokenized documents for dependency parsing
    textdata_tokens <- textdata_tokens %>% 
      group_by_(groupvar) %>% 
      summarise(documents = paste(word, collapse = "\n"))
    
    # parse dependencies with udpipe
    textdata_dep <- as.data.frame(udpipe_annotate(udmodel_lang, x = textdata_tokens$documents,
                                                  doc_id = textdata_tokens[[groupvar]],
                                                  tagger = "default", parser = "default"))
    
    # get sentiments for words
    if (length(sentiment_lexicon)>1){
      warning(paste0("You did not specify a sentiment lexicon. Function defaults to afinn"))
      sentiment_lexicon = "afinn"
    }
    if(sentiment_lexicon=="afinn"){
      textdata <- textdata_dep %>%
        left_join(get_sentiments(sentiment_lexicon), by = c("lemma" = "word"))
    } else if (sentiment_lexicon=="bing"){
      textdata <- textdata_dep %>%
        left_join(get_sentiments("bing"), by = c("lemma" = "word")) %>%
        mutate(score = ifelse(sentiment=="negative", -1,
                              ifelse(sentiment=="positive", 1,
                                     NA)))
    } else stop("You did not specify a supported sentiment lexicon.")
    
    # NOUN COMPOUNDS
    # retrieve noun compounds
    # row numbers of all compound elements
    noun_compound <- which(textdata$dep_rel=="compound")
    # list of consecutive compound elements
    compound_elements <- split(noun_compound, cumsum(c(1, diff(noun_compound) != 1)))
    # vector of compound bases
    compound_bases <- mapply(`[[`, compound_elements, lengths(compound_elements))+1
    # add compound bases to compound list
    all_compound_elements <- mapply(c, compound_elements, compound_bases, SIMPLIFY = FALSE)
    # retrieve all text elements and collapse them to get compound nouns
    compound_nouns <- sapply(all_compound_elements, function(x) paste0(textdata_dep$lemma[x], collapse = " "))
    
    # assign compound nouns to compound bases 
    textdata$lemma[compound_bases] <- compound_nouns
    
    # remove compound elements and punctuation from dataframe
    textdata <- textdata %>% 
      filter(dep_rel!="compound" & upos!="PUNCT")
    
    # subset to relevant variables and rename groupvar to avoid redudant coding
    # figuring out how to rename to groupvar was a pain, solution from here: https://stackoverflow.com/a/26003971
    textdata <- textdata %>% 
      rename(!!groupvar := doc_id,
             sid = sentence_id) %>% 
      select(!!groupvar, sid, lemma, upos, score)
  }
  
  # remove stopwords
  textdata <- textdata %>%
    anti_join(get_stopwords(language = language), by = c("lemma" = "word"))
  
  # compute total sentence sentiment and sentence length for average sentiment
  textdata <- textdata %>% 
    group_by_(groupvar, "sid") %>% 
    mutate(sentence_sent = sum(score, na.rm = TRUE),
           sentence_length = n())
  
  # subset textdata to nouns and proper nouns
  textdata <- textdata %>% 
    filter(upos%in%c("NOUN", "PROPN"))
  
  # calculate sentence level sentiment excluding noun
  if(nrow(textdata)>0){
    textdata$sentiment <- NA
  } else stop(paste0("There are no nouns or proper nouns in ", expr(textdata), "."))
  textdata$sentiment <- ifelse(is.na(textdata$score),
                               textdata$sentence_sent,
                               textdata$sentence_sent - textdata$score)
  
  # count word occurences within grouping variable
  if (length(node_type)>1){
    warning(paste0("You did not specify a node_type. Returned nodes are ", groupvar, "."))
    node_type <- "groups"
  }
  
  if (node_type=="groups"){
    # count terms by group
    textdata <- textdata %>%
      group_by(!!as.name(groupvar), lemma) %>%
      summarise(`count` = n(), sentiment = median(sentiment))
  }
  
  if (node_type=="words"){
    # count groups by term
    textdata <- textdata %>%
      group_by(lemma, !!as.name(groupvar)) %>%
      summarise(`count` = n(), sentiment = median(sentiment))
  }
  
  return(textdata)
}
