########################################################
## TEXTNETS PACKAGE                                   ##
## signed nets functions                              ##
## Friedolin Merhout                                  ##
## Duke University                                    ##
########################################################

## Data set: sotu
## Step: write functions that classify sentiment at sentence level, extract objects, and assign sentiment to objects


##
## SETTING UP WORK ENVIRONMENT
##


# loading required packages
library(udpipe)
library(cleanNLP)
library(sotu)
library(syuzhet)
library(dplyr)
library(tidyr)
# library(tidytext)
library(reshape2)
library(igraph)
library(ggraph)
library(stringi)


##
## DATA
##


# fictional example data
text_data <- data_frame(doc_ids = c(1,2,3,4,5),
                        doc_text = c("I don't like walls. However, chain migration is a problem.",
                                     "I hate him. We have to tear down walls and build bridges in their place.",
                                     "We love tax cuts. They give the people what they deserve.",
                                     "These hypocritical tax cuts bleed ordinary Americans dry.",
                                     "We love this place we call home. We should protect it with strong borders."),
                        doc_auth = c("A", "B", "C", "D", "E"))

# State of the Union data
sotu <- bind_cols(data_frame(sotu_text), sotu_meta)

# Twitter data
# load("~/Desktop/Dropbox/Textnets Development 2018/Elected Officials Twitter Data.Rdata")
# load("~/Dropbox/Files for Friedo Jan 29 2018/Parsed Bot Reactions-20180221.Rdata")

twitter_test <- sample_n(elected_tweets, 1000)

# remove twitter internal links for abbreviated tweets and line breaks
twitter_test$text <- gsub("https://t.co/.*$|http://t.co/.*$|\\n|#", "", twitter_test$text)

# delete emojis and symbols
twitter_test$text <- iconv(twitter_test$text, "ASCII", "UTF-8",sub='')

# sub &amp for and
twitter_test$text <- gsub("&amp;", "and", twitter_test$text)




##
## WRITING FUNCTIONS
##


# THIS FUNCTION EXTRACTS ALL NOUNS AND THE CORRESPONDING SENTIMENT OF THE SENTENCES CONTAINING THEM

get_noun_sentiments <- function(text_data, lang = "english", min_df = 0, max_df = 1){
  # SETUP
  # ensure first row is numeric document id
  if(!all(text_data[,1]==1:nrow(text_data))){
    text_data <- bind_cols(data_frame(id = 1:nrow(text_data)),
                           text_data)
  }
  
  # make sure first column is named 'id'
  colnames(text_data)[1] <- 'id'
  
  # set up udpipe backend with language support
  cnlp_init_udpipe(lang)
  
  # ANNOTATION, TOKENIZATION, AND DEPENDENCIES
  # annotate texts
  text_data_annotated <- cnlp_annotate(text_data)
  # extract tokens from annotation
  text_data_token <- cnlp_get_token(text_data_annotated)
  # extract dependencies from annotation
  text_data_dependencies <- cnlp_get_dependency(text_data_annotated, get_token = TRUE)
  # combine tokens and depencies 
  text_token_dependencies <- full_join(text_data_dependencies[,c("id", "sid", "tid_target", "relation")],
                                       text_data_token[,c("id", "sid", "tid", "word", "lemma", "pos", "upos")],
                                       by = c("id"="id", "sid"="sid", "tid_target"="tid"))
  
  # SENTIMENT ANALYSIS
  # get sentiment for lemmatized words
  text_token_dependencies$sentiment <- get_sentiment(text_token_dependencies$lemma)
  # get average sentence sentiment (among words with sentiment score)
  text_token_dependencies <- text_token_dependencies %>% group_by(id, sid) %>%
    mutate(ave_sent = mean(sentiment[sentiment!=0], na.rm = TRUE))
  # subset to nouns which are our edges
  text_nouns <- text_token_dependencies %>% filter(upos%in%c("NOUN", "PROPN"))
  
  # NOUN COMPOUNDS
  # retrieve noun compounds
  noun_compound <- which(text_nouns$relation=="compound")
  text_nouns$word[noun_compound+1] <- paste(text_nouns$word[noun_compound],
                                            text_nouns$word[noun_compound+1])
  # remove compounds
  nouns_from_text <- text_nouns %>% filter(relation!="compound")
  
  # TFIDF WEIGHTS
  # get tf-idf for all nouns
  nouns_tfidf <- cnlp_get_tfidf(nouns_from_text[,c("id", "word")], token_var = "word",
                                min_df = min_df, max_df = max_df)
  # force to matrix and transpose tfidf data
  nouns_tfidf <- t(as.matrix(nouns_tfidf))
  # add word column and turn into dataframe for reshaping
  nouns_tfidf <- dplyr::as_data_frame(cbind(nouns_tfidf, word = rownames(nouns_tfidf)))
  # reshape wide to long
  nouns_tfidf <- gather(nouns_tfidf, id, tfidf, -word)
  # add tfidf to output data
  nouns_from_text <- left_join(nouns_from_text, nouns_tfidf)
  
  # ADD META DATA
  if(ncol(text_data)>2){
    nouns_from_text$id <- as.numeric(nouns_from_text$id)
    nouns_from_text <- full_join(nouns_from_text, text_data[,c(1,3:ncol(text_data))], by = "id")
  }
  
  # return nouns
  return(nouns_from_text)
}


# THIS FUNCTION EXTRACTS ALL NOUNS AND CORRESPONDING MODIFIERS TO GET THEIR SENTIMENT

get_noun_modifiers <- function(text_data, lang = "english", min_df = 0, max_df = 1, verbose = FALSE){
  # SETUP
  # ensure first row is numeric document id
  if(!all(text_data[,1]==1:nrow(text_data))){
    text_data <- bind_cols(data_frame(id = 1:nrow(text_data)),
                           text_data)
  }
  
  # make sure first column is named 'id'
  colnames(text_data)[1] <- 'id'
  
  # set up udpipe backend with language support
  cnlp_init_udpipe(lang)
  
  if(isTRUE(verbose)){
  print(paste0("Setup complete at ", eval(Sys.time())))
    }
  
  # ANNOTATION, TOKENIZATION, AND DEPENDENCIES
  # annotate texts
  text_data_annotated <- cnlp_annotate(text_data)
  
  if(isTRUE(verbose)){
    print(paste0("Annotation complete at ", eval(Sys.time())))
  }
  
  # extract tokens from annotation
  text_data_token <- cnlp_get_token(text_data_annotated)
  
  if(isTRUE(verbose)){
    print(paste0("Tokens extracted at ", eval(Sys.time())))
    }
  
  # extract dependencies from annotation
  text_data_dependencies <- cnlp_get_dependency(text_data_annotated, get_token = TRUE)
  
  if(isTRUE(verbose)){
    print(paste0("Dependencies extracted at ", eval(Sys.time())))
    }
  
  # add relations to tokens 
  text_data_token <- full_join(text_data_token[,c("id", "sid", "tid", "word", "lemma", "pos", "upos")],
                               text_data_dependencies[,c("id", "sid", "tid_target", "relation")],
                               by = c("id"="id", "sid"="sid", "tid"="tid_target"))
  
  if(isTRUE(verbose)){
    print(paste0("Dependencies added to tokens at", eval(Sys.time())))
    }
  
  # 
  # names(text_token_dependencies) <- c("id", "sid", "tid", "word", "lemma", "pos", "upos",
  #                                     "relation")
  
  # SENTIMENT ANALYSIS
  ## initial idea:
  ## get all amods and their nouns and get all nouns and their targets
  ## then, get sentiment for amods and targets and assign to nouns
  ## also requires us to get compounds first
  # targets_to_keep <- text_token_dependencies[text_token_dependencies$upos%in%c("NOUN", "PROPN"), c("id","sid", "tid_target")]
  # targets_to_keep <- left_join(targets_to_keep, text_token_dependencies, by = c("id" = "id", "sid" = "sid", "tid_target" = "tid"))
  # names(targets_to_keep) <- names(text_to_keep)
  # text_data_sub <- union(text_to_keep, targets_to_keep) %>% arrange(id, sid, tid)
  
  # now: get sentiment for noun targets, then add amod sentiment
  # get nouns and their sentiment
  nouns_from_text <- text_data_token[text_data_token$upos%in%c("NOUN", "PROPN"), 
                                     c("id", "sid", "tid", "word", "lemma", "upos", "pos", "relation")]
  
  if(isTRUE(verbose)){
    print(paste0("Noun tokens subsetted at ", eval(Sys.time())))
    }
  
  nouns_from_text$noun_sent <- NA
  nouns_from_text$noun_sent <- get_sentiment(nouns_from_text$lemma, language = lang)
  
  if(isTRUE(verbose)){
    print(paste0("Noun sentiment complete at ", eval(Sys.time())))
    }
  
  # get amods and their sentiment
  amods_from_text <- text_data_dependencies[text_data_dependencies$relation=="amod", c("id", "sid", "tid", "word_target", "lemma_target")]
  
  if(isTRUE(verbose)){
    print(paste0("Amod token subset at ", eval(Sys.time())))
    }
  
  names(amods_from_text) <- c("id", "sid", "tid", "amod", "amod_lemma")
  amods_from_text$amod_sent <- NA
  amods_from_text$amod_sent <- get_sentiment(amods_from_text$amod_lemma, language = lang)
  
  if(isTRUE(verbose)){
    print(paste0("Amod sentiment completed at ", eval(Sys.time())))
    }
  
  # combine multiple amods to avoid multiplying nouns
  # following this example: https://stackoverflow.com/a/41001124
  amods_from_text  <- amods_from_text %>%
    group_by(id, sid, tid) %>%
    summarise(amod = paste(amod, collapse = ", "),
              amod_lemma = paste(amod_lemma, collapse = ", "), 
              amod_sent = mean(amod_sent))
  
  if(isTRUE(verbose)){
    print(paste0("Amods combined at ", eval(Sys.time())))
    }
  
  # combine nouns and amods
  nouns_from_text <- left_join(nouns_from_text, amods_from_text)
  
  if(isTRUE(verbose)){
    print(paste0("Amods added to nouns at ", eval(Sys.time())))
    }
  
  
  # calculate combined sentiment
  # need to figure out how to combine sentiments sensibly
  nouns_from_text$sentiment <- NA
  nouns_from_text$sentiment <- ifelse(is.na(nouns_from_text$amod),
                                      nouns_from_text$noun_sent,
                                      nouns_from_text$noun_sent + nouns_from_text$amod_sent)
  
  if(isTRUE(verbose)){
    print(paste0("Sentiment combined at ", eval(Sys.time())))
    }
  
  
  # NOUN COMPOUNDS
  # retrieve noun compounds
  # row numbers of all compound elements
  noun_compound <- which(nouns_from_text$relation=="compound")
  # list of consecutive compound elements
  compound_elements <- split(noun_compound, cumsum(c(1, diff(noun_compound) != 1)))
  # vector of compound bases
  compound_bases <- mapply(`[[`, compound_elements, lengths(compound_elements))+1
  # add compound bases to compound list
  all_compound_elements <- mapply(c, compound_elements, compound_bases)
  # retrieve all text elements and collapse them to get compound nouns
  compound_nouns <- sapply(all_compound_elements, function(x) paste(nouns_from_text$lemma[x], collapse = " "))
  
  if(isTRUE(verbose)){
    print(paste0("Compound nouns gathered at ", eval(Sys.time())))
    }
  
  # retrieve all text elements and collapse them to get all amods for compounds
  compound_amods <- sapply(all_compound_elements, function(x) paste(nouns_from_text$amod_lemma[x], collapse = ", "))
  compound_amods <- gsub("NA, |NA", "", compound_amods)
  compound_amods <- gsub("^$", NA, compound_amods)
  
  if(isTRUE(verbose)){
    print(paste0("Amods for compound nouns gathered at ", eval(Sys.time())))
    }
  
  # combine all noun, amod, and total sentiments for compounds
  compound_noun_sent <- sapply(all_compound_elements, function(x) sum(nouns_from_text$noun_sent[x], na.rm = TRUE))
  compound_amod_sent <- sapply(all_compound_elements, function(x) sum(nouns_from_text$amod_sent[x], na.rm = TRUE))
  compound_sentiment <- sapply(all_compound_elements, function(x) sum(nouns_from_text$sentiment[x], na.rm = TRUE))
  
  if(isTRUE(verbose)){
    print(paste0("Sentiment for compounds computed at ", eval(Sys.time())))
    }
  
  
  # assign compound nouns, compound amods, and respective sentiments to compound bases 
  nouns_from_text$lemma[compound_bases] <- compound_nouns
  nouns_from_text$amod_lemma[compound_bases] <- compound_amods
  nouns_from_text$noun_sent[compound_bases] <- compound_noun_sent
  nouns_from_text$amod_sent[compound_bases] <- compound_amod_sent
  nouns_from_text$sentiment[compound_bases] <- compound_sentiment
  
  if(isTRUE(verbose)){
    print(paste0("Compound elements added at ", eval(Sys.time())))
    }
  
  
  # remove compound elements from dataframe
  nouns_from_text <- nouns_from_text %>% filter(relation!="compound")
  
  if(isTRUE(verbose)){
    print(paste0("Compound rows removed at ", eval(Sys.time())))
    }
  
  
  # TFIDF WEIGHTS
  # get tf-idf for all nouns
  nouns_tfidf <- cnlp_get_tfidf(nouns_from_text[,c("id", "lemma")], token_var = "lemma",
                                min_df = min_df, max_df = max_df)
  
  if(isTRUE(verbose)){
    print(paste0("Tfidf computed at ", eval(Sys.time())))
    }
  
  # force to matrix and transpose tfidf data
  nouns_tfidf <- t(as.matrix(nouns_tfidf))
  # add word column and turn into dataframe for reshaping
  nouns_tfidf <- dplyr::as_data_frame(cbind(nouns_tfidf, lemma = rownames(nouns_tfidf)))
  # reshape wide to long
  nouns_tfidf <- gather(nouns_tfidf, id, tfidf, -lemma)
  
  if(isTRUE(verbose)){
    print(paste0("Tfidf reshaped at ", eval(Sys.time())))
    }
  
  # add tfidf to output data
  nouns_from_text <- left_join(nouns_from_text, nouns_tfidf)
  
  if(isTRUE(verbose)){
    print(paste0("Tfidf joined at ", eval(Sys.time())))
    }
  
  
  # add meta data
  if(ncol(text_data)>2){
    nouns_from_text$id <- as.numeric(nouns_from_text$id)
    nouns_from_text <- full_join(nouns_from_text, text_data[,c(1,3:ncol(text_data))], by = "id")
    
    if(isTRUE(verbose)){
      print(paste0("Meta data added at ", eval(Sys.time())))
      }
    
  }
  
  # return nouns
  return(nouns_from_text)
  
  if(isTRUE(verbose)){
    print(paste0("Run complete at ", eval(Sys.time())))
    }
  
}


# THIS FUNCTION TAKES A NOUN-SENTIMENT OBJECT AND RETURNS AN ADJACENCY MATRIX FOR THE AUTHORS

get_author_adjacency <- function(nouns_from_text, author_var = "name", tfidf_threshold = NULL, min_authors = 1){
  
  # PROVISIONALLY remove all nans
  nouns_from_text <- nouns_from_text[!is.nan(nouns_from_text$sentiment),]
  
  #
  if(!is.null(tfidf_threshold)){
    nouns_from_text <- nouns_from_text %>% filter(tfidf>tfidf_threshold)
  }
  
  # to retain compounds, assign compounds to lemma
  # compounds <- grep("[[:alpha:]][[:blank:]][[:alpha:]]", nouns_from_text$word)
  # nouns_from_text$lemma[compounds] <- nouns_from_text$word[compounds]
  
  # PROVISIONALLY only look at positive sentiment
  # WE MIGHT WANT TO ADD AN ARGUMENT TO ADJUST THIS
  # positive_sentiment <- nouns_from_text[nouns_from_text$sentiment>0,]
  
  ## PRODUCE ADJACENCY MATRIX
  # cells should be: average inverse absolute value of average sentiment
  # difference between words for speakers
  # NEED TO THINK ABOUT:
  # 1. HOW TO IMPLEMENT DISTANCE FOR SPEAKERS W/O SHARED NOUNS
  # 2. HOW TO KEEP TFIDF PER WORD IN WIDE FORMAT (IF WE JUST KEEP IT, IT CREATES ONE ROW PER WORD PER AUTHOR)
  # 3. HOW DO WE ACCOUNT FOR LARGE NUMBERS OF SHARED WORDS VS FEW STRONGLY SENTIMENTED SHARED WORD
  
  # cast to wide word by author format with average word sentiment per author in cells
  noun_sentiment_wide <- dcast(nouns_from_text[,c(author_var, "lemma", "sentiment")], 
                               paste0(rlang::sym(author_var),"~lemma"), mean, na.rm = TRUE)
  rownames(noun_sentiment_wide) <- noun_sentiment_wide[[author_var]]
  noun_sentiment_wide[[author_var]] <- NULL
  
  # remove nouns used by only one author
  noun_sentiment_wide <- Filter(function(x) length(which(!is.nan(x)))>min_authors, noun_sentiment_wide)
  
  # create similarity matrix for edge list
  # first, calculate euclidean distances distances
  author_dissimilarity <- dist(noun_sentiment_wide)
  # turn dist object to matrix
  author_dissimilarity <- as.matrix(author_dissimilarity)
  # turn matrix to vector
  dissimilarity_scores <- as.vector(author_dissimilarity)
  # second, standardize, invert, and flip sign to compute similarity
  similarity_scores <- -(1/as.vector(scale(dissimilarity_scores)))
  # third, replace NAs with 0
  similarity_scores[is.na(similarity_scores)] <- 0
  # # fourth, rescale similarity scores to 0 to 1
  # similarity_scores <- mapply(function(x,y,z){(x-y)/(z-y)}, 
  #                             similarity_scores, 
  #                             min(similarity_scores), 
  #                             max(similarity_scores))
  # populate similarity matrix
  author_similarity <- matrix(similarity_scores, 
                              nrow = nrow(noun_sentiment_wide),
                              dimnames = list(rownames(noun_sentiment_wide), rownames(noun_sentiment_wide)))
  return(author_similarity)
}







##
## TEST FUNCTIONS
##

# stime_nound <- system.time(parsed_sotu_noun <- get_noun_sentiments(sotu))
# stime_mod <- system.time(parsed_sotu_mod <- get_noun_modifiers(sotu))


twitter_noun_sentiment <- get_noun_sentiments(twitter_test)

noun_sentiment_adjacency <- get_author_adjacency(twitter_noun_sentiment, author_var = "real_name")

twitter_noun_modifiers <- get_noun_modifiers(twitter_test)

noun_modifier_adjacency <- get_author_adjacency(twitter_out, author_var = "real_name")


##
## SAVE PARSED SOTU DATA
##

# saveRDS(parsed_sotu_noun, "~/Work/Projects/textnets/parsed_sotu_noun.RDS")
# saveRDS(parsed_sotu_mod, "~/Work/Projects/textnets/parsed_sotu_mod.RDS")








# create igraph object
sotu_ran_net <- graph.adjacency(out, mode="undirected", weighted=TRUE, diag=FALSE)

# 
# V(eu_speech_positive_network)$country <- c("Germany", "Greece", "Greece", "Czech Republic", "Ireland", 
#                                            "Italy", "United Kingdom", "Poland", "France", "Italy", 
#                                            "Italy", "France", "United Kingdom", "Greece", "Germany", 
#                                            "United Kingdom", "Belgium", "Belgium", "Austria", "Netherlands", 
#                                            "Italy", "United Kingdom")
# # role
# V(eu_speech_positive_network)$role <- c("Country leader", "Country leader", "Country leader", 
#                                         "Country leader", "MEP", "MEP", "Country leader", "Country leader", 
#                                         "MEP", "Country leader", 
#                                         "MEP", "MEP", "Country leader", "Country leader", "MEP", 
#                                         "MEP", "MEP", "EC president", "MEP", "Country leader", 
#                                         "Country leader", "Country leader")

#V(text_network)$degree<-degree(text_network)

gg <- ggraph(sotu_ran_net)

gg +
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE)+
  geom_node_text(aes(label = name), repel = TRUE, size=4)+
  # geom_node_text(aes(label = name, filter=degree>2), repel = TRUE, size=2) + 
  # scale_color_manual(values = c("Republican" = "red", "Democrat" = "blue")) + 
  theme_graph()

gg +
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE)+
  geom_node_text(aes(label = name, color = role), repel = TRUE, size=4)+
  # geom_node_text(aes(label = name, filter=degree>2), repel = TRUE, size=2) + 
  # scale_color_manual(values = c("Republican" = "red", "Democrat" = "blue")) + 
  theme_graph()

#plot(text_network)
