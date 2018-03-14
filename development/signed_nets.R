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


##
## DATA
##


text_data <- data_frame(doc_ids = c(1,2,3,4,5),
                        doc_text = c("I don't like walls. However, chain migration is a problem.",
                                     "I hate him. We have to tear down walls and build bridges in their place.",
                                     "We love tax cuts. They give the people what they deserve.",
                                     "These hypocritical tax cuts bleed ordinary Americans dry.",
                                     "We love this place we call home. We should protect it with strong borders."),
                        doc_auth = c("A", "B", "C", "D", "E"))

sotu <- bind_cols(data_frame(sotu_text), sotu_meta)


load("/Users/christopherandrewbail/Desktop/Dropbox/Textnets Development 2018/Elected Officials Twitter Data.Rdata")

twitter_test<-sample_n(elected_tweets, 1000)
# 
# library(stringi)
# twitter_test$text<-stri_enc_toutf8(twitter_test$text)

twitter_test$text <- iconv(twitter_test$text, "latin1", "UTF-8",sub='')

twitter_test$text <- gsub("http.*", "", twitter_test$text)

twitter_test$text <- gsub("@|#", " ", twitter_test$text)

twitter_test$word_count<-sapply(gregexpr("\\W+", twitter_test$text), length)-1

twitter_test<-twitter_test[twitter_test$word_count>2,]

# twitter_test<-twitter_test[grep("^[[:blank:]]+$", twitter_test$text),]

out <- get_noun_sentiments(twitter_test[, c("text", "twitter_name", "party")])

twitter_out<-get_noun_modifiers(twitter_test[1,])




##
## WRITING FUNCTIONS
##


# THIS FUNCTION EXTRACTS ALL NOUNS AND THE CORRESPONDING SENTIMENT OF THE SENTENCES CONTAINING THEM

get_noun_sentiments <- function(text_data, lang = "english"){
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
  text_token_dependencies <- data_frame(id = text_data_token$id,
                                        sid = text_data_token$sid,
                                        tid = text_data_token$tid,
                                        word = text_data_token$word,
                                        lemma = text_data_token$lemma,
                                        pos = text_data_token$pos,
                                        upos = text_data_token$upos,
                                        relation = text_data_dependencies$relation)
  
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
  nouns_tfidf <- cnlp_get_tfidf(nouns_from_text[,c("id", "word")], token_var = "word")
  # force to matrix and transpose tfidf data
  nouns_tfidf <- t(as.matrix(nouns_tfidf))
  # add word column and turn into dataframe for reshaping
  nouns_tfidf <- as_data_frame(cbind(nouns_tfidf, word = rownames(nouns_tfidf)))
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

get_noun_modifiers <- function(text_data, lang = "english"){
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
  # combine tokens and dependencies 
  text_token_dependencies <- data_frame(id = text_data_token$id,
                                        sid = text_data_token$sid,
                                        tid = text_data_token$tid,
                                        word = text_data_token$word,
                                        lemma = text_data_token$lemma,
                                        pos = text_data_token$pos,
                                        upos = text_data_token$upos,
                                        relation = text_data_dependencies$relation,
                                        tid_target = text_data_dependencies$tid,
                                        word_dep = text_data_dependencies$word,
                                        lemma_dep = text_data_dependencies$lemma)
  # get all amods and their nouns and all nouns and their targets
  nouns_from_text <- text_token_dependencies[text_token_dependencies$upos%in%c("NOUN", "PROPN"),]
  
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
  nouns_from_text$noun_sent <- NA
  nouns_from_text$noun_sent[nouns_from_text$upos%in%c("NOUN", "PROPN")] <- get_sentiment(nouns_from_text$lemma_dep[nouns_from_text$upos%in%c("NOUN", "PROPN")],language = lang)
  # get amods and their referents
  amods_from_text <- text_token_dependencies[text_token_dependencies$relation=="amod", c("id", "sid", "tid_target", "lemma")]
  names(amods_from_text) <- c("id", "sid", "tid", "amod")
  # add amods to nouns
  nouns_from_text <- left_join(nouns_from_text, amods_from_text)
  nouns_from_text$amod_sent <- NA
  nouns_from_text$amod_sent[!is.na(nouns_from_text$amod)] <- get_sentiment(nouns_from_text$amod[!is.na(nouns_from_text$amod)], language = lang)
  
  # calculate combined sentiment
  # need to figure out how to combine sentiments sensibly
  nouns_from_text$sentiment <- NA
  nouns_from_text$sentiment <- ifelse(is.na(nouns_from_text$amod),
                                      nouns_from_text$noun_sent,
                                      nouns_from_text$noun_sent + nouns_from_text$amod_sent)
  
  # NOUN COMPOUNDS
  # retrieve noun compounds
  noun_compound <- which(nouns_from_text$relation=="compound")
  nouns_from_text$word[noun_compound+1] <- paste(nouns_from_text$word[noun_compound],
                                                 nouns_from_text$word[noun_compound+1])
  # remove first element from compounds
  nouns_from_text <- nouns_from_text %>% filter(relation!="compound")
  
  # TFIDF WEIGHTS
  # get tf-idf for all nouns
  nouns_tfidf <- cnlp_get_tfidf(nouns_from_text[,c("id", "word")], token_var = "word")
  # force to matrix and transpose tfidf data
  nouns_tfidf <- t(as.matrix(nouns_tfidf))
  # add word column and turn into dataframe for reshaping
  nouns_tfidf <- as_data_frame(cbind(nouns_tfidf, word = rownames(nouns_tfidf)))
  # reshape wide to long
  nouns_tfidf <- gather(nouns_tfidf, id, tfidf, -word)
  # add tfidf to output data
  nouns_from_text <- left_join(nouns_from_text, nouns_tfidf)
  
  # add meta data
  if(ncol(text_data)>2){
    nouns_from_text$id <- as.numeric(nouns_from_text$id)
    nouns_from_text <- full_join(nouns_from_text, text_data[,c(1,3:ncol(text_data))], by = "id")
  }
  
  # return nouns
  return(nouns_from_text)
}


# THIS FUNCTION TAKES A NOUN-SENTIMENT OBJECT AND RETURNS AN ADJACENCY MATRIX FOR THE AUTHORS

get_author_adjacency <- function(sentiment_data, tfidf_threshold = NULL){
  
  # PROVISIONALLY remove all nans
  sentiment_data <- sentiment_data[!is.nan(sentiment_data$sentiment),]
  
  #
  if(!is.null(tfidf_threshold)){
    sentiment_data <- sentiment_data %>% filter(tfidf>tfidf_threshold)
  }
  
  # to retain compounds, assign compounds to lemma
  compounds <- grep("[[:alpha:]][[:blank:]][[:alpha:]]", sentiment_data$word)
  sentiment_data$lemma[compounds] <- sentiment_data$word[compounds]
  
  # PROVISIONALLY only look at positive sentiment
  # WE MIGHT WANT TO ADD AN ARGUMENT TO ADJUST THIS
  # positive_sentiment <- sentiment_data[sentiment_data$sentiment>0,]
  
  ## PRODUCE ADJACENCY MATRIX
  # cells should be: average inverse absolute value of average sentiment
  # difference between words for speakers
  # NEED TO THINK ABOUT:
  # 1. HOW TO IMPLEMENT DISTANCE FOR SPEAKERS W/O SHARED NOUNS
  # 2. HOW TO KEEP TFIDF PER WORD IN WIDE FORMAT (IF WE JUST KEEP IT, IT CREATES ONE ROW PER WORD PER AUTHOR)
  # 3. HOW DO WE ACCOUNT FOR LARGE NUMBERS OF SHARED WORDS VS FEW STRONGLY SENTIMENTED SHARED WORD
  
  # cast to wide word by author format with average word sentiment per author in cells
  noun_sentiment_wide <- dcast(sentiment_data[,c("president", "lemma", "sentiment")], 
                               president~lemma, mean, na.rm = TRUE)
  rownames(noun_sentiment_wide) <- noun_sentiment_wide$president
  noun_sentiment_wide$president <- NULL

  
  # create similarity matrix for edge list
  library(cluster)
  
  out <- as.matrix(daisy(noun_sentiment_wide))
  out <- 1/out
  out <- replace_na(out, 0)
  out <- as.matrix(out)
}







##
## TEST FUNCTIONS
##

# stime_nound <- system.time(parsed_sotu_noun <- get_noun_sentiments(sotu))
# stime_mod <- system.time(parsed_sotu_mod <- get_noun_modifiers(sotu))


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
