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
# library(syuzhet)
# library(reshape2)
# library(igraph)
# library(ggraph)


##
## DATA
##


text_data <- data_frame(doc_ids = c(1,2,3,4),
                        doc_text = c("I don't like walls. However, chain migration is a problem.",
                                     "I hate him. We have to tear down walls and build bridges in their place.",
                                     "We love tax cuts. They give the people what they deserve.",
                                     "These hypocritical tax cuts bleed ordinary Americans dry."),
                        doc_auth = c("A", "B", "C", "D"))

sotu <- bind_cols(data_frame(sotu_text), sotu_meta)

# load descriptions of english specific pos abbreviations
# url <- "https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html"
# en_pos_abb <- url %>%
#   read_html() %>%
#   html_nodes(xpath='/html/body/table') %>%
#   html_table()
# en_pos_abb <- en_pos_abb[[1]][2:nrow(en_pos_abb[[1]]),2:3]
# saveRDS(en_pos_abb, "~/Work/Projects/textnets/english_pos_abbreviations.RDS")
en_pos_abb <- readRDS("~/Work/Projects/textnets/english_pos_abbreviations.RDS")




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
  text_data_dependencies <- cnlp_get_dependency(text_data_annotated)
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
  nouns_from_text$sentiment <- NA
  nouns_from_text$sentiment[nouns_from_text$upos%in%c("NOUN", "PROPN")] <- get_sentiment(nouns_from_text$lemma_dep[nouns_from_text$upos%in%c("NOUN", "PROPN")],language = lang)
  # get amods and their referents
  amods_from_text <- text_token_dependencies[text_token_dependencies$relation=="amod", c("id", "sid", "tid_target", "lemma")]
  names(amods_from_text) <- c("id", "sid", "tid", "amod")
  # add amods to nouns
  nouns_from_text <- left_join(nouns_from_text, amods_from_text)
  nouns_from_text$amod_sent <- NA
  nouns_from_text$amod_sent[!is.na(nouns_from_text$amod)] <- get_sentiment(nouns_from_text$amod[!is.na(nouns_from_text$amod)], language = lang)
  
  # calculate combined sentiment
  # need to figure out how to combine sentiments sensibly
  nouns_from_text$comb_sent <- NA
  nouns_from_text$comb_sent <- ifelse(is.na(nouns_from_text$amod),
                                      nouns_from_text$sentiment,
                                      nouns_from_text$sentiment + nouns_from_text$amod_sent)
  
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







##
## TEST FUNCTIONS
##

stime_nound <- system.time(parsed_sotu_noun <- get_noun_sentiments(sotu))
stime_mod <- system.time(parsed_sotu_mod <- get_noun_modifiers(sotu))


##
## SAVE PARSED SOTU DATA
##

saveRDS(parsed_sotu_noun, "~/Work/Projects/textnets/parsed_sotu_noun.RDS")
saveRDS(parsed_sotu_mod, "~/Work/Projects/textnets/parsed_sotu_mod.RDS")




# add president
parsed_sotu_noun$president <- NA
parsed_sotu_noun$president <- sotu$president[ parsed_sotu_noun$id]




# # subset to objects only, i.e. drop modifiers
# # then group by speaker and word
# eu_speech_nets <- parsed_eu_speech %>%
#   filter(relation=="obj") %>% 
#   group_by(speaker, word) %>%
#   summarise(mean_word_sent = mean(ave_sent, na.rm=TRUE))

# for now, remove all NANs
sotu_noun_nets <- parsed_sotu_noun[!is.nan(parsed_sotu_noun$ave_sent),]
sotu_mod_nets <- parsed_sotu_mod[!is.nan(parsed_sotu_mod$comb_sent),]


# # remove stop words and non-word characters
# eu_speech_nets <- eu_speech_nets[!eu_speech_nets$word%in%cstop_words$word,]
# eu_speech_nets <- eu_speech_nets[-grep("[[:punct:]]", eu_speech_nets$word),]

# only look at positive words
positive_sotu_noun_nets <- sotu_noun_nets[sotu_noun_nets$ave_sent>median(sotu_noun_nets$ave_sent),]
negative_sotu_noun_nets <- sotu_noun_nets[sotu_noun_nets$ave_sent<0,]


# now produce adjacency matrix where cells are populated by average
# inverse absolute value of average sentiment difference between 
# words for each speaker

for_crossprod <- acast(positive_sotu_noun_nets, speaker~word, sum,
                       value.var="mean_word_sent")

# the line above is not working with the noun phrase function
# create weighted adjacency matrix
weighted_adjacency <- tcrossprod(for_crossprod)

# create igraph object
eu_speech_positive_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted=TRUE, diag=FALSE)

# 
V(eu_speech_positive_network)$country <- c("Germany", "Greece", "Greece", "Czech Republic", "Ireland", 
                                           "Italy", "United Kingdom", "Poland", "France", "Italy", 
                                           "Italy", "France", "United Kingdom", "Greece", "Germany", 
                                           "United Kingdom", "Belgium", "Belgium", "Austria", "Netherlands", 
                                           "Italy", "United Kingdom")
# role
V(eu_speech_positive_network)$role <- c("Country leader", "Country leader", "Country leader", 
                                        "Country leader", "MEP", "MEP", "Country leader", "Country leader", 
                                        "MEP", "Country leader", 
                                        "MEP", "MEP", "Country leader", "Country leader", "MEP", 
                                        "MEP", "MEP", "EC president", "MEP", "Country leader", 
                                        "Country leader", "Country leader")

#V(text_network)$degree<-degree(text_network)

gg <- ggraph(eu_speech_positive_network)

gg +
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE)+
  geom_node_text(aes(label = name, color = country), repel = TRUE, size=4)+
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
