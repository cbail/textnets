#see this site for parts of speech: http://universaldependencies.org/docs/u/dep/index.html
#parts of speech we like: 

# amods: An adjectival modifier of a noun is any adjectival phrase that serves to modify the meaning of the noun.
# conjucts:
# VBG: 
# verb-conjuncts:


#according to Mark, udpipe needs to be installed before cleanNLP
library(udpipe)
library(cleanNLP)
library(sotu)
library(syuzhet)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)

cnlp_init_udpipe("english")


sotu <- data.frame(cbind(sotu_text, sotu_meta), stringsAsFactors=FALSE)
sotu$sotu_text <- as.character(sotu$sotu_text)

for_nets <- as.data.frame(NULL)

for(i in 1:nrow(sotu)){
  
  # get parts-of-speech
  results <- cnlp_annotate(sotu$sotu_text[i])
  texty_token <- cnlp_get_token(results, combine = TRUE) 
  
  # filter to only adjectival modfiers
  output_new <- texty_token %>%
    select(word, pos, relation, sid) %>%
    filter(relation %in% c('amod', 'obj'))
  
  output_new$word <-as.character(output_new$word)
  output_new$sentiment <- sapply(output_new$word, get_sentiment)
  output_new$sentiment[output_new$relation == "obj"] <- NA
  
  # group by sentenceid and calculate the mean sentiment
  # filter out the adjectival modfiers and their ave_sent
  words <- output_new %>%
    group_by(sid) %>% 
    mutate(ave_sent = mean(sentiment, na.rm=TRUE)) %>%
    ungroup() %>%
    filter(relation != 'amod') %>%
    select(word, ave_sent)
  
  # add the president's name
  words$author <- sotu$president[i]
  for_nets <-rbind(for_nets, words)
  print(i)
}


#group by president and then word
positive_net <- for_nets %>%
  group_by(author, word) %>%
  summarise(sentiment = mean(ave_sent, na.rm=TRUE)) %>%
  filter(!is.nan(sentiment) & sentiment > 0) #only look at positive words


#now produce adjacency matrix where cells are populated by average
# inverse absolutle value of average sentiment difference between 
# words for each president

for_crossprod <- acast(positive_net, author ~ word, sum,
                       value.var="sentiment")

#the line above is not working with the noun phrase function
#create weighted adjacency matrix
weighted_adjacency <- tcrossprod(for_crossprod)

#create igraph object
text_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted=TRUE, diag=FALSE)

#V(text_network)$degree<-degree(text_network)

ggraph(text_network, layout = "fr") +
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE)+
  geom_node_text(aes(label = name), repel = TRUE, size=2)+
  # geom_node_text(aes(label = name, filter=degree>2), repel = TRUE, size=2) +
  theme_void()

#plot(text_network)
