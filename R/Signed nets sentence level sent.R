#see this site for parts of speech: http://universaldependencies.org/docs/u/dep/index.html
#parts of speech we like: amods, conjucts, VBG, verb-conjuncts
#according to Mark, udpipe needs to be installed before cleanNLP


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


sotu$sotu_text[233]

sent<-c("The name of the game is smearing everyone who dared to vote against Hillary Clinton as a willing accomplice, unwitting tool, or hapless dupe of sinister forces, effectively erasing the 2016 election from the history books.")

final_edges<-as.data.frame(NULL)

for(i in 1:length(sotu$sotu_text)){
  sentences<-strsplit(sotu$sotu_text[i],"\\.")
     for(j in 1:length(sentences[[1]])){
       sentiment<-get_sentiment(sentences[[1]][j])
       #normalize by number of words
       how_long<-sapply(gregexpr("\\W+", sentences[[1]][j]), length) + 1
       sentiment<-sentiment/how_long
       results<-cnlp_annotate(sentences[[1]][j])
       texty_token <- cnlp_get_token(results)
       out<-texty_token[,c("word","tid","upos")]
       nouns<-out[out$upos=="NOUN",]
       nouns$sentiment<-sentiment
       edges<-nouns[,c("word","sentiment")]
       edges$author<-sotu$president[i]
       final_edges<-rbind(final_edges, edges)
     }
print(i)  
}
  
  