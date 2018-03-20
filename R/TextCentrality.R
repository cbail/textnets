# In many cases, such as the Twitter analysis in the running example, one wants
# to not only group documents according to their similarities to each other but
# also understand which documents are most influential. In this case, we might
# want to measure whose discourse on Twitter reaches across the most conversations
# among elected officials.

TextCentrality<-function(text_network){
  between<-data.frame(betweenness(text_network))
  closeness<-data.frame(closeness(text_network))
  output<-cbind(between, closeness)
  names(output)<-c("betweenness_centrality","closness_centrality")
  return(output)
}