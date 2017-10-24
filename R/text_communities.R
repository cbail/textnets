# One of the principal virtues of creating text networks is that one can apply
# community detection algorithms to them in order to cluster documents according
# to similarities in their content. This approach has some nice advantages over
# topic modeling that I describe in a recent paper

text_communities<-function(text_network){
  library(igraph)
  communities<-cluster_louvain(text_network)
  results<-data.frame(cbind(communities$names, communities$membership),
                      stringsAsFactors = FALSE)
  names(results)<-c("group","modularity_class")
  return(results)
}
