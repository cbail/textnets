interpret<-function(text_network, tidytext){

  #first create communities (either louvain or fast.greedy)
  communities<-cluster_louvain(text_network)
  #optimal modularity kept crashing :(
  #communities<-optimal.community(text_network)
  results<-data.frame(cbind(communities$names, communities$membership),
                      stringsAsFactors = FALSE)
  names(results)<-c("group","modularity_class")
  #now merge with tidy text dataframe

  tfidf_text<-tidytext %>%
    bind_tf_idf(group, word, count)

  inner_join(tfidf_text, results) %>%
    #summarize words by modularity class
    group_by(modularity_class) %>%
    top_n(n=10, wt=tf_idf) %>%
    select(modularity_class, word)
}
