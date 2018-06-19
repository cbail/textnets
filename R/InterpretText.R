InterpretText <- function(text_network, tidytextobject){
  
  #first create communities (either louvain or fast.greedy)
  communities <- cluster_louvain(text_network)
  #optimal modularity kept crashing :(
  #communities<-optimal.community(text_network)
  results<-data.frame(cbind(communities$names, communities$membership),
                      stringsAsFactors = FALSE)
  names(results)<-c("group", "modularity_class")
  
  #now merge with tidy text dataframe
  # determine network type
  node_type <- ifelse(names(tidytextobject)[1]=="lemma",
                      "words",
                      "groups")
  
  # remove grouping allow arranging after adding tfidf
  tidytextobject <- ungroup(tidytextobject)
  
  # cadd tfidf for node_type groups
  if(node_type=="groups"){
    
    # rename grouping variable for easier referencing
    names(tidytextobject)[1] <- "group"
    
    # add tfidf for adjacency matrix
    tfidf_text <- tidytextobject %>%
      # calculate tfidf
      bind_tf_idf(lemma, group, count)
  }
  
  # add tfidf for node_type words
  if(node_type=="words"){
    
    # rename grouping variable for easier referencing
    names(tidytextobject)[2] <- "group"
    
    # add tfidf for adjacency matrix
    tfidf_text <- tidytextobject %>%
      # calculate tfidf
      bind_tf_idf(group, lemma, count)
  }
    
  
  inner_join(tfidf_text, results) %>%
    #summarize words by modularity class
    group_by(modularity_class) %>%
    top_n(n=10, wt=tf_idf) %>%
    select(modularity_class, lemma)
}
