#### CREATING NETWORK OBJECTS FROM TEXT DATASETS

# This is the workhorse function of the package, it requires a tidy text
# dataset, and a) calculates the term frequency inverse document frequency of
# each word; b) creates a weighted adjacency matrix where the rows and columns
# are documents and the cells describe the sum of overlapping TFIDF between them;
# and c) creates an igraph object from this adjacency matrix. The user specifies
# whether nodes should be documents or words.

create_textnet<-function(tidytextobject, node_type=c("groups","words")){

  if(node_type=="groups"){
    for_adjacency<-tidytextobject %>%
      #calculate tfidf
      bind_tf_idf(word, group, count)
    # I was not able to find a dplyr function that will do this next line
    # this is too bad because this is the slowest of the functions
    for_crossprod<-acast(for_adjacency, group~word, sum,
                         value.var="tf_idf")
    #the line above is not working with the noun phrase function
    #create weighted adjacency matrix
    weighted_adjacency<-tcrossprod(for_crossprod)
    #create igraph object
    text_network<-graph.adjacency(weighted_adjacency, mode="undirected", weighted=TRUE, diag=FALSE)
  }
  if(node_type=="words"){
   for_adjacency<-tidytextobject %>%
     bind_tf_idf(group, word, count)
-    # I was not able to find a dplyr function that will do this next line
-    # this is too bad because this is the slowest of the functions
-    for_crossprod<-acast(for_adjacency, word~group, sum,
-                         value.var="tf_idf")

    #the line above is not working with the noun phrase function
    #create weighted adjacency matrix
    weighted_adjacency<-tcrossprod(for_crossprod)
    #create igraph object
    text_network<-graph.adjacency(weighted_adjacency, mode="undirected", weighted=TRUE, diag=FALSE)
  }

  return(text_network)

}
