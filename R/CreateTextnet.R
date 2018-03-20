#### CREATING NETWORK OBJECTS FROM TEXT DATASETS

# This is the workhorse function of the package, it requires a tidy text
# dataset, and a) calculates the term frequency inverse document frequency of
# each word; b) creates a weighted adjacency matrix where the rows and columns
# are documents and the cells describe the sum of overlapping TFIDF between them;
# and c) creates an igraph object from this adjacency matrix. The user specifies
# whether nodes should be documents or words.

CreateTextnet<-function(tidytextobject, node_type=c("groups","words")){
  
  if(node_type=="groups"){
    
    for_adjacency<-tidytextobject %>%
      #calculate tfidf
      bind_tf_idf(word, group, count) %>%
      #sort on word (MUCH faster
      arrange(word)
    
    for_crossprod<-sparseMatrix(i=match(for_adjacency$group, unique(for_adjacency$group)), 
                                j=match(for_adjacency$word, unique(for_adjacency$word)), 
                                x=for_adjacency$tf_idf, 
                                dimnames=list(unique(for_adjacency$group), 
                                              unique(for_adjacency$word)))
    for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
    #the line above is not working with the noun phrase function
    #create weighted adjacency matrix
    weighted_adjacency<-tcrossprod(for_crossprod)
    #create igraph object
    text_network<-graph.adjacency(weighted_adjacency, mode="undirected", weighted=TRUE, diag=FALSE)
  }
  if(node_type=="words"){
    for_adjacency<-tidytextobject %>%
      bind_tf_idf(group, word, count)
    
    # I was not able to find a dplyr function that will do this next line
    # this is too bad because this is the slowest of the functions
    for_crossprod<-sparseMatrix(i=match(for_adjacency$word, unique(for_adjacency$word)), 
                                j=match(for_adjacency$group, unique(for_adjacency$group)), 
                                x=for_adjacency$tf_idf, 
                                dimnames=list(unique(for_adjacency$word), 
                                              unique(for_adjacency$group)))
    for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
    #the line above is not working with the noun phrase function
    #create weighted adjacency matrix
    weighted_adjacency<-tcrossprod(for_crossprod)
    #create igraph object
    text_network<-graph.adjacency(weighted_adjacency, mode="undirected", weighted=TRUE, diag=FALSE)
  }
  
  return(text_network)
  
}