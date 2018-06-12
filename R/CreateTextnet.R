#### CREATING NETWORK OBJECTS FROM TEXT DATASETS

# This is the workhorse function of the package, it requires a tidy text
# dataset, and a) calculates the term frequency inverse document frequency of
# each word; b) creates a weighted adjacency matrix where the rows and columns
# are documents and the cells describe the sum of overlapping TFIDF between them;
# and c) creates an igraph object from this adjacency matrix. The user specifies
# whether nodes should be documents or words.

CreateTextnet <- function(tidytextobject){
  
  # determine network type
  node_type <- ifelse(names(tidytextobject)[1]=="lemma",
                      "words",
                      "groups")
  
  # remove grouping allow arranging after adding tfidf
  tidytextobject <- ungroup(tidytextobject)
  
  # create adjacency matrix for node_type groups
  if(node_type=="groups"){
    
    # rename grouping variable for easier referencing
    names(tidytextobject)[1] <- "group"
    
    # add tfidf for adjacency matrix
    for_adjacency <- tidytextobject %>%
      # calculate tfidf
      bind_tf_idf(lemma, group, count) %>%
      # sort on lemma
      arrange(lemma)
    
    # remove lemmas used by only one author
    for_adjacency <- for_adjacency %>% 
      group_by(lemma) %>%
      filter(n() > 1) %>% 
      ungroup()
    
    # create sparse matrix
    # this appears to work and provides a convenient wrapper around sparseMatrix
    # the error appears to be related to some deprecated functions
    for_crossprod <- cast_sparse(for_adjacency, row = group, col = lemma, value = tf_idf)
        # for_crossprod <- sparseMatrix(i=match(for_adjacency$group, unique(for_adjacency$group)),
        #                             j=match(for_adjacency$lemma, unique(for_adjacency$lemma)),
        #                             x=for_adjacency$tf_idf,
        #                             dimnames=list(unique(for_adjacency$group),
        #                                           unique(for_adjacency$lemma)))
    for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
    # the line above is not working with the noun phrase function
    # create weighted adjacency matrix
    weighted_adjacency <- Matrix::tcrossprod(for_crossprod)
    # create igraph object
    text_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted = TRUE, diag = FALSE)
  }
  
  # create adjacency matrix for node_type words
  if(node_type=="words"){
    
    # rename grouping variable for easier referencing
    names(tidytextobject)[2] <- "group"
    
    # add tfidf for adjacency matrix
    for_adjacency <- tidytextobject %>%
      # calculate tfidf
      bind_tf_idf(group, lemma, count) %>%
      # sort on lemma
      arrange(group)
    
    # create sparse matrix
    # this appears to work and provides a convenient wrapper around sparseMatrix
    # the error appears to be related to some deprecated functions
    for_crossprod <- cast_sparse(for_adjacency, row = lemma, col = group, value = tf_idf)
        # for_crossprod <- sparseMatrix(i=match(for_adjacency$lemma, unique(for_adjacency$lemma)), 
        #                             j=match(for_adjacency$group, unique(for_adjacency$group)), 
        #                             x=for_adjacency$tf_idf, 
        #                             dimnames=list(unique(for_adjacency$lemma), 
        #                                           unique(for_adjacency$group)))
    for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
    #the line above is not working with the noun phrase function
    #create weighted adjacency matrix
    weighted_adjacency <- Matrix::tcrossprod(for_crossprod)
    #create igraph object
    text_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted = TRUE, diag = FALSE)
  }
  
  return(text_network)
  
}
