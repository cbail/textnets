#### CREATING NETWORK OBJECTS FROM TEXT DATASETS

# This is the workhorse function of the package, it requires a tidy text
# dataset as created by PrepText or PrepTextSent, and a) calculates the term 
# frequency inverse document frequency of each word; b) creates a weighted adjacency 
# matrix where the rows and columns are the groups and the cells describe the sum of 
# overlapping TFIDF between them; and c) creates an igraph object from this adjacency 
# matrix. For PrepTextSent objects, b) will be multiplied by the sentiment and the 
# cells in c) are the overlapping TFIDF-sentiment products.

CreateTextnet <- function(tidytextobject){
  
  # determine network type
  node_type <- ifelse(names(tidytextobject)[1]=="lemma",
                      "words",
                      "groups")
  
  # determine if regular or signed nets
  nets_type <- ifelse(ncol(tidytextobject)>3,
                      "signed",
                      "regular")
  
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
    
    # create sparse matrix of tfidfs
    # this appears to work and provides a convenient wrapper around sparseMatrix
    # the error appears to be related to some deprecated functions
    suppressWarnings(for_crossprod <- cast_sparse(for_adjacency, row = group, col = lemma, value = tf_idf))
    for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
    
    # create sparse matrix of sentiment scores and multiply with tfidf matrix
    if(nets_type=="signed"){
      suppressWarnings(sent_matrix <- cast_sparse(for_adjacency, row = group, col = lemma, value = sentiment))
      sent_matrix <- sent_matrix[sort(rownames(sent_matrix)),]
      for_crossprod <- sent_matrix * for_crossprod
    }
    
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
    suppressWarnings(for_crossprod <- cast_sparse(for_adjacency, row = lemma, col = group, value = tf_idf))
    for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
    
    # create sparse matrix of sentiment scores and multiply with tfidf matrix
    if(nets_type=="signed"){
      suppressWarnings(sent_matrix <- cast_sparse(for_adjacency, row = group, col = lemma, value = sentiment))
      sent_matrix <- sent_matrix[sort(rownames(sent_matrix)),]
      for_crossprod <- sent_matrix * for_crossprod
    }
    
    # create weighted adjacency matrix
    weighted_adjacency <- Matrix::tcrossprod(for_crossprod)
    # create igraph object
    text_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted = TRUE, diag = FALSE)
  }
  
  return(text_network)
  
}
