# One of the principal virtues of creating text networks is that one can apply
# community detection algorithms to them in order to cluster documents according
# to similarities in their content. This approach has some nice advantages over
# topic modeling that I describe in a recent paper

TextCommunities<-function(text_network, alpha=.25){
  
  
  if (igraph::has.multiple(text_network))
    stop("This disparity filter does not yet support multiple edges")
  if (is.null(V(text_network)$name)){
    text_network <- set_vertex_attr(text_network, "name", value = as.character(1:vcount(text_network)))
  }
  
  #create network backbone 
  
  e <- cbind(igraph::as_data_frame(text_network)[, 1:2 ], weight = E(text_network)$weight)
  
  # in
  w_in <- graph.strength(text_network, mode = "in")
  w_in <- data.frame(to = names(w_in), w_in, stringsAsFactors = FALSE)
  k_in <- degree(text_network, mode = "in")
  k_in <- data.frame(to = names(k_in), k_in, stringsAsFactors = FALSE)
  
  e_in <- e %>%
    left_join(w_in, by = "to") %>%
    left_join(k_in, by = "to") %>%
    mutate(alpha_in = (1-(weight/w_in))^(k_in-1))
  
  # out
  
  w_out <- graph.strength(text_network, mode = "out")
  w_out <- data.frame(from = names(w_out), w_out, stringsAsFactors = FALSE)
  k_out <- degree(text_network, mode = "out")
  k_out <- data.frame(from = names(k_out), k_out, stringsAsFactors = FALSE)
  
  e_out <- e %>%
    left_join(w_out, by = "from") %>%
    left_join(k_out, by = "from") %>%
    mutate(alpha_out = (1-(weight/w_out))^(k_out-1))
  
  e_full <- left_join(e_in, e_out, by = c("from", "to", "weight"))
  
  e_full <- e_full %>%
    mutate(alpha = ifelse(alpha_in < alpha_out, alpha_in, alpha_out)) %>%
    select(from, to, alpha)
  
  E(text_network)$alpha <- e_full$alpha
  
  pruned <- delete.edges(text_network, which(E(text_network)$alpha >= alpha))
  pruned <- delete.vertices(pruned, which(degree(pruned) == 0))
  
  
  # make degree for labelling most popular nodes
  V(pruned)$degree <- degree(pruned)
  
  # remove isolates
  isolates <- V(pruned)[degree(pruned)==0]
  pruned <- delete.vertices(pruned, isolates)
  
  
  library(igraph)
  communities<-cluster_louvain(text_network)
  results<-data.frame(cbind(communities$names, communities$membership),
                      stringsAsFactors = FALSE)
  names(results)<-c("group","modularity_class")
  return(results)
}
