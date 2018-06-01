backbone <- function(g, alpha = 0.05){
  
  if (igraph::has.multiple(g))
    stop("This disparity filter does not yet support multiple edges")
  if (is.null(V(g)$name)){
    g <- set_vertex_attr(g, "name", value = as.character(1:vcount(g)))
  }
  
  e <- cbind(igraph::as_data_frame(g)[, 1:2 ], weight = E(g)$weight)
  
  # in
  w_in <- graph.strength(g, mode = "in")
  w_in <- data.frame(to = names(w_in), w_in, stringsAsFactors = FALSE)
  k_in <- degree(g, mode = "in")
  k_in <- data.frame(to = names(k_in), k_in, stringsAsFactors = FALSE)
  
  e_in <- e %>%
    left_join(w_in, by = "to") %>%
    left_join(k_in, by = "to") %>%
    mutate(alpha_in = (1-(weight/w_in))^(k_in-1))
  
  # out
  
  w_out <- graph.strength(g, mode = "out")
  w_out <- data.frame(from = names(w_out), w_out, stringsAsFactors = FALSE)
  k_out <- degree(g, mode = "out")
  k_out <- data.frame(from = names(k_out), k_out, stringsAsFactors = FALSE)
  
  e_out <- e %>%
    left_join(w_out, by = "from") %>%
    left_join(k_out, by = "from") %>%
    mutate(alpha_out = (1-(weight/w_out))^(k_out-1))
  
  e_full <- left_join(e_in, e_out, by = c("from", "to", "weight"))
  
  e_full <- e_full %>%
    mutate(alpha = ifelse(alpha_in < alpha_out, alpha_in, alpha_out)) %>%
    select(from, to, alpha)
  
  E(g)$alpha <- e_full$alpha
  
  g <- delete.edges(g, which(E(g)$alpha >= alpha))
  g <- delete.vertices(g, which(degree(g) == 0))
  
  return(g)
  
}
