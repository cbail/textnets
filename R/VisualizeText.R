
# Finally, many people will want to visualize text networks. This first function
# requires an igraph object. It begins by pruning edges from the network, since
# most of these edges have very low weight, and therefore create a "hairball" style
# network plot. This requires the user to input a number between 0 and 1 that describes
# the quantile above which edge weights  should be kept. Next, it uses the ggraph package
# to create something resembling a pretty plot. Right now a big problem is that text labels
# take up too much space,  I may move to a heatmap implementation to get at this.

VisualizeText <- function(text_network, backbone_alpha, label_degree_cut=0, betweenness=FALSE){
  
  # define backbone function for pruning
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
  
  # subset edges to backbone of the network
  pruned <- backbone(text_network, backbone_alpha)
  
  # make degree for labelling most popular nodes
  V(pruned)$degree <- degree(pruned)
  
  # remove isolates
  isolates <- V(pruned)[degree(pruned)==0]
  pruned <- delete.vertices(pruned, isolates)
  
  # calculate modularity for coloring
  communities <- cluster_louvain(text_network)
  V(pruned)$modularity <- communities$memberships
  
  #calculate betweenness for sizing nodes
  size <- 2
  if(betweenness){
    size <- betweenness(text_network)
  }
  
  # make visualization
  ggraph(pruned, layout = "fr") +
    geom_node_point(color = V(pruned)$modularity, size = size) +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE)+
    geom_node_text(aes(label = name, filter=degree>label_degree_cut), repel = TRUE, size=2) +
    theme_void()
}

