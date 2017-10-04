
# Finally, many people will want to visualize text networks. This first function
# requires an igraph object. It begins by pruning edges from the network, since
# most of these edges have very low weight, and therefore create a "hairball" style
# network plot. This requires the user to input a number between 0 and 1 that describes
# the quantile above which edge weights  should be kept. Next, it uses the ggraph package
# to create something resembling a pretty plot. Right now a big problem is that text labels
# take up too much space,  I may move to a heatmap implementation to get at this.

visualize<-function(text_network, prune_cut, label_degree_cut=0, betweenness=FALSE){

  library(ggraph)
  #prune edges
  pruned <- delete.edges(text_network, E(text_network) [ weight < quantile(weight, prune_cut) ])
  #make degree for labelling most popular nodes
  V(pruned)$degree <- degree(pruned)
  #remove isolates
  isolates <- V(pruned)[degree(pruned)==0]
  pruned <- delete.vertices(pruned, isolates)
  #calculate modularity for coloring
  communities<-cluster_louvain(text_network)
  V(pruned)$modularity<-communities$memberships
  #calculate betweenness for sizing nodes
  size=2
  if(betweenness){
    size<-betweenness(text_network)
  }
  #make visualization
  ggraph(pruned, layout = "fr") +
    geom_node_point(color = V(pruned)$modularity, size = size) +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE)+
    geom_node_text(aes(label = name, filter=degree>label_degree_cut), repel = TRUE, size=2) +
    theme_void()
}
