
# I envision that many people will use this package inductively- that is, they will visualize large networks and then want to explore who is connected and why. To this
# end, I've implemented a d3js style interactive plot that lets people mouse-over
# nodes to figure out which documents they represent.


visualize_d3js<-function(text_network, prune_cut, height = NULL, width = NULL, bound = FALSE, zoom = FALSE){
  pruned <- delete.edges(text_network, E(text_network) [ weight < quantile(weight, prune_cut) ])

  lc <- cluster_louvain(pruned)
  members <- membership(lc)
  text_d3<-igraph_to_networkD3(pruned, group=members, what = "both")
  forceNetwork(Links = text_d3$links, Nodes = text_d3$nodes,
               Source = 'source', Target = 'target', NodeID = 'name',
               Group="group", height = height, width = width, bounded = bound, zoom = zoom)

}
