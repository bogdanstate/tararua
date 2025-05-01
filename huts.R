library('data.table')
library('ggplot2')
library("ggrepel")
library('ggraph')
library('tidygraph')
library('sfnetworks')
library('sf')
library('tflplot')
library('ggpubr')
huts <- fread('huts.tsv',fill=T)
connections <- fread('connections.tsv',fill=T)
huts[,Show:=Show=="T"]
print(head(huts))
print(head(connections))

#huts[,latitude:=sapply(strsplit(LatLon, ","), function(x) 90 + as.numeric(x[1]))]
#huts[,longitude:=sapply(strsplit(LatLon, ","), function(x) as.numeric(x[2]))]
connections[,x:=huts$latitude[match(Node1, huts$Symbol)]] 
connections[,y:=huts$longitude[match(Node1, huts$Symbol)]] 
connections[,xend:=huts$latitude[match(Node2, huts$Symbol)]] 
connections[,yend:=huts$longitude[match(Node2, huts$Symbol)]] 
connections[,node1.id:=Node1]
connections[,node2.id:=Node2]
connections[,circular:=F]

huts[,node.id:=Symbol]
edges <- connections[,.(to=node1.id,from=node2.id,route=ifelse(is.na(Route) | Route=='','NA',Route))]
edges <- edges[,.(route=unlist(strsplit(route, ":"))),by=.(from,to)]#[order(route),]
#edges[,from:=paste(from,route,sep="-")]
#edges[,to:=paste(to,route,sep="-")]
edges[is.na(route),route:="NA"]
nodes <- huts[,.(node.id,route=Route,Type,longitude,latitude,Show,Symbol,Name)]
nodes[is.na(route),route:="NA"]
#nodes <- nodes[,.(route=unique(c(unlist(strsplit(route, ":")),'NA')),latitude,longitude),by=.(node.id)]
#nodes <- nodes[route!='NA',]
#nodes <- subset(nodes, !is.na(route) && route != 'NA')
#nodes[route=="SC",latitude:=latitude-0.001]
#nodes[route=="SC",longitude:=longitude-0.002]
#nodes[route=="JH",latitude:=latitude-0.001]
#nodes[route=="JH",longitude:=longitude-0.002]
#nodes[route=="SKV",latitude:=latitude+0.001]
#nodes[route=="SKV",longitude:=longitude+0.002]
#edges <- rbind(edges,
#    nodes[,.(
#      from=paste(node.id,route[1:length(route)-1],sep='-'),
#      to=paste(node.id,route[2:length(route)],sep='-'),
#      route=route[2:length(route)]
#    ),by=node.id][route %in% c('SC','JH','SKV'),.(from,to,route)]
#)
# Assuming your sfnetwork object is named 'net'

reverse_north_south_edges <- function(net) {
  # Get node coordinates
  node_coords <- sf::st_coordinates(sf::st_geometry(tidygraph::activate(net, "nodes")))

  # Extract edges
  edges <- tidygraph::activate(net, "edges") %>% tibble::as_tibble()
  nodes <- tidygraph::activate(net, "nodes") %>% tibble::as_tibble()

  # For each edge, determine if it goes from North to South
  from_y <- node_coords[edges$from, "Y"]
  to_y <- node_coords[edges$to, "Y"]

  # Identify North-to-South edges (from_y > to_y)
  ns_edges <- which(from_y < to_y)

  # Create a new edges dataframe with reversed directions for N-S edges
  for (i in ns_edges) {
    print(i)
    # Swap from and to for these edges
    temp_from <- edges$from[i]
    temp_to <- edges$to[i]
    edges$from[i] <- temp_to
    edges$to[i] <- temp_from

    # If there are geometric edge features, reverse those too
    if ("geom" %in% names(edges)) {
      edges$geometry[i] <- sf::st_reverse(edges$geometry[i])
    }
  }

  # Reconstruct the sfnetwork
  new_net <- sfnetworks::sfnetwork(nodes, edges)

  return(new_net)
}
nodes[,node.label:=node.id]
#nodes[,node.id:=paste(node.id,route,sep="-")]
nodes <- nodes[,.(node.id,route,Type,Show,latitude,longitude,Name,st_as_sf(data.frame(list(wkt=sprintf('POINT(%3.8f %3.8f)', longitude, latitude))), wkt='wkt'))]
print(head(nodes$node.id))
print(head(edges[!route %in% c('SC', 'JH', 'SKV')]))
print(head(edges[!from %in% nodes$node.id,]))
print(head(nodes[!node.id %in% c(edges$from,edges$to),]))
print(setdiff(edges$from, nodes$node.id))
print(setdiff(edges$to, nodes$node.id))
edges[,width:=ifelse(route=='NA',1,10)]
nodes[,size:=ifelse(route=='NA',1,10)]
edges[,route:=factor(route)]
nodes[,route:=factor(route,levels=levels(edges$route))]
nodes[,shape:=factor(ifelse(Type=='Summit',2,1),levels=c(1,2),labels='Triangle','Square')]
nodes <- nodes[order(longitude, latitude),]
graph <- sfnetworks::as_sfnetwork(tbl_graph(nodes=nodes, edges=edges))
edges.to.delete <- igraph::E(graph)[which(igraph::E(graph)$route=='NA')]
routes <- unique(edges$route)
priorities <- 1:length(routes)
names(priorities) <- routes
print(edges.to.delete)
print(nodes)
igraph::V(graph)$degree <- igraph::degree(igraph::simplify(igraph::delete.edges(graph,edges.to.delete)))
igraph::V(graph)$Show <- ifelse(nodes$Show,TRUE,FALSE)
igraph::V(graph)$Label <- ifelse(is.na(nodes$Name),"",nodes$Name)
prior <- create_layout(graph, 'sf')
pal <- tfl_pal(palette="underground", n=11, type="discrete")
edges <- edges[order(route),edge.id:=1:.N[order(route)]]
print(head(edges))
print(head(nodes))
routes <- c(
  "JH"="Jumbo Holdsworth",
  "SKV"="SK Valleys",
  "SC"="Southern Crossing",
  "MSK"="Main Range SK",
  "TSK"="SK Tarn",
  "CSK"="Carkeek SK",
  "NH"="Neill-Winchcombe",
  "SMR"="Southern Main Ranges",
  "NC"="Northern Crossing",
  "TA"="Te Araroa"
)
#graph <- graph %>% activate(edges) %>% dplyr::arrange(factor(route, levels = routes))

g <- ggraph(graph, 'metro', x=prior$x, y=prior$y, grid_space = 0.005, max_movement = 0.004) + 
  geom_node_point(aes(filter=degree <= 2), shape=18,size=1,show.legend=F) + 
  geom_edge_parallel(aes(filter=route=='NA'),color = 'grey', check_overlap=T,angle_calc='along',width=1,show.legend=F)
  g <- g + geom_edge_parallel0(aes(color=factor(route, levels=names(routes), labels=routes),filter=route!='NA'), 
      width=3, 
      check_overlap=T,angle_calc='rot',show.legend=T,linejoin='round',end_cap = circle(2, 'mm'),lineend='round',linemitre=100,n=100,sep=grid::unit("2","mm"),
  )
g <- g + 
  geom_node_point(aes(filter=degree > 2, shape=shape),color='black',fill='white',size=9,show.legend=F) + 
  geom_node_point(aes(filter=degree > 2, shape=shape),color = 'white',size=7,show.legend=F) +
  geom_node_label(aes(filter=Show,label=Label),repel=T,family='Optima') +
  #geom_label_repel(aes(label=Symbol, x=longitude, y=latitude), data=subset(huts, Show)) +
  scale_color_manual(
    values = pal
  ) +
  scale_edge_color_manual(
    values = pal,
  ) +  
  theme_graph() +
  theme(text = element_text(family = "Optima", size=24), plot.title=element_text(family='Optima',size=48)) +
  theme(panel.grid = element_blank(), legend.position="inside") +
  theme(legend.position.inside = c(.73, .08)) + guides(shape='none') +
  labs(title='Tararua Metro Map',edge_color="Route") + guides(edge_color=guide_legend(ncol=2)) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(1, "in"), pad_y = unit(5, "in"),
    rotation=270,
    height=unit(1,"in"),
    width=unit(1,"in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      text_size=0,
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) + annotate('text',y=-40.82100, x=175.21,label='Glory') +
  annotate('text',y=-40.82100, x=175.12,label='Shopping')
#Glory,Glory,-40.813405,175.225368,Metaphor,NA,T
#Shopping,Shopping,-40.813405,175.125368,Metaphor,NA,T
ggsave(g, file='map.png', width=10, height=14)

# p <- ggplot(data=huts)
# p <- p + geom_point(
#   aes(x = longitude, y = latitude),
#   size=4, color="white", fill="white", shape=21, stroke=2) 
# #p <- p + geom_segment(data=connections, 
# #       aes(x=lon1, y=lat1, xend=lon2, yend=lat2, color = 'black'))
# p <- p + geom_text_repel(aes(label=Symbol, x=longitude, y=latitude), data=subset(huts, Type %in% c("Roadend", "Hut", "Summit", "Saddle"))) 
# p <- p + geom_edge_diagonal(data=connections)
# p <- p + theme_void() +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     legend.text = element_text(face = "bold"),
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.margin = margin(1, 1, 1, 1, "cm")
#   ) +
#   # Equal aspect ratio
#   coord_fixed(ratio = 1)
# print(p)
