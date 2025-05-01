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
connections[,edge.id:=1:.N]

huts[,node.id:=Symbol]
edges <- connections[,.(to=node1.id,from=node2.id,route=ifelse(is.na(Route) | Route=='','NA',Route))]
edges <- edges[,.(route=unlist(strsplit(route, ":"))),by=.(from,to)]
#edges[,from:=paste(from,route,sep="-")]
#edges[,to:=paste(to,route,sep="-")]
edges[is.na(route),route:="NA"]
nodes <- huts[,.(node.id,route=Route,Type,longitude,latitude)]
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
nodes[,node.label:=node.id]
#nodes[,node.id:=paste(node.id,route,sep="-")]
nodes <- nodes[,.(node.id,route,Type,st_as_sf(data.frame(list(wkt=sprintf('POINT(%3.8f %3.8f)', longitude, latitude))), wkt='wkt'))]
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
graph <- sfnetworks::as_sfnetwork(tbl_graph(nodes=nodes, edges=edges))
igraph::V(graph)$degree <- igraph::degree(graph)
prior <- create_layout(graph, 'sf')
pal <- tfl_pal(palette="underground", n=10, type="discrete")
g <- ggraph(graph, 'metro', x=prior$x, y=prior$y, grid_space = 0.003, max_movement = 0.005) + 
  geom_node_point(aes(filter=degree <= 2,color=route), shape=1,size=4) + 
  geom_edge_parallel(aes(color=route,width=width), check_overlap=T,angle_calc='across',linemitre=100) + 
  geom_edge_parallel(color = 'white', check_overlap=T,angle_calc='along',width=1) + 
  geom_node_point(aes(filter=degree > 2, shape=shape),color='black',fill='white',size=4) + 
  geom_node_point(aes(filter=degree > 2, shape=shape),color = 'white',size=3.7) +
  #geom_label_repel(aes(label=Symbol, x=longitude, y=latitude), data=subset(huts, Show)) +
  scale_color_manual(
    values = pal
  ) +
  scale_edge_color_manual(
    values = pal
  ) +  guides(fill="none", color="none", shape="none", size="none", width="none", route="none") + theme(legend.position="none",panel.grid = element_blank())
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
