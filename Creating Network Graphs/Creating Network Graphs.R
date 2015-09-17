# install.packages("sand")
# install.packages("igraph")

library("sand")
library("igraph")

#############################manipulation network data######################
############ Undirected graph
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
g
# Vertex
V(g)
# Edge
E(g)
str(g)
plot(g)

########### Directed graph
Dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(Dg)

dg <- graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)
str(dg)
plot(dg)
#change the name of Vertex 
V(dg)$name <- c("Bob", "Mary", "Tom")


########## Representations for Graph
E(dg)
#return a two-column R matrix  
get.adjacency(g)
# return edge list 
get.edgelist(g)

#network graph read by adjacency edge  
dg2 <- get.adjacency(dg)
plot(graph.adjacency(dg2))
#also can be read by graph.edgelist graph.adjlist

########## Decorating Network Graphs
V(dg)
V(dg)$gender <- c("M", "F", "M")
V(dg)$color <- "red"
plot(dg)

########## weighted graph 
is.weighted(dg) 
wdg <- dg
plot(wdg)
# vcount gives the number of vertices in the graph.
# ecount gives the number of edges in the graph.
#runif uniform distribution 
E(wdg)$weight <- runif(ecount(wdg))
is.weighted(wdg)
wdg$name <- "friends' relationship"

######### using data frame
g.lazega <- graph.data.frame(elist.lazega, directed="FALSE", vertices=v.attr.lazega)
g.lazega$name <- "Lazega Lawyer"
plot(g.lazega)
vcount(g.lazega)
ecount(g.lazega)
#show vertex attributes 
list.vertex.attributes(g.lazega)

########basic graph concepts
is.simple(g)
#TRUE
plot(g)
mg <- g + edge(2,3)
str(mg)
is.simple(mg)
#FALSE

#show neighbor
neighbors(g, 5)

#degree
degree(g)
degree(dg, mode="in")
degree(dg, mode="out")

is.connected(g)
clusters(g)

is.connected(dg, mode="weak")
is.connected(dg, mode="strong")

#geodesic distance
diameter(g, weights=NA)

##########Special Types of Graphs
g.full <- graph.full(7)
g.ring <- graph.ring(7)
g.tree <- graph.tree(7, children=3, mode="undirected")
g.star <- graph.star(7, mode="undirected")
par(mfrow=c(2,2))
plot(g.full)
plot(g.ring)
plot(g.tree)
plot(g.star)

#bipartite graph
g.bip <- graph.formula(actor1:actor2:actor3, 
                       movie1:movie2, actor1:actor2 - movie1,
                       actor2:actor3 - movie2)
V(g.bip)$type <- grepl("^movie", V(g.bip)$name)
str(g.bip, v=T)

proj <- bipartite.projection(g.bip)
str(proj[[1]])
str(proj[[2]])


