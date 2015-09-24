library("sand")
library("igraph")

#############################Visualizing Network Data##########################
g.l <- graph.lattice(c(5, 5, 5))
data(aidsblog)
summary(aidsblog)

#circular layout
igraph.options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.5)
par(mfrow=c(1,2))
plot(g.l, layout= layout.circle)
title("5*5*5 Lattice")
plot(aidsblog, layout=layout.circle)
title("Blog Network")

plot(g.l, layout=layout.fruchterman.reingold)
title("5*5*5 Lattice")
plot(aidsblog, layout=layout.fruchterman.reingold)
title("Blog Network")

plot(g.l, layout=layout.kamada.kawai)
title("5*5*5 Lattice")
plot(aidsblog, layout=layout.kamada.kawai)
title("Blog Network")

g.tree <- graph.formula(1-+2, 1-+3, 1-+4, 2-+5, 2-+6, 2-+7, 3-+8, 3-+9, 4-+10)
par(mfrow=c(1,3))
igraph.options(vertex.size=30, edge.arrow.size=0.5, vertex.label=NULL)
#circular
plot(g.tree, layout=layout.circle)
#radial
plot(g.tree, layout=layout.reingold.tilford(g.tree, circular=T))
#layered
plot(g.tree, layout=layout.reingold.tilford)

#bipartite graph
g.bip <- graph.formula(actor1:actor2:actor3, 
                       movie1:movie2, actor1:actor2 - movie1,
                       actor2:actor3 - movie2)
V(g.bip)$type <- grepl("^movie", V(g.bip)$name)
str(g.bip, v=T)

proj <- bipartite.projection(g.bip)
str(proj[[1]])
str(proj[[2]])
#"i" reverse direction [,2:1] 2 columns graphS
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1], vertex.size=30,
     vertex.shape=ifelse(V(g.bip)$type, "rectangle", "circle"), 
     vertex.color=ifelse(V(g.bip)$type,"red", "cyan"))


#####Decorating Graph Layout
library(igraphdata)
data(karate)
str(karate)
V(karate)
summary(karate)
V(karate)$name
V(karate)$color
head(karate)
set.seed(42)

l <- layout.kamada.kawai(karate)
igraph.options(vertex.size=10)
par(mfrow=c(1,1))
plot(karate, layout=l, vertex.label=V(karate))

#start to decorate
V(karate)$label <- sub("Actor", "", V(karate)$name)
#two leaders get shapes different from club members
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
#differentiate two factions by color
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"

#Vertex area proportional to vertex strength
#(i.e.,total weight of incident edges)
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2 <- V(karate)$size*0.5

#Weight edges by number of common activities
E(karate)$width <- E(karate)$weight

#color edges by within/between faction
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[F1 %--% F1]$color <- "pink"
E(karate)[F2 %--% F2]$color <- "lightblue"
E(karate)[F1 %--% F1]$color <- "yellow"

#offset vertex labels for smaller points(default=0)
V(karate)$label.dist <- ifelse(V(karate)$size>=10, 0, 0.75)

tiff('karate network.tiff', width=800,height=800 )
plot(karate, layout=l)
dev.off()

########lageza lawyer example
library(sand)
data(lazega)
summary(lazega)
#office location indicated by color
colbar <- c("red", "dodgerblue", "goldenrod")
v.colors <- colbar[V(lazega)$Office]

#Type of practice indicated by vertex shape
v.shape <- c("circle", "square")[V(lazega)$Practice]

#Vertex size proportional to years with firm
v.size <- 3.5*sqrt(V(lazega)$Years)

#Label vertices according to seniority
v.label <- V(lazega)$Seniority

set.seed(42)
l <- layout.fruchterman.reingold(lazega)
plot(lazega, layout=l, vertex.color=v.colors, 
      vertex.shape=v.shape, vertex.size=v.size, vertex.label = v.label)

#Visualizing large networks
summary(fblog)
party.names <- sort(unique(V(fblog)$PolParty))
set.seed(42)
l = layout.kamada.kawai(fblog)
party.nums.f <- as.factor(V(fblog)$PolParty)
party.nums <- as.numeric(party.nums.f) 

plot(fblog, layout=l, vertex.label=NA, vertex.color=party.nums, vertex.size=3)

set.seed(42)
l <- layout.drl(fblog)
plot(fblog, layout=l, vertex.size=5, vertex.label=NA, vertex.color=party.nums)

##clustering exists
fblog.c <- contract.vertices(fblog, party.nums)
E(fblog.c)$weight <- 1
#Simple graphs are graphs which do not contain loop and multiple edges.
fblog.c <- simplify(fblog.c)

party.size <- as.vector(table(V(fblog)$PolParty))
plot(fblog.c, vertex.size=5*sqrt(party.size), vertex.label=party.names, 
     vertex.color=V(fblog.c), edge.width=sqrt(E(fblog.c)$weight),
     vertex.label.dist=1.5, edge.arrow.size=0)

#########egocentric network visualization
data(karate)
#extracting first order neighborhoods surrounding each vertex
k.nbhds <- graph.neighborhood(karate, order=1)
sapply(k.nbhds, vcount)

k.1 <- k.nbhds[[1]]
V(k.1)
vcount(k.1)
k.34 <- k.nbhds[[34]]
V(k.34)
par(mfrow=c(1,2))
plot(k.1, vertex.label=NA, vertex.color=c("red", rep("lightblue", 16)))
plot(k.34, vertex.color=c(rep("lightblue", 17), "red"))






