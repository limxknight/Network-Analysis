library("igraph")

########Vertex Degree
library(sand)
data(karate)
hist(degree(karate), col="lightblue", xlim=c(0, 50), xlab="Vertex Degree", ylab="Frequency", mian="")

summary(karate)
E(karate)$weight
V(karate)$label
karate$name

#degree distribution can exhibit a variety of shapes
# install.packages("igraphdata")
library("igraphdata")
data(yeast)
ecount(yeast)
vcount(yeast)

d.yeast <- degree(yeast)
hist(d.yeast, col="blue", xlab="Degree", ylab="Frequency", main="Degree Distribution")

#Given the nature of the decay in this distribution, a log-log scale is more effective 
#in summarizing the degree information
dd.yeast <- degree.distribution(yeast)
d <- 1:max(d.yeast)-1
ind <- (dd.yeast != 0)
plot(d[ind], dd.yeast[ind], log="xy", col="blue", xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

##neighbors degree and degree
a.nn.deg.yeast <- graph.knn(yeast, V(yeast))$knn
plot(d.yeast, a.nn.deg.yeast, log="xy", col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Averge Neighbor Degree"))



#################Vertex Centrality
A <- get.adjacency(karate, sparse=FALSE)
# install.packages("network")
library(network)
g <- network::as.network.matrix(A)
# install.packages("sna")
library("sna")
V(karate)
V(g)

par(mfrow=c(2,2))
sna::gplot.target(g, degree(g), main="Degree", circ.lab = FALSE, circ.col="skyblue", 
                  usearrows = FALSE, vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")
sna::gplot.target(g, closeness(g), main="closeness", circ.lab = FALSE, circ.col="skyblue", 
                  usearrows = FALSE, vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")
sna::gplot.target(g, betweenness(g), main="betweenness", circ.lab = FALSE, circ.col="skyblue", 
                  usearrows = FALSE, vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")
sna::gplot.target(g, evcent(g), main="eigenvector", circ.lab = FALSE, circ.col="skyblue", 
                  usearrows = FALSE, vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")

###directed graph
library(sand)
data(aidsblog)
l <- layout.kamada.kawai(aidsblog)
par(mfrow=c(1, 2))
plot(aidsblog, layout=l, main="Hubs", vertex.label="", vertex.color="skyblue",
     vertex.size=10*sqrt(hub.score(aidsblog)$vector))
plot(aidsblog, layout=l, main="Authorities", vertex.label="",vertex.color="skyblue", 
     vertex.size=10*sqrt(authority.score(aidsblog)$vector))

###characterizing edges
eb <- edge.betweenness(karate)
E(karate)[order(eb, decreasing=T)[1:4]]


#######Characterizing Network Cohesion
vcount(karate)
table(sapply(cliques(karate), length))
cliques(karate)[sapply(cliques(karate), length) == 5]
table(sapply(maximal.cliques(karate), length))
clique.number(yeast)

cores <- graph.coreness(karate)
par(mfrow=c(1, 1))
sna::gplot.target(g, cores, circ.lab=FALSE, circ.col="skyblue", usearrow=FALSE,
                  vertex.col= cores, edge.col="darkgray")


# aidsblog <- simplify(aidsblog)
# dyad.census(aidsblog)

############Density and Related Notions of Relative Frequency
V(karate)
igraph::neighborhood(karate,1,"Mr Hi")
ego.instr <- induced.subgraph(karate, igraph::neighborhood(karate,1,"Mr Hi")[[1]])
ego.admin <- induced.subgraph(karate, igraph::neighborhood(karate,1,"John A")[[1]])
graph.density(karate)
graph.density(ego.instr)
graph.density(ego.admin)

###Connectvity, Cuts and Flows
detach("package:sna")
detach("package:network")
is.connected(yeast)
# Creates a separate graph for each component of a graph.
comps <- decompose.graph(yeast)
table(sapply(comps, vcount))
yeast.gc <- decompose.graph(yeast)[[1]]
diameter(yeast.gc)



