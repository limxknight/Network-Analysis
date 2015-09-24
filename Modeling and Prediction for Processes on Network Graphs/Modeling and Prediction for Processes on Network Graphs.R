library(sand)
set.seed(42)
data(ppi.CC)

######Nearest Neighbor Methods
summary(ppi.CC)
V(ppi.CC)$ICSC[1:10]
V(ppi.CC)[ICSC == 1]$color <- "yellow"
V(ppi.CC)[ICSC == 0]$color <- "blue"
plot(ppi.CC, vertex.size=5, vertex.label=NA)

clu <- clusters(ppi.CC)
ppi.CC.gc <- induced.subgraph(ppi.CC, clu$membership==which.max(clu$csize))
summary(ppi.CC.gc)
nn.ave <- sapply(V(ppi.CC.gc), function(x) mean(V(ppi.CC.gc)[nei(x)]$ICSC))


par(mfrow=c(2,1))
hist(nn.ave[V(ppi.CC.gc)$ICSC == 1], col="yellow", ylim=c(0, 30), xlab="Proportion Neightbors w/ICSC",
     main="Egos w/ICSC")
hist(nn.ave[V(ppi.CC.gc)$ICSC == 0], col="blue", ylim=c(0, 30), xlab="Proportion Neightbors w/ICSC",
     main="Egos w/out ICSC")

nn.pred <- as.numeric(nn.ave > 0.5)
mean(as.numeric(nn.pred != V(ppi.CC.gc)$ICSC))
