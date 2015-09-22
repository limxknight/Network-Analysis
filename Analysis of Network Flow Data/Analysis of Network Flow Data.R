library(sand)
data(calldata)
names(calldata)

#Austrian call network
summary(calldata)
head(calldata)

min.call <- min(calldata$Flow)
calldata$FlowCnt <- round(5 * calldata$Flow / min.call)

W <- xtabs(FlowCnt ~ Orig + Dest, calldata)
g.cd <- graph.adjacency(W, weight=TRUE)
E(g.cd)$weight

#add all in or out together
in.flow <- graph.strength(g.cd, mode="in")
out.flow <- graph.strength(g.cd, mode="out")
vsize <- sqrt(in.flow + out.flow) / 100
pie.vals <- lapply(1:vcount(g.cd), function(i){
  c(in.flow[i], out.flow[i])
})
ewidth <- E(g.cd)$weight / 10^5
#white in, blue out
plot(g.cd, vertex.size=vsize, vertex.shape="pie", vertex.pie=pie.vals, edge.width=ewidth,
     edge.arrow.size=0.1)


calldata$lFlowCnt <- log(calldata$FlowCnt, 10)
calldata$lO.GRP <- log(calldata$O.GRP, 10)
calldata$lD.GRP <- log(calldata$D.GRP, 10)
calldata$lDistRd <- log(calldata$DistRd, 10)

# install.packages("car")
library(car)
scatterplotMatrix( ~ lFlowCnt + lO.GRP + lD.GRP + lDistRd, data=calldata )



######Predicting Network Flows:Traffic Matrix Estimation
# install.packages("networkTomography")
library(networkTomography)
data(bell.labs)
summary(bell.labs)
bell.labs$A

g.bl <- graph.formula(fddi:switch:local:corp ++ Router)
plot(g.bl)

B <- bell.labs$A
Z <- bell.labs$X
X <- bell.labs$Y

library(lattice)
traffic.in <- c("dst fddi", "dst switch", "dst local", "dst corp")
traffic.out <- c("src fddi", "src switch", "src local", "src corp")

my.df <- bell.labs$df
summary(my.df)
my.df$t <- sapply(my.df$time, function(x){
  hrs <- as.numeric(substring(x, 11, 12))
  mins <- as.numeric(substring(x, 14, 15))
  t <- hrs + mins/60
  return (t)
})

#Separate according to whether data
my.df.in <- subset(my.df, nme %in% traffic.in)
head(my.df.in)
my.df.out <- subset(my.df, nme %in% traffic.out)
head(my.df.out)

#Set up trellis plots for each case
p.in <- xyplot((value/2^10) ~ t | nme, data=my.df.in,
               type="l",  col.line="goldenrod", 
               lwd=2, layout= c(1, 4),
               xlab="Hour of Day", ylab="Kbytes/sec")
p.out <- xyplot((value/2^10) ~ t | nme, data=my.df.out,
               type="l",  col.line="red", 
               lwd=2, layout= c(1,4),
               xlab="Hour of Day", ylab="Kbytes/sec")
#Generate trellis plots
print(p.in, position=c(0, 0.5, 1, 1), more=TRUE)
print(p.out, position=c(0, 0, 1, 0.5))










