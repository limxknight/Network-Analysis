########Dynamic Network
library("sand")

data(hc)
summary(hc)
head(hc)

####Representation and Manipulation of Dynamic Network
ID.stack <- c(hc$ID1, hc$ID2)
Status.stack <- c(as.character(hc$S1), as.character(hc$S2))
my.t <- table(ID.stack, Status.stack)
v.status <- character(nrow(my.t))

for(i in (1:nrow(my.t))){
  v.status[i] <- names(which(my.t[i,] != 0))
}
table(v.status)
# ADM MED NUR PAT 
# 8  11  27  29 

status.t <- table(hc$S1, hc$S2)
status.t <- status.t + t(status.t)
diag(status.t) <- round(diag(status.t)/2)
status.t

tmp.es <- paste(hc$S1, "-", hc$S2, sep="")
# create a new character vector as hc rows number
e.status <-  character(dim(hc)[1])
e.status[tmp.es == "ADM-ADM"] <- "ADM-ADM"
e.status[tmp.es == "MED-MED"] <- "MED-MED"
e.status[tmp.es == "NUR-NUR"] <- "NUR-NUR"
e.status[tmp.es == "PAT-PAT"] <- "PAT-PAT"
e.status[tmp.es == "ADM-MED" | tmp.es == "MED-ADM"] <- "ADM-MED"
e.status[tmp.es == "ADM-NUR" | tmp.es == "NUR-ADM"] <- "ADM-NUR"
e.status[tmp.es == "ADM-PAT" | tmp.es == "PAT-ADM"] <- "ADM-PAT"
e.status[tmp.es == "NUR-MED" | tmp.es == "MED-NUR"] <- "MED-NUR"
e.status[tmp.es == "MED-PAT" | tmp.es == "PAT-MED"] <- "MED-PAT"
e.status[tmp.es == "NUR-PAT" | tmp.es == "PAT-NUR"] <- "NUR-PAT"
my.hc <- data.frame(Time = hc$Time/(60*60),
                    ID1 =hc$ID1,
                    ID2 =hc$ID2,
                    Status = e.status)
library(lattice)
histogram(~ Time|Status, data=my.hc, xlab="Hours", layout=c(5,2))

####visuzlizing with network graph
library(igraph)
##important! how to convert data frame to network 
vids <- sort(unique(c(hc$ID1, hc$ID2)))
g.week <- graph.data.frame( hc[, c("ID1", "ID2", "Time")], vertices=data.frame(vids), directed="FALSE")
E(g.week)$Time <- E(g.week)$Time/ (60*60)
# plot(g.week)
# hc[, c("ID1", "ID2", "Time")]

status <- unique(rbind(data.frame(id=hc$ID1, status=hc$S1), data.frame(id=hc$ID2, status=hc$S2)))
V(g.week)$Status <- as.character(status[order(status[,1]), 2])

E(g.week)$weight <- 1
g.week.wgtd <- simplify(g.week)
g.week.wgtd
# E(g.week.wgtd)$weight
is.simple(g.week.wgtd)

g.week.96 <- subgraph.edges(g.week, E(g.week)[E(g.week)$Time <= 96],delete.vertices=FALSE)
vcount(g.week.96)
ecount(g.week.96)
plot(g.week.96)

#return to a list 
g.sl12 <- lapply(1:8, function(i) {
          g <- subgraph.edges(g.week, 
                              E(g.week)[Time > 12*(i-1) & Time <= 12*i], delete.vertices=FALSE)
          simplify(g)
})

sapply(g.sl12, vcount)
sapply(g.sl12, ecount)

install.packages("networkDynamic")
library("networkDynamic")
hc.spls <- cbind((hc$Time-20)/ (60*60),
                 hc$Time/(60*60),
                 hc$ID1,
                 hc$ID2)
hc.dn <- networkDynamic(edge.spells=hc.spls)

#the first edge, if it's active during the first hour or the second hour 
is.active(hc.dn, onset=0, terminus=1, e=c(1))
is.active(hc.dn, onset=1, terminus=2, e=c(1))

#for any given edge we can extract the full set of spells during which it was active
get.edge.activity(hc.dn, e=c(1))
get.edge.activity(hc.dn, e=c(10))

#we can produce eight subnetworks corresponding to successive 12-hour periods
g.sl12.dN <- get.networks(hc.dn, start=0, end=96, time.increment=12)
hc.dn.df <- as.data.frame(hc.dn)
summary(hc.dn.df)

######Visulaization of Dynamic Networks
detach(package:networkDynamic)
l = layout.fruchterman.reingold(g.week.wgtd)
v.cols <- character(75)
v.cols[V(g.week.wgtd)$Status == "ADM"] <- "yellow"
v.cols[V(g.week.wgtd)$Status == "MED"] <- "blue"
v.cols[V(g.week.wgtd)$Status == "NUR"] <- "green"
v.cols[V(g.week.wgtd)$Status == "PAT"] <- "black"

plot(g.week.wgtd, layout=l, vertex.size=3, 
     edge.width = 2*(E(g.week.wgtd)$weight/100),
     vertex.color=v.cols, vertex.label=NA)


par(mfrow=c(2,4), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0.5, 1.0, 0.5, 0))
for (i in (1:8)){
  plot(g.sl12[[i]], layout=l, vertex.size=5, 
       edge.width = 2*(E(g.week.wgtd)$weight)/1000,
       vertex.color = v.cols,
       vertex.label = NA)
  title(paste(12*(i-1), "to", 12*i, "hrs"),cex.main=1.4)
}


######ndtv
tmp.es <- paste(v.status[hc.dn.df$tail], "-", v.status[hc.dn.df$head], sep="")
summary(hc.dn.df)
head(hc.dn.df)

# A <- c(1,1,2,3)
# B <- c("aa", "bb", "cc")
# B[A]
mycols <- numeric(nrow(hc.dn.df))
mycols[tmp.es == "ADM-ADM"] <- 1
mycols[tmp.es == "MED-MED"] <- 2
mycols[tmp.es == "NUR-NUR"] <- 3
mycols[tmp.es == "PAT-PAT"] <- 4
mycols[(tmp.es == "ADM-MED") | (tmp.es == "MED-ADM")] <- 5
mycols[(tmp.es == "ADM-NUR") | (tmp.es == "NUR-ADM")] <- 6
mycols[(tmp.es == "ADM-PAT") | (tmp.es == "PAT-ADM")] <- 7
mycols[(tmp.es == "NUR-MED") | (tmp.es == "MED-NUR")] <- 8
mycols[(tmp.es == "PAT-MED") | (tmp.es == "MED-PAT")] <- 9
mycols[(tmp.es == "NUR-PAT") | (tmp.es == "PAT-NUR")] <- 10
my.palette <- rainbow(10)
#produce plot
ne <- max(hc.dn.df$edge.id)
max.t <- max(hc.dn.df$terminus)
plot(c(0, max.t), c(0, ne), ann=F, type='n')
segments(hc.dn.df$onset, hc.dn.df$edge.id, hc.dn.df$terminus, hc.dn.df$edge.id, 
         col=my.palette[mycols])
title(xlab="Time (hour)", ylab="Interacting Pair(Ordered by First Interaction)")
abline(v=c(11, 35, 59, 83), lty="dashed", lw=2, col="lightgray")
#Add legend to plot
status.pairs <- c("ADM-ADM", "MED-MED", "NUR-NUR", "PAT-PAT", "ADM-NUR",
                  "ADM-MED", "ADM-PAT", "MED-NUR", "MED-PAT", "NUR-PAT")
legend(7, 1170, status.pairs, text.col=my.palette[(1:10)], cex=0.75)

#######Characterization of Dynamic Networks
all.deg <- sapply(g.sl12, degree)
sl.lab <- sapply(1:8, function(i){
  paste(12*(i-1), "-", 12*i, "hrs", sep="")
})
deg.df <- data.frame(Degree=as.vector(all.deg), Slice = rep(sl.lab, each=75),
                     Status = rep(V(g.week)$Status, times=8))
library(ggplot2)
p = qplot(factor(Degree), data=deg.df, geom="bar", fill=Status)
p + facet_grid(Slice ~ .) + xlab("Degree") + ylab("Count")

top.deg <- lapply(1:8, function(i){
  all.deg[,i][rank(all.deg[,i]) >= 70]
})


table(unlist(lapply(1:8, function(i){ 
                          as.numeric(names(top.deg[[i]]))})))

#########degree analysis
V(g.week)$Status[c(7, 15)]

all.str <- sapply(g.sl12, graph.strength)
all.r <- all.str/all.deg
round(all.r[c(7, 15),], digits=2)

######shortest path mean
sp.len <- lapply(1:8, function(i) {
  spl <- shortest.paths(g.sl12[[i]], v=c(7,15),
                        to=V(g.sl12[[i]]),
                         weight=NA)
  spl[spl == Inf] <- NA
  spl
})

ave.spl <- sapply(1:8, function(i){
                  apply(sp.len[[i]], 1, mean, na.rm=T)
})

round(ave.spl, digits=2)

sapply(g.sl12, diameter)
round(sapply(g.sl12, average.path.length), digits=2)


