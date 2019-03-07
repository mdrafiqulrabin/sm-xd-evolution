setwd("H:/Ph.D/2nd\ Semester/Statistical\ Method\ in\ Research/Project/sm-xd-evolution/Data/FigS1/Data/")

library(igraph)
library(NetSwan)

data_peers = read.csv("UP_All.csv")
data_matrix = data.matrix(data_peers)
gra_s1 <- graph.edgelist(data_matrix, directed=FALSE)

s1_f4 <- swan_combinatory(gra_s1,40)

plot(s1_f4[,1],s1_f4[,5], type='o', col='yellow',xlab="q, fraction of removed links", ylab="Connectivity loss")
lines(s1_f4[,1],s1_f4[,3], type='o', col='red')
lines(s1_f4[,1],s1_f4[,4], type='o', col='orange')
lines(s1_f4[,1],s1_f4[,2], type='o', col='blue')
legend('bottomright',c("Random", "Betweenness", "Degree", "Cascading"), 
       lty=c(1,1,1,1), pch=c(1,1,1,1), 
       col=c("yellow","blue","red", "orange"))



