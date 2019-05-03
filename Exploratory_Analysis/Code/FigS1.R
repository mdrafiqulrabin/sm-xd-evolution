# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Exploratory_Analysis/Data/FigS1/Data/")

# Import library
library(igraph, warn.conflicts=F)
library(NetSwan, warn.conflicts=F)

# Read CSV file
data_peers = read.csv("UP_All.csv")
data_matrix = data.matrix(data_peers)
gra_s1 <- graph.edgelist(data_matrix, directed=F)

# Main
runOrRead=F
numOfItr=10
figS1=""

if (runOrRead==T) {
  #Saved data of swan_combinatory {NetSwan}
  figS1 <- swan_combinatory(gra_s1, numOfItr)
  fn = paste0("result_i",numOfItr,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names=F)
} else{
  # Draw from saved data
  figS1 <- read.csv("result_i10.csv")
}

# Draw plot
plot(1 - figS1[,1],figS1[,5],  type='l', col='yellow', 
     xlab="q, fraction of removed links", 
     ylab="Size of giant component, SG(q)/SG(q=0)", lwd=2)
lines(1 - figS1[,1],figS1[,3], type='l', col='red', lwd=2)
lines(1 - figS1[,1],figS1[,4], type='l', col='black', lwd=2)
lines(1 - figS1[,1],figS1[,2], type='l', col='blue', lwd=2)
legend('bottomleft',c("Random", "Betweenness", "Degree", "Cascading"), 
       lty=c(1,1,1,1), col=c("yellow","blue","red", "black"))

