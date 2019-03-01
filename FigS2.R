# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr); library(ggplot2)

# Read Faculty_GoogleScholar_Funding_Data_N4190.csv
df = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
df = df %>% select(dept, KTotal, KDirect, KMediated, XDIndicator)

# FigS2: a. biology
df0 = filter(df, df$dept == "BIO")
hg0 = ggplot(data=df0) +
  geom_histogram(aes(x=df0$KMediated),
                 breaks=seq(0,1000,20),
                 col="red", fill="red", alpha=0.3) +
  geom_histogram(aes(x=df0$KDirect),
                 breaks=seq(0,1000,2),
                 col="blue", fill="blue", alpha=0.3) +
  xlab(expression(paste(d[i],", link degree", sep=""))) + 
  ylab(expression(paste("Count Histogram, ", H(d[i]),"", sep=""))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-20, 1020),
                     breaks = seq(0, 1000, 200)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1000),
                     breaks = c(0,1,10,100, 1000)) +
  annotate(geom="text", hjust=0, size=3, colour='red', 
           x=75, y=725, label="a", face = "bold") +
  annotate(geom="text", hjust=0, size=2, colour='red', 
           x=575, y=750, label="- Mediated (Biology)") +
  annotate(geom="text", hjust=0, size=2, colour='blue', 
           x=575, y=700, label="- Direct (Biology)") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))

# FigS2: b. computing
df1 = filter(df, df$dept == "CS")
hg1 = ggplot(data=df1) +
  geom_histogram(aes(x=df1$KMediated),
                 breaks=seq(0,1000,20),
                 col="red", fill="red", alpha=0.3) +
  geom_histogram(aes(x=df1$KDirect),
                 breaks=seq(0,1000,2),
                 col="blue", fill="blue", alpha=0.3) +
  xlab(expression(paste("", d[i],", link degree", sep=""))) + 
  ylab(expression(paste("Count Histogram, ", H(d[i]),"", sep=""))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-20, 1020),
                     breaks = seq(0, 1000, 200)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 500),
                     breaks = c(0,1,5,10,50,100,500)) +
  annotate(geom="text", hjust=0, size=3, colour='red', 
           x=75, y=450, label="b", face = "bold") +
  annotate(geom="text", hjust=0, size=2, colour='red', 
           x=575, y=470, label="- Mediated (Computing)") +
  annotate(geom="text", hjust=0, size=2, colour='blue', 
           x=575, y=425, label="- Direct (Computing)") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))

# Show Histogram
library(gridExtra, warn.conflicts = FALSE)
grid.arrange(hg0, hg1, ncol=2, 
             bottom="Fig. S2. F network distributions for direct and mediated associations. 
             for a. biology and b. computing.")