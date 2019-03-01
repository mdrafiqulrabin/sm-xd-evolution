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
  geom_histogram(aes(x=df0$KMediated,y=..count..+1),
                 breaks=seq(0,1000,20),
                 col="red", fill="red", alpha=0.3) +
  geom_histogram(aes(x=df0$KDirect,y=..count..+1),
                 breaks=seq(0,1000,2),
                 col="blue", fill="blue", alpha=0.3) +
  xlab(expression(paste(d[i],", link degree", sep=""))) + 
  ylab(expression(paste("Count Histogram, ", H(d[i]),"", sep=""))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-20, 1020),
                     breaks = seq(0, 1000, 200)) +
  scale_y_log10(expand = c(0, 0)) +
  annotate(geom="text", hjust=0, size=3, colour='red', 
           x=75, y=275, label="a") +
  annotate(geom="text", hjust=0, size=2, colour='red', 
           x=500, y=375, label="- Mediated (Biology)") +
  annotate(geom="text", hjust=0, size=2, colour='blue', 
           x=500, y=275, label="- Direct (Biology)") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))

# FigS2: b. computing
df1 = filter(df, df$dept == "CS")
hg1 = ggplot(data=df1) +
  geom_histogram(aes(x=df1$KMediated,y=..count..+1),
                 breaks=seq(0,1000,20),
                 col="red", fill="red", alpha=0.3) +
  geom_histogram(aes(x=df1$KDirect,y=..count..+1),
                 breaks=seq(0,1000,2),
                 col="blue", fill="blue", alpha=0.3) +
  xlab(expression(paste("", d[i],", link degree", sep=""))) + 
  ylab(expression(paste("Count Histogram, ", H(d[i]),"", sep=""))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-20, 1020),
                     breaks = seq(0, 1000, 200)) +
  scale_y_log10(expand = c(0, 0)) +
  annotate(geom="text", hjust=0, size=3, colour='red', 
           x=75, y=175, label="b") +
  annotate(geom="text", hjust=0, size=2, colour='red', 
           x=500, y=175, label="- Mediated (Computing)") +
  annotate(geom="text", hjust=0, size=2, colour='blue', 
           x=500, y=125, label="- Direct (Computing)") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold"))

# Show Histogram
library(gridExtra, warn.conflicts = FALSE)
grid.arrange(hg0, hg1, ncol=2, 
             bottom="Fig. S2. F network distributions for direct and mediated associations. 
             for a. biology and b. computing.")