# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)

# Read Faculty_GoogleScholar_Funding_Data_N4190.csv
df = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
df = df %>% select(dept, KTotal, KDirect, KMediated, XDIndicator)

# FigS2: a. biology
df0 = filter(df, df$dept == "BIO")
hg0 = ggplot(data=df0) +
  geom_histogram(aes(x=df0$KDirect,y=..count..+1,
                     fill="b", colour="b"),
                 breaks=seq(0,1000,2), alpha=0.3) +
  geom_histogram(aes(x=df0$KMediated,y=..count..+1,
                     fill="r", colour="r"),
                 breaks=seq(0,1000,20), alpha=0.5) +
  geom_vline(aes(xintercept=mean(df0$KMediated)), show.legend=F,
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(df0$KDirect)), show.legend=F,
             color="blue", linetype="dashed", size=1) +
  xlab(expression(paste(d[i],", link degree", sep=""))) + 
  ylab(expression(paste("Count Histogram, ", H(d[i]),"", sep=""))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-20, 1020),
                     breaks = seq(0, 1000, 200)) +
  scale_y_log10(expand = c(0, 0),
                limits = c(1, 1100),
                breaks = c(1, 10, 100, 1000)) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position=c(0.99,0.88), legend.justification='right', legend.direction='vertical') +
  scale_colour_manual(name="", values=c("r" = "red", "b"="blue"), 
                      labels=c("b"="Direct (Biology)", "r"="Mediated (Biology)")) +
  scale_fill_manual(name="", values=c("r" = "red", "b"="blue"), 
                    labels=c("b"="Direct (Biology)", "r"="Mediated (Biology)")) +
  annotate("text", x=50, y=400, label ='atop(bold("a"))', parse=T, size=10)

# FigS2: b. computing
df1 = filter(df, df$dept == "CS")
hg1 = ggplot(data=df1) +
  geom_histogram(aes(x=df1$KDirect,y=..count..+1,
                     fill="b", colour="b"),
                 breaks=seq(0,1000,2), alpha=0.3) +
  geom_histogram(aes(x=df1$KMediated,y=..count..+1,
                     fill="r", colour="r"),
                 breaks=seq(0,1000,20), alpha=0.5) +
  geom_vline(aes(xintercept=mean(df1$KMediated)), show.legend=F,
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(df1$KDirect)), show.legend=F,
             color="blue", linetype="dashed", size=1) +
  xlab(expression(paste("", d[i],", link degree", sep=""))) + 
  ylab(expression(paste("Count Histogram, ", H(d[i]),"", sep=""))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-20, 1020),
                     breaks = seq(0, 1000, 200)) +
  scale_y_log10(expand = c(0, 0), 
                limits = c(1, 600),
                breaks = c(1, 5, 10, 50, 100, 500)) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(hjust = 0.5),
        legend.position=c(0.99,0.88), legend.justification='right', legend.direction='vertical') +
  scale_colour_manual(name="", values=c("r" = "red", "b"="blue"), 
                      labels=c("b"="Direct (Computing)", "r"="Mediated (Computing)")) +
  scale_fill_manual(name="", values=c("r" = "red", "b"="blue"), 
                    labels=c("b"="Direct (Computing)", "r"="Mediated (Computing)")) +
  annotate("text", x=50, y=200, label ='atop(bold("b"))', parse=T, size=10)

# Show Histogram
library(gridExtra, warn.conflicts=F)
#grid.arrange(hg0, hg1, ncol=2, 
#bottom="Fig. S2. F network distributions for direct and mediated associations. 
#for a. biology and b. computing.")
grid.arrange(hg0, hg1, ncol=2)