# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Panel_Analysis/Data/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)

# Read CSV
#df = read.csv("beta_i_xd_r1.csv")
df = read.csv("beta_i_xd_r2.csv")

# beta_i vline
beta_i_s5 <- c(0.08043266, 0.1123566, 0.14428053) #TableS5.R
bi_vline <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(bi_vline) <- c("xval","color","type")
bi_vline <- rbind(bi_vline, data.frame(xval=beta_i_s5[1], color="blue" , type="line"))
bi_vline <- rbind(bi_vline, data.frame(xval=beta_i_s5[2], color="blue" , type="dashed"))
bi_vline <- rbind(bi_vline, data.frame(xval=beta_i_s5[3], color="blue" , type="line"))

# Histogram
hg = ggplot(data=df, aes(x=df$x, y=(..count..)/1000)) +
  geom_histogram(binwidth = 0.008, colour = 'black', alpha=0.5) +
  geom_vline(xintercept=c(0.0), linetype="dashed") +
  geom_vline(data = bi_vline, aes(xintercept = bi_vline$xval, linetype = bi_vline$type), 
             color = bi_vline$color, lwd = 1, show.legend = F) +
  scale_x_continuous(breaks = seq(-0.05,0.20,0.05)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,0.15,0.05)) +
  coord_cartesian(xlim = c(-0.11, 0.18), ylim = c(0.0, 0.18), clip = 'off') +
  xlab(expression(paste("Cross-disciplinary coefficient, ", beta[I], "", sep=""))) + 
  ylab(expression(paste("Prob. dist. ", P(beta[I]),"", sep=""))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size=0.5),
        plot.title = element_text(hjust = 0.5)) 
hg
