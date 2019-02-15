# Set working directory
setwd("~/Workspace/RStudio/")

# Read fgsfd csv file
library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
#head(fgsfd); nrow(fgsfd)
library(dplyr)
df = data.frame(fgsfd %>% select(XDIndicator, min_year, KTotal, Chi, mean_of_IF))

# Fig3-A: Probability distribution of the year of first publication
library(ggplot2)
ggplot(df, aes(x = min_year, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5) +
  xlab(expression(paste("Year of first publication, ",y[i]^0,sep=''))) + 
  ylab(expression(paste("PDF(",y[i]^0,")",sep=''))) +   
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(1955, max(df$min_year)+3), 
                     breaks = seq(1960, 2010, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.00, 0.04)) +   
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))

