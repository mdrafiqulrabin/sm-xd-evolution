# Set working directory
setwd("~/Workspace/RStudio/")

# Read fgsfd csv file
library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
#head(fgsfd); nrow(fgsfd)
library(dplyr)
df = data.frame(fgsfd %>% select(XDIndicator, KTotal))

# Fig3-B: Probability distribution of the year of first publication
library(ggplot2)
ggplot(df, aes(x = KTotal, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5) +
  xlab(expression("Total collaboration degree, "~K[i])) + 
  ylab(expression("PDF("~K[i]~")")) +   
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 1900), 
                     breaks = seq(0, 1900, 500)) +
  scale_y_continuous(expand = c(0, 0)) +   
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))
