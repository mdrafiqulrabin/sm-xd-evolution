# Set working directory
setwd("~/Workspace/RStudio/")

# Read fgsfd csv file
library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
#head(fgsfd); nrow(fgsfd)

# Probability Density Function (PDF)
library(dplyr)
df = data.frame(fgsfd %>% select(google_id, min_year, XDIndicator))
library(ggplot2)
ggplot(df, aes(x = min_year, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5) +
  xlab("Year of first publication, yi") + ylab("PDF(yi0)") +   
  scale_x_continuous(expand = c(0, 0),
                     limits = c(min(df$min_year)-lr, max(df$min_year)+lr), 
                     breaks = seq(1960, 2010, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.00, 0.04)) +   
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))
