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
  xlab("Year of first publication, yi") + ylab("PDF(yi0)")
