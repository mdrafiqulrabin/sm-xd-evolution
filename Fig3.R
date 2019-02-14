# Set working directory
setwd("~/Workspace/RStudio/")

# Read fgsfd csv file
library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
#head(fgsfd); nrow(fgsfd)

# Probability Density Function (PDF)
draw_pdf <- function(m_xd, m_color) {
  library(dplyr)
  m_min_year  <- as.numeric(unlist(fgsfd %>% filter(XDIndicator == m_xd)  %>% select(min_year)))
  
  library(ggplot2)
  ggplot(data.frame(m_min_year), aes(x=m_min_year)) + 
    geom_density(color=m_color, fill=m_color) +
    xlab("Year of first publication, yi") + ylab("PDF(yi0)") +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(min(m_min_year)-5, max(m_min_year)+5), 
                       breaks = seq(min(m_min_year), max(m_min_year), 10)) +
    scale_y_continuous(expand = c(0, 0))
}

draw_pdf("CS", "red") # PDF of CS
