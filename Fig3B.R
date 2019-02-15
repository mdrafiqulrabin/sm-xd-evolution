setwd("H:/Ph.D/2nd Semester/Statistical Method in Research/Project/sm-xd-evolution")

library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")

#head(fgsfd, 1)
#nrow(fgsfd)

library(dplyr)
df = data.frame(fgsfd %>% select(XDIndicator, KTotal))

min(df$KTotal)

# Fig3-B: Probability distribution of the year of first publication
library(ggplot2)

fig_3_b <- ggplot(df, aes(x = KTotal, color = XDIndicator, fill = XDIndicator)) + 
  geom_density()
  
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(KTotal))

fig_3_b + geom_vline(data=mu, aes(xintercept=grp.mean, color=XDIndicator),
                     linetype="dashed") +
  xlab(expression("Total collaboration degree, "~K[i])) + 
  ylab(expression("PDF("~K[i]~")")) +
  scale_x_continuous(expand = c(0, 0),
                    breaks = c(0, 500, 1000, 1500),
                    limits = c(0, 1900)
  ) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(10^-3, 10^-4, 10^-5, 10^-6)
                     ) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.spacing.x = margin()) 
