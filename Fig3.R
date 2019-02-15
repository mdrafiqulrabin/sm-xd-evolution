# Set working directory
setwd("~/Workspace/RStudio/")
#setwd("H:/Ph.D/2nd Semester/Statistical Method in Research/Project/sm-xd-evolution")

library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")

library(dplyr)
df = data.frame(fgsfd %>% select(XDIndicator, min_year, KTotal, Chi, mean_of_IF))

# Fig3-A: Probability distribution of the year of first publication.

library(ggplot2)
fig_3_a = ggplot(df, aes(x = min_year, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5)

mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(min_year))

fig_3_a + geom_vline(data=mu, aes(xintercept=grp.mean, color=XDIndicator),
                     linetype="dashed") +
  xlab(expression("Year of first publication, "~y[i]^0)) + 
  ylab(expression("PDF("~y[i]^0~")")) +   
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(1955, max(df$min_year)+3), 
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0.00, 0.04)) +   
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))


# Fig3-B: Probability distribution of the total number of collaborators.

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

# Fig3-C: Probability distribution of the fraction collaborators who are cross-disciplinary.

library(ggplot2)
fig_3_c = ggplot(df, aes(x = Chi, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5)

mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(Chi))

fig_3_c + geom_vline(data=mu, aes(xintercept=grp.mean, color=XDIndicator),
                     linetype="dashed") +
  xlab(expression("Cross-disciplinarity, "~X[i])) + 
  ylab(expression("PDF("~X[i]~")"))     +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))
