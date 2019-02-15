# Set working directory
setwd("~/Workspace/RStudio/")
#setwd("H:/Ph.D/2nd Semester/Statistical Method in Research/Project/sm-xd-evolution")

library(readr)
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")

library(dplyr)
df = data.frame(fgsfd %>% select(XDIndicator, min_year, KTotal, Chi, mean_of_IF, t_pubs_citations))

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

# Fig3-E: Probability distribution of the mean impact factor of the publication record.

library(ggplot2)
fig_3_a = ggplot(df, aes(x = mean_of_IF, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5)

mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(mean_of_IF))

fig_3_a + geom_vline(data=mu, aes(xintercept=grp.mean, color=XDIndicator),
                     linetype="dashed") +
  xlab(expression("Mean publication impact factor, "~IF[i])) + 
  ylab(expression("PDF("~IF[i]~")")) +   
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 29), 
                     breaks = seq(0, 30, 5)) +
  scale_y_continuous(expand = c(0, 0)) + 
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))


# Fig3-F: Probability distribution of the Total citation log10

fig_3_f = ggplot(df, aes(x = log10(t_pubs_citations), color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.5)

mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(log10(t_pubs_citations)))

fig_3_f + geom_vline(data=mu, aes(xintercept=grp.mean, color=XDIndicator),
                     linetype="dashed") +
  xlab(expression(paste("Total career citation, ", log[10], C[i], sep="")))  +
  ylab(expression(paste("PDF(", log[10], C[i],")", sep="")))     +
  scale_x_continuous(expand = c(.09, .09), 
                     breaks = c(0, 1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(expand = c(0, 0)) + 
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))


