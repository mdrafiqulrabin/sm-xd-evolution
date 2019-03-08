# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(scales, warn.conflicts=F)
library(readr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)

# Read Faculty_GoogleScholar_Funding_Data_N4190.csv
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
df = data.frame(fgsfd %>% select(XDIndicator, 
                          min_year, KTotal, Chi, mean_of_IF, t_pubs_citations, PRCentrality))

# Fig3-A: Probability distribution of the year of first publication.

df = filter(df, df$min_year != 0)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(min_year))
fig_3a = ggplot(df, 
                aes(x = min_year, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(data = mu, 
             aes(xintercept=grp.mean, color=XDIndicator), linetype="dashed") +
  xlab(expression("Year of first publication, "~y[i]^0)) + 
  ylab(expression("PDF("~y[i]^0~")")) +   
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(1954, 2018), 
                     breaks = seq(1960, 2010, 10)) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0.00, 0.04)) +   
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA)) +
  scale_colour_manual(values=c("green", "magenta", "grey")) +
  scale_fill_manual(values=c("green", "magenta", "grey"))
fig_3a

# Fig3-B: Probability distribution of the total number of collaborators.

df = filter(df, df$KTotal != 0)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(KTotal))
fig_3b <- ggplot(df, 
                  aes(x = KTotal, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(data = mu, 
             aes(xintercept=grp.mean, color=XDIndicator), linetype="dashed") +
  xlab(expression("Total collaboration degree, "~K[i])) + 
  ylab(expression("PDF("~K[i]~")")) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 1900),
                     breaks = seq(0, 1500, 500)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(10^-6, 10^-5, 10^-4, 10^-3),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA)) 
#fig_3b

# Fig3-C: Probability distribution of the fraction of the collaborators who are cross-disciplinary.

df = filter(df, df$Chi != 0)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(Chi))
fig_3c = ggplot(df, 
                aes(x = Chi, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.3) +
  geom_vline(data = mu, 
             aes(xintercept=grp.mean, color=XDIndicator), linetype="dashed") +
  xlab(expression("Cross-disciplinarity, "~X[i])) + 
  ylab(expression("PDF("~X[i]~")"))     +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0.0, 1.0),
                     breaks = seq(0.0, 1.0, 0.2)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(10^-2, 10^-1, 1, 10),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))
#fig_3c

# Fig3-D: Probability distribution of the PageRank centrality scaled by number of F.

nf = nrow(fgsfd) #  number of researcher
df = filter(df, df$PRCentrality != 0)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(nf*(PRCentrality)))
fig_3d = ggplot(df, 
                aes(x = nf*(PRCentrality), color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.3) +
  geom_vline(data = mu, 
             aes(xintercept=grp.mean, color=XDIndicator), linetype="dashed") +
  xlab(expression("PageRank centrality, "~N[F]~"*"~E[i]^{PR})) + 
  ylab(expression("PDF("~N[F]~"*"~E[i]^{PR}~")"))     +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 9),
                     breaks = seq(0, 8, 2)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(10^-3, 10^-2, 10^-1, 1),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))
#fig_3d

# Fig3-E: Probability distribution of the mean impact factor of the publication record.

df = filter(df, df$mean_of_IF != 0)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(mean_of_IF))
fig_3e = ggplot(df, 
                 aes(x = mean_of_IF, color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.3) + 
  geom_vline(data = mu, 
             aes(xintercept=grp.mean, color=XDIndicator), linetype="dashed") +
  xlab(expression("Mean publication impact factor, "~IF[i])) + 
  ylab(expression("PDF("~IF[i]~")")) +   
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 29), 
                     breaks = seq(0, 25, 5)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(10^-4, 10^-3, 10^-2, 10^-1),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))
#fig_3e

# Fig3-F: Probability distribution of the total citations.

df = filter(df, df$t_pubs_citations != 0)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(log10(t_pubs_citations)))
fig_3f = ggplot(df, 
                aes(x = log10(t_pubs_citations), color = XDIndicator, fill = XDIndicator)) + 
  geom_density(alpha = 0.3) +
  geom_vline(data = mu, 
             aes(xintercept=grp.mean, color=XDIndicator), linetype="dashed") +
  xlab(expression(paste("Total career citation, ", log[10], C[i], sep="")))  +
  ylab(expression(paste("PDF(", log[10], C[i],")", sep="")))     +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 6.2), 
                     breaks = seq(0, 6, 1)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(10^-4, 10^-3, 10^-2, 10^-1, 1),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA))
#fig_3f

# Show Plots
library(magrittr, warn.conflicts=F)
library(ggpubr, warn.conflicts=F)
ggarrange(fig_3a, fig_3b, 
          fig_3c, fig_3d, 
          fig_3e, fig_3f,
          ncol=2, nrow=3, 
          common.legend =T, legend="top")