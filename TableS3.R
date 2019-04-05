# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Select CV, Network and Dummy parameters
df = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
df = df %>% select(t_pubs_citations, 
                   SchoolRank, h_index, t_deflated_nsf, num_nsf, t_deflated_nih, num_nih, 
                   PRCentrality, BetCentrality, KDirect, Chi,
                   XDIndicator, Y05yr)

# Best practices for missing values 
df = df[complete.cases(df),] 

# Natural logarithmic transformation
df[,1:3] = log(df[1:3]) #min>0 [t_pubs_citations, SchoolRank, h_index]
df[,4:7] = log1p(df[4:7]) #min=0 [t_deflated_nsf, num_nsf, t_deflated_nih, num_nih]

# Model (a) with PageRank centrality
df_a <- filter(df, df$PRCentrality > 0)
df_a['PRCentrality'] = log(df_a['PRCentrality'])
model_a = lm (t_pubs_citations ~ 
                SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                PRCentrality + Chi +
                factor(XDIndicator) + factor(Y05yr), data = df_a)
summary.lm(model_a)
nrow(df_a)

# Model (b) with Betweenness centrality
df_b <- filter(df, df$BetCentrality > 0)
df_b['BetCentrality'] = log(df_b['BetCentrality'])
model_b = lm (t_pubs_citations ~ 
                SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                BetCentrality + Chi +
                factor(XDIndicator) + factor(Y05yr), data = df_b)
summary.lm(model_b)
nrow(df_b)

# Model (c) with Degree centrality
df_c <- filter(df, df$KDirect > 0)
df_c['KDirect'] = log(df_c['KDirect'])
model_c = lm (t_pubs_citations ~ 
                SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                KDirect + Chi +
                factor(XDIndicator) + factor(Y05yr), data = df_c)
summary.lm(model_c)
nrow(df_c)

# Model (d) without the number of grants variables
df_d <- df_a
model_d = lm (t_pubs_citations ~ 
                SchoolRank + h_index + t_deflated_nsf + t_deflated_nih +
                PRCentrality + Chi +
                factor(XDIndicator) + factor(Y05yr), data = df_d)
summary.lm(model_d)
nrow(df_d)

# Model (e) without the departmental rank variable
df_e <- df_a
model_e = lm (t_pubs_citations ~ 
                h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                PRCentrality + Chi +
                factor(XDIndicator) + factor(Y05yr), data = df_e)
summary.lm(model_e)
nrow(df_e)

# Residual plots against the independent variable
par(mfrow=c(3, 2))
plot(model_a, which = 1, main = "Model (a)")
plot(model_b, which = 1, main = "Model (b)")
plot(model_c, which = 1, main = "Model (c)")
plot(model_d, which = 1, main = "Model (d)")
plot(model_e, which = 1, main = "Model (e)")

# Check the multicollinearity of model
car::vif(model_a)
car::vif(model_b)
car::vif(model_c)
car::vif(model_d)
car::vif(model_e)
