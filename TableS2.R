# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)
library(sjstats, warn.conflicts=F)

# Read CV_Network_Dummy_Data_N4190.csv
# [t_pubs_citations, 
# SchoolRank, h_index, t_deflated_nsf, num_nsf, t_deflated_nih, num_nih, 
# PRCentrality, BetCentrality, KDirect, Chi,
# XDIndicator, Y05yr]
df = read.csv("Data/TableS2S3/CV_Network_Dummy_Data_N4190.csv")
df$BetCentrality = NULL
df$KDirect = NULL

# Best practices for missing values 
df = df[complete.cases(df),] 

# Natural logarithmic transformation
df[,1:3] = log(df[1:3]) #min>0 [t_pubs_citations, SchoolRank, h_index]
df[,4:7] = log1p(df[4:7]) #min=0 [t_deflated_nsf, num_nsf, t_deflated_nih, num_nih]

# Model (CV)
df_cv <- df
model_cv = lm (t_pubs_citations ~ 
                 SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih + 
                 factor(XDIndicator) + factor(Y05yr), data = df_cv)
summary.lm(model_cv)
nrow(df_cv)

# Model (CV + Network)
df_cvnet <- filter(df, df$PRCentrality > 0)
df_cvnet['PRCentrality'] = log(df_cvnet['PRCentrality'])
model_cvnet = lm (t_pubs_citations ~ 
                    SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                    PRCentrality + Chi +
                    factor(XDIndicator) + factor(Y05yr), data = df_cvnet)
summary.lm(model_cvnet)
nrow(df_cvnet)

# Model (CV + Network [Std])
df_cvnetstd <- df_cvnet
df_cvnetstd[,2:9] <- data.frame(sapply(df_cvnetstd[2:9], scale), stringsAsFactors=F)
model_cvnetstd = lm (t_pubs_citations ~ 
                       SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                       PRCentrality + Chi +
                       factor(XDIndicator) + factor(Y05yr), data = df_cvnetstd)
summary.lm(model_cvnetstd)
nrow(df_cvnetstd)

# Residual plots against the independent variable
par(mfrow=c(1, 3))
plot(model_cv, which = 1, main = "Model (CV)")
plot(model_cvnet, which = 1, main = "Model (CV + Network)")
plot(model_cvnetstd, which = 1, main = "Model (CV + Network [Standardized])")

# Check the multicollinearity of model
car::vif(model_cv)
car::vif(model_cvnet)
car::vif(model_cvnetstd)
