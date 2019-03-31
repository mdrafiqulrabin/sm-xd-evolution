# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read CV_Network_Dummy_Data_N4190.csv
# [t_pubs_citations, 
# SchoolRank, h_index, t_deflated_nsf, num_nsf, t_deflated_nih, num_nih, 
# PRCentrality, BetCentrality, KTotal, Chi,
# XDIndicator, Y05yr]
df = read.csv("Data/TableS2S3/CV_Network_Dummy_Data_N4190.csv")

# Best practices for missing values 
df = df[complete.cases(df),] 

# Natural logarithmic transformation
df[,1:3] = log(df[1:3]) #min>0 [t_pubs_citations, SchoolRank, h_index]
df[,4:7] = log1p(df[4:7]) #min=0 [t_deflated_nsf, num_nsf, t_deflated_nih, num_nih]

# Model (a) with PageRank centrality
df_a <- filter(df, df$PRCentrality > 0)
df_a['PRCentrality'] = log(df_a['PRCentrality'])
df_a['Chi'] = log(df_a['Chi'] + 0.63)

model_a = lm (t_pubs_citations ~ 
                SchoolRank + h_index + t_deflated_nsf + num_nsf + t_deflated_nih + num_nih +
                PRCentrality + Chi +
                factor(XDIndicator) + factor(Y05yr), data = df_a)
summary.lm(model_a)
nrow(df_a)


