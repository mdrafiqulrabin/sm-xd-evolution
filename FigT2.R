# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read Faculty_GoogleScholar_Funding_Data_N4190.csv
# [google_id name dept h_index i10index min_year max_year t_publication t_pubs_citations
# highest_citations mean_of_IF mean_of_co_authors num_nsf t_deflated_nsf num_nih t_deflated_nih Y05yr KTotal
# KDirect KMediated Chi BetCentrality PRCentrality XDIndicator SchoolRank]
df = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")

cv = df %>% select(t_pubs_citations, SchoolRank, h_index, 
                   t_deflated_nsf, num_nsf, t_deflated_nih, num_nih, 
                   XDIndicator, Y05yr)

cv[,1:3] = log(cv[1:3]) #min>0
cv[,4:7] = log1p(cv[4:7]) #min=0

model_cv = lm (t_pubs_citations ~ SchoolRank + h_index +
                 t_deflated_nsf + num_nsf + t_deflated_nih + num_nih + 
                 factor(XDIndicator) + factor(Y05yr), data=cv)
summary.lm(model_cv)
