# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/TableS3/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Read Faculty_GoogleScholar_Funding_Data_N4190.csv
# [google_id name dept h_index i10index min_year max_year t_publication t_pubs_citations
# highest_citations mean_of_IF mean_of_co_authors num_nsf t_deflated_nsf num_nih t_deflated_nih Y05yr KTotal
# KDirect KMediated Chi BetCentrality PRCentrality XDIndicator SchoolRank]
df = read.csv("../Faculty_GoogleScholar_Funding_Data_N4190.csv")

# Select CV, Network and Dummy parameters
df = df %>% select(t_pubs_citations, 
                   SchoolRank, h_index, t_deflated_nsf, num_nsf, t_deflated_nih, num_nih, 
                   PRCentrality, BetCentrality, KTotal, Chi,
                   XDIndicator, Y05yr)

# Add Serial ID number
# df <- mutate(df, ID = rownames(df))

# Save to CSV file
fn = paste0("CV_Network_Dummy_Data_N4190.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names=F)
