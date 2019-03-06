# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

# Read CSV file
t_csv = read.csv("Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
t_csv = t_csv %>% select(google_id, XDIndicator)

# Write CSV file
fn = paste0("XD_2017.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(t_csv, file = fn, row.names = FALSE)

#"0Kl8sf4AAAAJ","XD"
#"0r5B_WMAAAAJ","XD"
#RJwCyF0AAAAJ