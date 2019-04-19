# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/TableS4S5/")

# Import library
library(readr, warn.conflicts=F)

# Load data
df_ps = read.csv("../GoogleScholar_paper_stats.csv") # paper stats
df_gs = read.csv("../Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
df = df_ps[1:100,] # working df

# Methods
f_remove_pollinators <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ",")) # unlist
  coauth = coauth [! coauth %in% c(0:2)] # Remove pollinators
  coauth = paste(as.character(coauth), collapse=",") # relist
  return(as.character(coauth))
}

f_get_dept <- function (gsid) {
  dept = (df_gs %>% filter(google_id==gsid))$dept
  return(as.character(dept))
}

f_get_xdf <- function (gsid) {
  xd = (df_gs %>% filter(google_id==gsid))$XDIndicator
  return(as.character(xd))
}

f_get_xdp <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ","))
  n = length(coauth)
  if (n > 1) {
    dept1 = f_get_dept(coauth[1])
    for (i in (2):(n)) {
      dept2 = f_get_dept(coauth[i])
      if (dept1 != dept2) {
        return(1)
      }
    }
  }
  return(0)
}

# Main
df$coauthor_codes = sapply(df$coauthor_codes, f_remove_pollinators)
df$XDF = sapply(df$google_id, f_get_xdf)
df$XDP = sapply(df$coauthor_codes, f_get_xdp)

# Write data
fn = paste0("Panel_Analysis_Data.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names=F)
print("Done")

df = read.csv("Panel_Analysis_Data.csv")
df