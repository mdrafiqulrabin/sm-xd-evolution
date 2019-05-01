# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig5/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Read CSV
df_gs = read.csv("../Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
gsids = as.vector(df_gs$google_id)
df_pa = read.csv("../Panel_Analysis/Panel_Analysis_Data.csv", stringsAsFactors=F)
df_pa = df_pa[df_pa$XDIndicator=="XD",] # Only XD faculty
length(unique(df_pa$google_id)) # F(xd) = 1247

f_get_beta_i_xd <- function(x) {
  gs1k = sample(gsids, 1000)
  df_5d = df_pa[df_pa$google_id %in% gs1k,]
  mod_5d = lm(zp ~ ap + tp + iXDp + factor(year), data=df_5d)
  return((mod_5d$coefficients)[4][[1]])
}

beta_i_xd = replicate(n = 1000, expr = f_get_beta_i_xd(x))
fn = paste0("beta_i_xd.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(beta_i_xd, file = fn, row.names=F)

print("Done")
