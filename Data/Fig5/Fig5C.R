# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig5/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Read CSV
df_gs = read.csv("../Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
gsids = as.vector(df_gs$google_id)
df_pa = read.csv("../Panel_Analysis/Panel_Analysis_Data.csv", stringsAsFactors=F)
length(unique(df_pa$google_id)) # F(all) = 4190

ci95_l = c(); ci95_h = c()
f_get_beta_i_all <- function(x) {
  gs1k = sample(gsids, 1000)
  df_5c = df_pa[df_pa$google_id %in% gs1k,]
  mod_5c = lm(zp ~ ap + tp + iXDp + factor(year), data=df_5c)
  ci95 = confint(mod_5c, "iXDp", level=0.95)
  ci95_l <- c(ci95_l, ci95[1])
  ci95_h <- c(ci95_l, ci95[2])
  return((mod_5c$coefficients)[4][[1]])
}

# beta_i_all
beta_i_all = replicate(n = 1000, expr = f_get_beta_i_all(x))
fn = paste0("beta_i_all.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(beta_i_all, file = fn, row.names=F)

# 95% CI
n = 1000
c(mean(ci95_l), mean(ci95_h))
#[1] -0.03936276 0.02946249

print("Done")
