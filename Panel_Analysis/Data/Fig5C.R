# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Panel_Analysis/Data/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Read CSV
df_gs = read.csv("../../Paper_Data/Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
df_gs = filter(df_gs, df_gs$PRCentrality > 0) #3900 connected scholars
gsids = as.vector(df_gs$google_id)
length(gsids) # F(all,PR>0) = 3900
df_pa = read.csv("Panel_Analysis_Data.csv", stringsAsFactors=F)
df_pa = df_pa[df_pa$year >= 1970,]
df_pa = df_pa[df_pa$year <= 2015,]
df_pa = filter(df_pa, df_pa$PRCentrality > 0) 
length(unique(df_pa$google_id)) # F(all,PR>0) = 3900
df_pa['ap'] = log(df_pa['ap']) # Log Transformation

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
fn = paste0("beta_i_all_r1.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(beta_i_all, file = fn, row.names=F)

# 95% CI
df = read.csv("beta_i_all_r2.csv")
confidence_interval <- function(vector0, level0, flag0) {
  if (flag0) {
    vector0 = vector0[which(vector0 > 0)]
  } else {
    vector0 = vector0[which(vector0 < 0)]
  }
  n0 = length(vector0)
  sd0 = sd(vector0)
  mu0 = mean(vector0)
  er0 = qt((level0+1)/2, df=n0-1) * sd0 / sqrt(n0)
  if (flag0) {
    return(mu0 + er0)
  } else {
    return(mu0 - er0)
  }
}

#c(mean(ci95_l), mean(ci95_h))
c(confidence_interval(df$x, 0.95, FALSE), confidence_interval(df$x, 0.95, TRUE))
#[1] -0.01712336  0.03477999

print("Done")
