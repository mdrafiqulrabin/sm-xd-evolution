# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Panel_Analysis/Data/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data
df_ps = read.csv("../../Paper_Data/GoogleScholar_paper_stats.csv") # paper stats
df_gs = read.csv("../../Paper_Data/Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars
df_lm = read.csv("../../Paper_Data/Faculty_Googlescholar_lambda_mu_N4190.csv") # lambda mu
df_bs = read.csv("../../Paper_Data/Biology_citations_stats_CitationNormalizationData.csv")
df_cs = read.csv("../../Paper_Data/ComputerScience_citations_stats_CitationNormalizationData.csv")
df = df_ps # new df for panel model

# Methods
f_remove_pollinators <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ",")) # unlist
  coauth = coauth [! coauth %in% c(0:2)] # Remove pollinators
  coauth = paste(as.character(coauth), collapse=",") # relist
  return(as.character(coauth))
}

f_get_xd <- function (gsid) {
  xd = (df_gs %>% filter(google_id==gsid))$XDIndicator
  return(as.character(xd))
}

f_get_dept <- function (gsid) {
  if (gsid %in% c(0,1,2)) { #pollinators
    return(as.integer(gsid))
  } else { #faculty
    dept0 = (df_gs %>% filter(google_id==gsid))$dept
    if (dept0 == "BIO") {
      return(0)
    } else if (dept0 == "CS") {
      return(1)
    } else {
      return(2)
    }
  }
}

f_get_normcite <- function (gsid, pyr, ncite) {
  df_ts = ""
  if(f_get_dept(gsid) == 0) {
    df_ts = df_bs
  } else {
    df_ts = df_cs
  }
  ys = df_ts %>% filter(YEAR==pyr)
  sd0 = ys$std_ln_citations
  mu0 = ys$sum_ln_citations/ys$num_pub
  zp0 = (log(1 + ncite) - mu0)/sd0
  return(zp0)
}

f_get_totalcoauth <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ","))
  n = length(coauth)
  return(n)
}

f_get_careerage <- function (gsid, pubyr) {
  minyr = (df_gs %>% filter(google_id==gsid))$min_year
  return(as.integer(pubyr) - as.integer(minyr) + 1)
}

f_get_iXDp <- function (coauth) {
  coauth = f_remove_pollinators(coauth)
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

f_get_pagerank <- function(gsid) {
  return((df_gs %>% filter(google_id==gsid))$PRCentrality)
}

f_get_bridgelambda <- function(gsid) {
  return((df_lm %>% filter(google_id==gsid))$Lambda)
}

df = read.csv("Panel_Analysis_Data.csv") #bypass

# Main
df$XDIndicator = sapply(df$google_id, f_get_xd)
df$dept = sapply(df$google_id, f_get_dept)
df$zp = mapply(f_get_normcite, df$google_id, df$year, df$citations)
df$ap = sapply(df$coauthor_codes, f_get_totalcoauth)
df$tp = mapply(f_get_careerage, df$google_id, df$year)
df$iXDp = sapply(df$coauthor_codes, f_get_iXDp)
df$PRCentrality = sapply(df$google_id, f_get_pagerank)
df$Lambda = sapply(df$google_id, f_get_bridgelambda)

# Write data
fn = paste0("Panel_Analysis_Data.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names=F)
print("Done")
