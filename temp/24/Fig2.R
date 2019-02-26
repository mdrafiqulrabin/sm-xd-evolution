# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)

# Methods
checkFX <- function(gs_id, fxi) {
  f = fgsfd %>% filter(fgsfd$google_id == gs_id)
  return(nrow(f) == 1 && f$XDIndicator == fxi)
}

# Read CSV file
fgsfd = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
fgsfd = fgsfd %>% select(google_id, dept, XDIndicator)
gsps  = read.csv("Data/GoogleScholar_paper_stats.csv")
gsps  = gsps %>% select(year, coauthor_codes)

gspst = filter(gsps, gsps$year %in% c(1979,1980))
coauthors = gspst$coauthor_codes
print(nrow(gspst))

lp_bb = 0; lp_bc = 0; lp_bx = 0
lp_cb = 0; lp_cc = 0; lp_cx = 0
lp_xb = 0; lp_xc = 0; lp_xx = 0

lp_bio = 0; lp_cs = 0; lp_xd = 0
tfp = 0; lp_un = 0; lp_all = 0

for (c in coauthors) {
  ca  = unlist(strsplit(c, ","))
  
  fca = ca [! ca %in% c(0:2)]
  pca = ca [ca %in% c(0:2)]
  
  pb = pca [pca %in% c(0)]
  pc = pca [pca %in% c(1)]
  px = pca [pca %in% c(2)]
  
  nf = length(fca)
  np = length(pca)
  
  if (nf == 0 || np == 0) next
  
  for (f in fca) {
    tfp = tfp + 1
    if (checkFX(f,"CS")) {
      lp_cb = lp_cb + length(pb)
      lp_cc = lp_cc + length(pc)
      lp_cx = lp_cx + length(px)
      
      lp_cs = lp_cs + np
    } else if (checkFX(f,"BIO")) {
      lp_bb = lp_bb + length(pb)
      lp_bc = lp_bc + length(pc)
      lp_bx = lp_bx + length(px)
      
      lp_bio = lp_bio + np
    } else if (checkFX(f,"XD")) {
      lp_xb = lp_xb + length(pb)
      lp_xc = lp_xc + length(pc)
      lp_xx = lp_xx + length(px)
      
      lp_xd = lp_xd + np
    } else {
      lp_un = lp_un + 1
    }
  }
  
}

lp_bio
lp_cs
lp_xd

lp_all = lp_bio + lp_cs + lp_xd
lp_all
lp_xd/lp_all

lp_bb
lp_bc
lp_bx
lp_ball = lp_bb + lp_bc + lp_bx
lp_ball

lp_cb
lp_cc
lp_cx
lp_call = lp_cb + lp_cc + lp_cx
lp_call

lp_xb
lp_xc
lp_xx
lp_xall = lp_xb + lp_xc + lp_xx
lp_xall

lp_allall = lp_ball + lp_call + lp_xall
lp_allall

(lp_bx + lp_cx + lp_xall)/lp_allall
  
lp_un
tfp
