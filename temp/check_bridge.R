df_ps = read.csv("../GoogleScholar_paper_stats.csv") # paper stats
df_gs = read.csv("../Faculty_GoogleScholar_Funding_Data_N4190.csv") # google scholars

f_check_gs <- function(gsid) {
  dept = (df_gs %>% filter(google_id==gsid))$dept
  if (length(dept) == 0) {
    print(paste0("Bridge: ",gsid))
  }
}

f_check_bridge <- function (coauth) {
  coauth = unlist(strsplit(as.character(coauth), ","))
  coauth = coauth [! coauth %in% c(0:2)] # Remove pollinators
  sapply(coauth, f_check_gs)
}

sapply(df_ps$coauthor_codes, f_check_bridge)
