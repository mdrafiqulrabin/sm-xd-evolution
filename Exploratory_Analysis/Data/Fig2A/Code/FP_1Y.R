# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig2A/Data/")

# Import library
library(dplyr, warn.conflicts=F)
library(readr, warn.conflicts=F)

# Methods
csv_id = read.csv("Id.csv")
getF <- function(gid) {
  f = csv_id %>% filter(google_id == gid)
  return(f)
}

tc=0 # Test 

# DataFrame 2
df_y2 <- data.frame(matrix(vector(), ncol=5))
colnames(df_y2) <-c("year","Source","Target","s_dept","t_dept")
df_y2 <- df_y2[c(), ] # Clear DataFrame

years = c(1970 : 2015) #"Data/GoogleScholar_paper_stats.csv"
for (y in years) {
  # DataFrame 1
  fn = paste0("MF_1Y/MF_",y,".csv")
  df_y1 = read.csv(fn)
  df_y2 <- df_y2[c(), ] # Clear DataFrame
  
  n_y1 = nrow(df_y1)
  if (n_y1<1) {
    #next
  } else {
    for (i in (1):(n_y1)) {
      cc = (df_y1[i,])$coauthor_codes
      cc = unlist(strsplit(as.character(cc), ",")) # unlist
      p = t(combn(cc, 2)) 
      for (i in (1):(nrow(p))) {
        s = getF(p[i,][1])
        t = getF(p[i,][2])
        if (s$Id > t$Id) {
          df_y2 <- rbind(df_y2, data.frame(year=(df_y1[i,])$year, Source=t$Id, Target=s$Id, 
                   s_dept=t$dept, t_dept=s$dept))
        } else {
          df_y2 <- rbind(df_y2, data.frame(year=(df_y1[i,])$year, Source=s$Id, Target=t$Id, 
                   s_dept=s$dept, t_dept=t$dept))
        }
      }
    }
  }
  n_y2 = nrow(df_y2)

  # Write CSV file
  fn = paste0("FP_1Y/FP_",y,".csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df_y2, file = fn, row.names=F)
  
  # Test 
  print(paste0("Done -> ", y, " = ", n_y1, " - ", n_y2))
  tc = tc + n_y2
}

tc #103568