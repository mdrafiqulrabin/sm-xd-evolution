# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Read CSV file
csv_sid = read.csv("Fig2A/Serial_Id.csv")
csv_sd  = read.csv("Fig2A/Source_Target.csv")
n = nrow(csv_sid)

# Main
years = seq(1990, 2015, by=5)
ly = 1970 # min(csv$year)

# Create DataFrame
df <- data.frame(matrix(vector(), ncol=3))
colnames(df) <-c("serial_id", "google_id", "XDIndicator")

# Store to DataFrame
for (y in years) {
  df <- df[c(), ] # Clear DataFrame
  
  for (i in (1):(n)) {
    r_sid = csv_sid[i,] # i-th row of csv_sid
    xd = r_sid$dept # Initial dept
    r_sd  = filter(csv_sd, csv_sd$year%in%c(ly:y),
                   csv_sd$Source==r_sid$serial_id | csv_sd$Target==r_sid$serial_id)
    r_sd  = r_sd[!duplicated(r_sd), ] # Remove duplicate
    if (length(which(r_sd$s_dept != r_sd$d_dept)) != 0) xd = "XD"
    df <- rbind(df, data.frame(serial_id=r_sid$serial_id, 
                               google_id=r_sid$google_id, XDIndicator=xd))
  }
  
  # Save to CSV
  fn = paste0("Fig2A/",y,"_XD.csv")
  if (file.exists(fn)) file.remove(fn)
  write.csv(df, file = fn, row.names = FALSE)
  print(paste0("Done: setXD() : ",y))
}

if(FALSE){
xd = r_sid$XDIndicator # Initial dept
r_sd  = filter(csv_sd, csv_sd$year%in%c(ly:y),
               csv_sd$Source==r_sid$serial_id | csv_sd$Target==r_sid$serial_id)
r_sd  = r_sd[!duplicated(r_sd), ] # Remove duplicate
if (length(which(r_sd$s_xd=="XD" || r_sd$d_xd=="XD")) != 0) xd = "XD"
}
