# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)

# Ensure XD
xd_main = read.csv("../Data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
xd_2015 = read.csv("Fig2A/2015_XD.csv")
n = nrow(xd_2015)
t = 0

chk = TRUE
for (i in (1):(n)) {
  if (i == 112) next
  t = t + 1
  xd1 = (xd_main[i,])$XDIndicator
  xd2 = (xd_2015[i,])$XDIndicator
  if (xd1 != xd2) {
    print(paste0(i," ",xd1," ",xd2))
    print(xd_main[i,])
    print(xd_2015[i,])
    chk = FALSE
    break
  }
}

if (chk == TRUE) {
  print("Result is correct!")
} else {
  print("Result isn't correct!")
}

print(paste0("Done: ", t))
#125: 0r5B_WMAAAAJ