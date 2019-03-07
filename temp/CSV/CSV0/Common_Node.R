# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/CSV/")

# Import library
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)

# Read CSV
df1 = read.csv("Fig2A/1990_Node.csv")
df2 = read.csv("Fig2A/1995_Node.csv")
df3 = read.csv("Fig2A/2000_Node.csv")
df4 = read.csv("Fig2A/2005_Node.csv")
df5 = read.csv("Fig2A/2010_Node.csv")
df6 = read.csv("Fig2A/2015_Node.csv")

# Common DataFrame
df <- df1
df <- cbind(df, XD95=df2$XDIndicator)
df <- cbind(df, XD00=df3$XDIndicator)
df <- cbind(df, XD05=df4$XDIndicator)
df <- cbind(df, XD10=df5$XDIndicator)
df <- cbind(df, XD15=df6$XDIndicator)

# Write CSV
fn = paste0("Fig2A/Common_Node.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(df, file = fn, row.names = FALSE)
print(paste0("Done: Common_Node()"))