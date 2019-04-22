# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Panel_Analysis/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
df = df[df$year >= 1970,]
df = df[df$year <= 2017,]
df = filter(df, df$PR > 0)

df['ap'] = log(df['ap'])
df['PR'] = log(df['PR'])

mod3 = lm(zp ~ ap + tp + iXDp + factor(year), data=df)
summary(mod3)

