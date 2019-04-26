# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Panel_Analysis/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
df = df[df$year >= 1970,]
df = df[df$year <= 2017,]
df = filter(df, df$PRCentrality > 0)

# Model (1) No Fixed Effects
df1 = df
df1['ap'] = log1p(df1['ap'])
df1['PRCentrality'] = log(df1['PRCentrality'])
mod1 = lm(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df1)
summary(mod1)

# Model (3) Fixed Effects
df3 = df
mod3 = lm(zp ~ ap + tp + iXDp + factor(year), data=df3)
summary(mod3)
