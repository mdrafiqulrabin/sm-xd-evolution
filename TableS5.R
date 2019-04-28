# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Panel_Analysis/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
df = df[df$year >= 1970,]
df = df[df$year <= 2017,]
df = filter(df, df$PRCentrality > 0) #3900 connected scholars
df = df[df$XDIndicator=="XD",] #1247 XD faculty

# Log Transformation
df['ap'] = log(df['ap'])
df['PRCentrality'] = log(df['PRCentrality'])
df['Lambda'] = log(df['Lambda'])

nrow(df) #n=166621

# Model (1) No Fixed Effects
df1 = df
mod1 = lm(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df1)
summary(mod1)

# Model (2) No Fixed Effects [Std]
df2 = df
df2[,8:9] <- data.frame(sapply(df2[8:9], scale), stringsAsFactors=F)
df2[,11:12] <- data.frame(sapply(df2[11:12], scale), stringsAsFactors=F)
mod2 = lm(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df2)
summary(mod2)

# Model (3) Fixed Effects
df3 = df
mod3 = lm(zp ~ ap + tp + iXDp + factor(year), data=df3)
summary(mod3)

# Model (4) Fixed Effects [Std]
df4 = df
df2[,8:9] <- data.frame(sapply(df2[8:9], scale), stringsAsFactors=F)
mod4 = lm(zp ~ ap + tp + iXDp + factor(year), data=df4)
summary(mod4)
