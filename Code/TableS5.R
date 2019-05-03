# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Panel_Analysis/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(Hmisc, warn.conflicts=F)
library(miceadds, warn.conflicts=F)
library(multiwayvcov, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
df = df[df$year >= 1970,]
df = df[df$year <= 2015,]
df = filter(df, df$PRCentrality > 0) #3900 connected scholars
df = df[df$XDIndicator=="XD",] #1247 XD faculty

nrow(df) #n=166621
sum(df$iXDp==1) #3915

# Log Transformation
df['ap'] = log(df['ap'])
df['PRCentrality'] = log(df['PRCentrality'])
df['Lambda'] = log(df['Lambda'])

# Model (1) No Fixed Effects
df1 = df
#mod1 = lm(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df1)
mod1 = lm.cluster(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df1, cluster="google_id")
summary(mod1)

# Model (2) No Fixed Effects [Std]
df2 = df
df2[,8:9] <- data.frame(sapply(df2[8:9], scale), stringsAsFactors=F)
df2[,11:12] <- data.frame(sapply(df2[11:12], scale), stringsAsFactors=F)
#mod2 = lm.(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df2)
mod2 = lm.cluster(zp ~ ap + tp + iXDp + PRCentrality + Lambda + dept + factor(year), data=df2, cluster="google_id")
summary(mod2)

# Model (3) Fixed Effects
df3 = df
#mod3 = lm(zp ~ ap + tp + iXDp + factor(year), data=df3)
#summary(mod3)
mod3 = plm(zp ~ ap + tp + iXDp + factor(year), data=df3, index="google_id", model="within")
coeftest(mod3, vcov = vcovHC, type = "HC0")  #iXDp=0.1123566
within_intercept(mod3, vcov = vcovHC, type = "HC0")
# 95% CI
ci95 = confint(mod3, "iXDp", level=0.95)
c (ci95[1], ci95[2])
#[1] 0.08043266 0.14428053

# Model (4) Fixed Effects [Std]
df4 = df
df4[,8:9] <- data.frame(sapply(df4[8:9], scale), stringsAsFactors=F)
#mod4 = lm(zp ~ ap + tp + iXDp + factor(year), data=df4)
#summary(mod4)
mod4 = plm(zp ~ ap + tp + iXDp + factor(year), data=df4, index="google_id", model="within")
coeftest(mod4, vcov = vcovHC, type = "HC0")
within_intercept(mod4, vcov = vcovHC, type = "HC0")
