# Set working directory
setwd("H:/Ph.D/Spring'19/Statistical-Method-in-Research/Project/sm-xd-evolution/Data/Panel_Analysis/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
View(df)
df = df[df$year >= 1970,]
df = df[df$year <= 2017,]
df = filter(df, df$PRCentrality > 0) #3900 connected scholars
df = df[df$XDIndicator=="XD",] #1247 XD faculty

tot= 0
all_gs = unique(df$google_id) 
for (gs in all_gs) {
  pubs = df[df$google_id == gs,]
  pubs <- pubs[order(pubs$year),] 

  count = 0
  for(i in (1:nrow(pubs)-1)){
    diff = pubs[i+1, 2] - pubs[i, 2] 
    xor = pubs[i+1, 10] + pubs[i, 10]
    prcnt = ((abs(pubs[i+1, 8] - pubs[i, 8])) / (pubs[i+1, 8] + pubs[i, 8])/2) * 100
    
    if(length(diff)>0 && diff < 2 && length(xor)>0 && xor==1 && length(prcnt)>0 && prcnt <=20)
#    if(length(diff)>0 && diff < 2 && pubs[i, 10]==0 && pubs[i+1, 10]==1 && length(prcnt)>0 && prcnt <=20)
    {
      count = count +1
    }
  }

  if(count>=10){
    print(pubs[1,1])  
    tot = tot + 1
  }
  else {
    df = df[df$google_id!=gs,]
  }
}

print(tot)
nrow(df) 
all_gs = unique(df$google_id)
View(df)

# Log Transformation
df['ap'] = log(df['ap'])
df['PRCentrality'] = log(df['PRCentrality'])
df['Lambda'] = log(df['Lambda'])

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
# 95% CI
confint(mod3, "iXDp", level=0.95)

# Model (4) Fixed Effects [Std]
df4 = df
df4[,8:9] <- data.frame(sapply(df4[8:9], scale), stringsAsFactors=F)
mod4 = lm(zp ~ ap + tp + iXDp + factor(year), data=df4)
summary(mod4)
