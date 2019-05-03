# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Panel_Analysis/Data/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
df = df[df$year >= 1970,]
df = df[df$year <= 2017,]
df = filter(df, df$PRCentrality > 0) #3900 connected scholars
df = df[df$XDIndicator=="XD",] #1247 XD faculty

nrow(df)   

#================== Filter with Year and ixdp ===================

all_gs = unique(df$google_id) 
new_df = df[df$google_id=="ds", ]
count =0
for (gs in all_gs) 
  {
  pubs = df[df$google_id == gs,]
  pubs <- pubs[order(pubs$year),] 
  nrow(pubs)

  all_years = unique(pubs$year)
  year1 = pubs[1, 2]
  for(year in all_years)
    {
      pubs_year = pubs[((pubs$year == year) | (pubs$year == (year+1))),]
      
      nrow(pubs_year)

      tot_ixdP = nrow(pubs_year)
      tot_1 = sum(pubs_year$iXDp)
      
      if((tot_1<tot_ixdP) && (tot_1>0))
        {
        if(year == year1)
          {
          pubs_year = pubs_year[pubs_year$year == year+1,]
          }
        new_df = rbind(new_df, pubs_year)
        print(nrow(new_df))
        }
      year1 = year + 1
    }
  print(count)
  count = count + 1
}

df_ixdp = new_df
fn = paste0("tables6_year_ixdp.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(new_df, file = fn, row.names=F)


#===================== Filter with year and ap ===================


df_ixdp = read.csv("tables6_year_ixdp.csv", header = T)
nrow(df_ixdp)

all_gs = unique(df_ixdp$google_id) 
matched = df_ixdp[df_ixdp$google_id=="ds", ]
count =0
for (gs in all_gs) 
{
  pubs = df_ixdp[df_ixdp$google_id == gs,]
  pubs <- pubs[order(pubs$year),] 
  nrow(pubs)

  all_years = unique(pubs$year)
  year1 = pubs[1, 2]
  for(year in all_years)
  {
    pubs_year = pubs[((pubs$year == year) | (pubs$year == (year+1))),]
    
    nrow(pubs_year)

    num_row = nrow(pubs_year)
    for (i in 1:(num_row - 1))
    {
      row1 = pubs_year[i,]
      j = i + 1
      while(j < num_row)
      {
        row2 = pubs_year[j, ]
        prcnt = ((abs(row1[8] - row2[8])) / ((row1[8] + row2[8])/2)) * 100

        if(prcnt <= 20)
        {
          matched = rbind(matched, row1)
          matched = rbind(matched, row2)
        }
        j = j+ 1
      }
    }
  }
  print(count)
  count = count + 1
}

nrow(matched)
matched = matched[!duplicated(matched), ]

nrow(matched)
View(matched)

fn = paste0("tables6_year_ixdp_percent.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(matched, file = fn, row.names=F)


#=================== Filter by at least 10 match ================

matched_final = read.csv("tables6_year_ixdp_percent.csv", header = T)
nrow(matched_final)

all_gs = unique(matched_final$google_id) 
count =0
for (gs in all_gs) 
{
  pubs = matched_final[matched_final$google_id == gs,]
  if( nrow(pubs) < 10)
  {
    matched_final = matched_final[(matched_final$google_id != gs),]
  }
}

nrow(matched_final)
all_gs = unique(matched_final$google_id) 
count =0
for (gs in all_gs) {
  count = count + 1
}
print(count)  

fn = paste0("tableS6.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(matched_final, file = fn, row.names=F)

#===================================

df = read.csv("tableS6.csv", header = T)
nrow(df)

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
