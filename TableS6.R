# Set working directory
setwd("H:/Ph.D/Spring'19/Statistical-Method-in-Research/Project/sm-xd-evolution/Data/Panel_Analysis/")

# Import library
library(readr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

# Load data and filter
df = read.csv("Panel_Analysis_Data.csv",stringsAsFactors=F)
df = df[df$year >= 1970,]
df = df[df$year <= 2017,]
df = filter(df, df$PRCentrality > 0) #3900 connected scholars
df = df[df$XDIndicator=="XD",] #1247 XD faculty

nrow(df)   # 166621

# 2SF6AXQAAAAJ

# Year and ixdp

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
#      year = 1971
      pubs_year = pubs[((pubs$year == year) | (pubs$year == (year+1))),]
      
      nrow(pubs_year)
#      View(pubs_year)

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
#        print(year)
        }
      year1 = year + 1
    }
#  print(pubs$google_id)
  print(count)
  count = count + 1
}

df_ixdp = new_df
fn = paste0("tables6_year_ixdp.csv")
if (file.exists(fn)) file.remove(fn)
write.csv(new_df, file = fn, row.names=F)


#===================== year and ap


# -chjN0MAAAAJ
# -IMW-w0AAAAJ
# 7BM1uyYAAAAJ

df_ixdp = read.csv("tables6_year_ixdp.csv", header = T)
nrow(df_ixdp)
View(df_ixdp)

all_gs = unique(df_ixdp$google_id) 
#View(all_gs)
matched = df_ixdp[df_ixdp$google_id=="ds", ]
count =0
for (gs in all_gs) 
{
  print(gs)
  pubs = df_ixdp[df_ixdp$google_id == gs,]
  pubs <- pubs[order(pubs$year),] 
  nrow(pubs)

  all_years = unique(pubs$year)
  year1 = pubs[1, 2]
  for(year in all_years)
  {
#    year = 2014
    pubs_year = pubs[((pubs$year == year) | (pubs$year == (year+1))),]
    
    nrow(pubs_year)

    num_row = nrow(pubs_year)
    for (i in 1:(num_row - 1))
    {
      row1 = pubs_year[i,]
      j = i + 1
      while(j <= num_row)
      {
        row2 = pubs_year[j, ]
#        print(row1)
#        print(row1[8])
#        print("-")
#        print(row2)
        prcnt = ((abs(row1[8] - row2[8])) / ((row1[8] + row2[8])/2)) * 100

        if(prcnt <= 20)
        {
          matched = rbind(matched, row1)
          matched = rbind(matched, row2)
#          print(prcnt)
        }
#        print(prcnt)
        j = j+ 1
      }
    }

  }
  #  print(pubs$google_id)
#  print(count)
  count = count + 1
}

View(matched)
matched = matched[!duplicated(matched), ]



#-----------------
tot= 0
all_gs = unique(new_df$google_id) 

new_df = df[df$google_id=="ds", ]

for (gs in all_gs) {
  pubs = df[df$google_id == gs,]
  pubs <- pubs[order(pubs$year),] 

  count = 0
  for(i in 1:(nrow(pubs)-1)){
    j=1
    while(i+j<=nrow(pubs)){
      diff = pubs[i+j, 2] - pubs[i, 2] 
      if(diff>=2){
        break
      }
      prcnt = ((abs(pubs[i+j, 8] - pubs[i, 8])) / ((pubs[i+j, 8] + pubs[i, 8])/2)) * 100
      
      if(diff < 2 && xor==1 && prcnt <=20)
      {
        count = count +1
      }
      if(count>=10){
        break
      }
      j=j+1
    }
    if(count>=10){
      break
    }
  }

  if(count>=10){
    print(pubs[1,1])  
    tot = tot + 1
    print(tot)
    new_df = rbind(new_df, pubs)
  }
  else {
    df = df[df$google_id!=gs,]
  }
}

print(tot)
nrow(df) 
nrow(new_df)
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
