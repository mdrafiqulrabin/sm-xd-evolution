# Set working Mediatedory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)

# Empty DataFrame
df <- data.frame(matrix(vector(),ncol=3))
colnames(df) <-c("Faculty","XDIndicator","NumOfPolinator")

# Read from CSV
years = seq(1980, 2014, by=2)
fract = c()

for (y in years) {
  fn1 = paste0("CSV/Fig2B_Mediated/",(y-1),"_Mediated.csv")
  fn2 = paste0("CSV/Fig2B_Mediated/",(y),"_Mediated.csv")
  if (!file.exists(fn1) || !file.exists(fn2)) {
    print ("File doesn't exist!")
    return(1)
  }
  
  df1 = as.data.frame(read.csv(fn1))
  df2 = as.data.frame(read.csv(fn2))
  df  = rbind(df1, df2)
  
  lp = aggregate(df$NumOfPolinator, by=list(df$XDIndicator), FUN=sum)
  lp_bi = lp$x[1]; lp_cs = lp$x[2]; lp_xd = lp$x[3]
  lp_fx = lp_xd / (0.1 + lp_bi + lp_cs + lp_xd)

  fract = c(fract, lp_fx)
}

# Draw the Mediated XD links
df <- data.frame(years, fract)
ggplot(data=df, aes(x=years, y=fract)) + 
  geom_line(color = "red") + 
  geom_rect(data=df, 
            aes(xmin=1990, xmax=2003, ymin=0.0, ymax=0.5), 
            fill="red",
            alpha=0.01) +
  ggtitle("GHP (1990-2003)") +
  xlab("") + 
  ylab("f.,XD(t)\nFraction of collaborations\nthat are cross-disciplinary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1980, 2016),
                     breaks = seq(1980, 2010, 10)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.5),
                     breaks = seq(0.0, 0.5, 0.1)) 
