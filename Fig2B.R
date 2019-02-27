# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)

years = seq(1980, 2014, by=2)

getMediatedXdLink <- function() {
  # Empty DataFrame
  df <- data.frame(matrix(vector(), ncol=3))
  colnames(df) <-c("Faculty","XDIndicator","NumOfPolinator")
  
  # Read from CSV
  fract = c()
  for (y in years) {
    fn1 = paste0("CSV/Fig2B_Mediated/",(y-1),"_Mediated.csv")
    fn2 = paste0("CSV/Fig2B_Mediated/",(y),"_Mediated.csv")
    if (!file.exists(fn1) || !file.exists(fn2)) {
      print ("File doesn't exist!")
      return(1)
    }
    
    # Create DataFrame
    df1 = as.data.frame(read.csv(fn1)) # year-1
    df2 = as.data.frame(read.csv(fn2)) # year-2
    df  = rbind(df1, df2) # Combine 2 years
    #df  = df[!duplicated(df), ] # Remove duplicate
    
    # Generate DirectXdLink
    lp = aggregate(df$NumOfPolinator, by=list(df$XDIndicator), FUN=sum)
    lp_bi = lp$x[1]; lp_cs = lp$x[2]; lp_xd = lp$x[3]
    
    # Store the fraction
    lp_fx = lp_xd / (0.01 + lp_bi + lp_cs + lp_xd)
    fract = c(fract, lp_fx)
  }
  return(fract)
}

getDirectXdLink <- function() {
  # Empty DataFrame
  df <- data.frame(matrix(vector(),ncol=3))
  colnames(df) <-c("FacultyI","FacultyJ","XDIndicator")
  
  # Read from CSV
  fract = c()
  for (y in years) {
    fn1 = paste0("CSV/Fig2B_Direct/",(y-1),"_Direct.csv")
    fn2 = paste0("CSV/Fig2B_Direct/",(y),"_Direct.csv")
    if (!file.exists(fn1) || !file.exists(fn2)) {
      print ("File doesn't exist!")
      return(1)
    }
    
    # Create DataFrame
    df1 = as.data.frame(read.csv(fn1)) # year-1
    df2 = as.data.frame(read.csv(fn2)) # year-2
    df  = rbind(df1, df2) # Combine 2 years
    #df  = df[!duplicated(df), ] # Remove duplicate
    
    # Generate DirectXdLink
    lf_bi = length(which(df$XDIndicator == 0))
    lf_cs = length(which(df$XDIndicator == 1))
    lf_xd = length(which(df$XDIndicator == 2))
    
    # Store the fraction
    lf_fx = (lf_xd / (0.01 + lf_bi + lf_cs + lf_xd))
    fract = c(fract, lf_fx)
  }
  return(fract)
}

# Draw the XD links
directXdLink   = getDirectXdLink()
mediatedXdLink = getMediatedXdLink()

ggplot() + 
  geom_line(aes(x=years,y=directXdLink),color='blue') + 
  geom_line(aes(x=years,y=mediatedXdLink),color='red') + 
  geom_rect(data=df, 
            aes(xmin=1990, xmax=2003, ymin=0.0, ymax=0.3), 
            fill="red",
            alpha=0.01) +
  ggtitle("") +
  xlab("") + 
  ylab("f.,XD(t)\nFraction of collaborations\nthat are cross-disciplinary") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1980, 2016),
                     breaks = seq(1980, 2010, 10)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.3),
                     breaks = seq(0.0, 0.2, 0.1)) +
  annotate(geom="text", hjust=0, size=2, colour='red', 
           x=1981, y=0.24, label="- Mediated XD links - Career data") +
  annotate(geom="text", hjust=0, size=2, colour='blue',
           x=1981, y=0.26,label="- Direct XD links - Career data") +
  annotate(geom="text", hjust=0, size=3, colour='black',
           x=1992, y=0.28, label="HGP (1990-2003)")


