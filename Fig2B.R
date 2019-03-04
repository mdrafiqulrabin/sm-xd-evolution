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

p <- ggplot() + 
  geom_line(aes(x=years,y=directXdLink,colour="linecolor1"),size=2) + 
  geom_line(aes(x=years,y=mediatedXdLink,colour="linecolor2"),size=2) + 
  geom_rect(alpha=0.3, fill="red",
            aes(xmin=1990, xmax=2003, ymin=0.0, ymax=0.3)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1980, 2016),
                     breaks = seq(1980, 2010, 10)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.3),
                     breaks = seq(0.0, 0.2, 0.1)) +
  annotate(geom="text", hjust=0, size=3, colour='black',
           x=1994, y=0.28, label="HGP (1990-2003)") +
  xlab("") + ylab("f.,XD(t)\nFraction of collaborations\nthat are cross-disciplinary") +
  theme(plot.title = element_text(hjust = 0.5, size = 7, face = "bold"),
        legend.position=c(0,0.9), legend.justification='left', legend.direction='vertical') +
  scale_colour_manual(name="",labels=c("Direct XD links","Mediated XD links"),
                      values=c(linecolor1="blue",linecolor2="red"))

# Show Line
library(gridExtra, warn.conflicts = FALSE)
grid.arrange(p, 
             bottom="Fig. 2. Growth of cross-disciplinary social capital.
             (B) Evolution of the fraction of collaboration links in the F network that are cross-disciplinary.")
