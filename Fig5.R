# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Data/Fig5/")

# Import library
library(readr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

get_beta_i <- function(allxd) {
  # Read beta_i CSV
  if (allxd == TRUE) {
    df = read.csv("beta_i_xd.csv")
  } else {
    df = read.csv("beta_i_all.csv")
  }
  
  # beta_i vline
  bi_value = ""
  if (allxd == TRUE) {
    bi_value <- c(0.1133847, 0.1458400, 0.1782953) #TableS5.R
  } else {
    bi_value <- c(0.1505765, 0.1817444, 0.2129123) #TableS4.R
  }
  bi_vline <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(bi_vline) <- c("xval","color","type")
  bi_vline <- rbind(bi_vline, data.frame(xval=bi_value[1], color="blue" , type="line"))
  bi_vline <- rbind(bi_vline, data.frame(xval=bi_value[2], color="blue" , type="dashed"))
  bi_vline <- rbind(bi_vline, data.frame(xval=bi_value[3], color="blue" , type="line"))
  
  # Histogram
  hg_bw = ""; hg_xbreak = ""; hg_ybreak = ""; hg_xlim = ""; hg_ylim = "";
  if (allxd == TRUE) {
    hg_bw <- 0.008
    hg_xbreak <- seq(-0.05,0.20,0.05)
    hg_ybreak <- seq(0.0,0.15,0.05)
    hg_xlim   <- c(-0.11, 0.18)
    hg_ylim   <- c(0.0, 0.18)
  } else {
    hg_bw <- 0.006
    hg_xbreak <- seq(-0.05, 0.20, 0.05)
    hg_ybreak <- seq( 0.00, 0.12, 0.04)
    hg_xlim   <- c(-0.11, 0.22)
    hg_ylim   <- c(0.0, 0.15)
  }
  hg = ggplot(data=df, aes(x=df$x, y=(..count..)/1000)) +
    geom_histogram(binwidth = hg_bw, colour = 'black', alpha=0.5) +
    geom_vline(xintercept=c(0.0), linetype="dashed") +
    geom_vline(data = bi_vline, aes(xintercept = bi_vline$xval, linetype = bi_vline$type), 
               color = bi_vline$color, lwd = 1, show.legend = F) +
    scale_x_continuous(breaks = hg_xbreak) +
    scale_y_continuous(expand = c(0, 0), breaks = hg_ybreak) +
    coord_cartesian(xlim = hg_xlim, ylim = hg_ylim, clip = 'off') +
    xlab(expression(paste("Cross-disciplinary coefficient, ", beta[I], "", sep=""))) + 
    ylab(expression(paste("Prob. dist. ", P(beta[I]),"", sep=""))) +
    theme(panel.background = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, size=0.5),
          plot.title = element_text(hjust = 0.5)) 
  return (hg)
}

hg_5c = get_beta_i(FALSE)
hg_5d = get_beta_i(TRUE)

# Show Histogram
library(gridExtra, warn.conflicts=F)
grid.arrange(ncol=2, hg_5c, hg_5d, hg_5c, hg_5d)
