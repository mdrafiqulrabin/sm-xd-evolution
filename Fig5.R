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
  bi_value = ""; pm_value = ""
  if (allxd == TRUE) {
    bi_value <- c(0.113, 0.146, 0.178) #TableS5.R
    pm_value <- c(-0.034, 0.036) #Fig5D.R
  } else {
    bi_value <- c(0.151, 0.182, 0.213) #TableS4.R
    pm_value <- c(-0.039, 0.029) #Fig5C.R
  }
  bi_vline <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(bi_vline) <- c("xval","color","type")
  bi_vline <- rbind(bi_vline, data.frame(xval=bi_value[1], color="blue" , type="line"))
  bi_vline <- rbind(bi_vline, data.frame(xval=bi_value[2], color="blue" , type="dashed"))
  bi_vline <- rbind(bi_vline, data.frame(xval=bi_value[3], color="blue" , type="line"))
  
  # beta_i gtext
  bi_gtext <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(bi_gtext) <- c("xval","yval","color","label")
  bi_gtext <- rbind(bi_gtext, data.frame(xval=-0.10, yval=0.08, color="black", label=paste0("Placebo model")))
  bi_gtext <- rbind(bi_gtext, data.frame(xval=-0.08, yval=0.08, color="black", 
                                         label=paste0("95% CI = (",pm_value[1]," , ",pm_value[2],")")))
  bi_gtext <- rbind(bi_gtext, data.frame(xval=(bi_value[2]+0.015), yval=0.08, color="blue", 
                                         label=paste0("95% CI = (",bi_value[1]," , ",bi_value[3],")")))
  
  # Histogram
  hg_bw = ""; hg_xbreak = ""; hg_ybreak = ""; hg_xlim = ""; hg_ylim = "";
  if (allxd == TRUE) {
    hg_bw <- 0.008
    hg_label <- "atop(bold(D))"
    hg_xbreak <- seq(-0.05,0.20,0.05)
    hg_ybreak <- seq(0.0,0.15,0.05)
    hg_xlim   <- c(-0.11, 0.18)
    hg_ylim   <- c(0.0, 0.18)
  } else {
    hg_bw <- 0.006
    hg_label <- "atop(bold(C))"
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
    geom_text(data = bi_gtext, aes(x=bi_gtext$xval, y=bi_gtext$yval, label=bi_gtext$label), 
              angle=90, size=3, family="serif", colour=bi_gtext$color) +
    geom_text(aes(x=bi_value[2]-0.015, y=0.08, label=sprintf("beta[I] == %f", bi_value[2])), 
              angle=90, size=3, family="serif", color="blue", parse=T) +
    scale_x_continuous(breaks = hg_xbreak) +
    scale_y_continuous(expand = c(0, 0), breaks = hg_ybreak) +
    coord_cartesian(xlim = hg_xlim, ylim = hg_ylim, clip = 'off') +
    xlab(expression(paste("Cross-disciplinary coefficient, ", beta[I], "", sep=""))) + 
    ylab(expression(paste("Prob. dist. ", P(beta[I]),"", sep=""))) +
    annotate("text", x=-Inf, y=Inf, label = hg_label, parse=T, size=7) +
    theme(aspect.ratio = 0.6, panel.background = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, size=0.5),
          plot.title = element_text(hjust = 0.5)) 
  return (hg)
}

# Call Plots
hg_5c = get_beta_i(FALSE)
hg_5d = get_beta_i(TRUE)

# Show Plots
library(gridExtra, warn.conflicts=F)
grid.arrange(ncol=2, hg_5c, hg_5d, hg_5c, hg_5d)
