# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/Panel_Analysis/Data/")

# Import library
library(readr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(forcats, warn.conflicts=F)

getFigA <- function() {
  df <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                   fe_standardized = c(0.206,  -0.0971, 0.145),
                   fe_stand_error  = c(0.00361, 0.0186, 0.0235))
  
  pooled_df = data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                         fe_standardized = c(0.187,  -0.0560,  0.126),
                         fe_stand_error  = c(0.00474, 0.00940, 0.0341))
  
  fig5A <- ggplot(df, aes(x=parameters, y=fe_standardized)) +
    geom_point(data = df, colour = "blue", size = 1.5) +
    geom_errorbar(aes(ymin = fe_standardized + fe_stand_error * 2, 
                      ymax = fe_standardized - fe_stand_error * 2),
                  width=0.04, colour = "blue", size=0.9) +
    
    geom_point(data = pooled_df, 
               aes(x = c(1.2, 2.2, 3.2), y=fe_standardized),
               colour = "black", size = 1.5) +
    geom_errorbar(data = pooled_df, 
                  aes(x=c(1.2, 2.2, 3.2),
                      ymin = fe_standardized + fe_stand_error * 2, 
                      ymax = fe_standardized - fe_stand_error * 2), 
                  width=0.04, colour = "black", size=0.9) +
    
    coord_cartesian(ylim = c(-.15, .22), clip = 'off') +
    #theme(plot.margin = unit(c(5,3,4,3), "lines"))  +
    geom_hline(yintercept=c(0,3), linetype="dashed") +
    
    geom_text(aes(x=1.0,y=0.27,label="*")) +
    geom_text(aes(x=1.0,y=0.26,label="*")) +
    geom_text(aes(x=1.0,y=0.25,label="*")) +
    
    geom_text(aes(x=2.0,y=0.27,label="*")) +
    geom_text(aes(x=2.0,y=0.26,label="*")) +
    geom_text(aes(x=2.0,y=0.25,label="*")) +
    
    geom_text(aes(x=3.0,y=0.27,label="*")) +
    geom_text(aes(x=3.0,y=0.26,label="*")) +
    geom_text(aes(x=3.0,y=0.25,label="*")) +
    
    geom_text(aes(x=1.2,y=0.27,label="*")) +
    geom_text(aes(x=1.2,y=0.26,label="*")) +
    geom_text(aes(x=1.2,y=0.25,label="*")) +
    
    geom_text(aes(x=2.2,y=0.27,label="*")) +
    geom_text(aes(x=2.2,y=0.26,label="*")) +
    geom_text(aes(x=2.2,y=0.25,label="*")) +
    
    geom_text(aes(x=3.2,y=0.27,label="*")) +
    geom_text(aes(x=3.2,y=0.26,label="*")) +
    geom_text(aes(x=3.2,y=0.25,label="*")) +
    
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=.5)
    ) +
    
    labs(tag = "A", x = "", y = "Regression\nCoefficients") +
    geom_text(aes(x=1.85,y=0.21,label="All faculty, Fi"), size = 3.0, hjust = 0) +
    geom_text(size = 2.5, hjust = 0, 
              aes(x=1.5,y=0.18, label="Fixed effects: Standardized variables")) +
    geom_text(size = 2.5, hjust = 0, 
              aes(x=1.5,y=0.16, label="Pooled: Standardized variables")) +
    geom_segment(aes(x= 1.35,y=0.18,xend=1.44,yend=0.18), 
                 size = 1, colour = "blue") +
    geom_segment(aes(x= 1.35,y=0.155,xend=1.44,yend=0.155), 
                 size = 1, colour = "black") +
    
    aes(x = fct_inorder(parameters)) +
    scale_x_discrete("", labels = c(expression("Coauthors, " ~ beta[italic(alpha)]), 
                                    expression("Author age, " ~ beta[italic(tau)]), 
                                    expression("Cross-disc., " ~ beta[italic(I)]) ))
  
  return(fig5A)
}

getFigB <- function() {
  df_all <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                       fe_standardized = c(0.206,  -0.0971, 0.145),
                       fe_stand_error  = c(0.00361, 0.0186, 0.0235))
  
  df_xd <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                      fe_standardized = c(0.252,  -0.0662, 0.112),
                      fe_stand_error  = c(0.00632, 0.0272, 0.0234))
  
  df_matched <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                           fe_standardized = c(0.387,  0.489,  0.135),
                           fe_stand_error  = c(0.0581, 0.0653, 0.0470))
  
  
  fig5B <- ggplot(df_xd, aes(x=parameters, y=fe_standardized)) +
    geom_point(data = df_xd, 
               colour = "blue", size = 1.5, shape = 22, fill = "blue") +
    geom_errorbar(aes(ymin = fe_standardized + fe_stand_error * 2, 
                      ymax = fe_standardized - fe_stand_error * 2),
                  width=0.04, colour = "blue", size=0.9) +
    
    geom_point(data = df_all, 
               aes(x = c(0.9, 1.9, 2.9), y=fe_standardized),
               colour = "blue", size = 1.5, shape = 21, fill = "blue") +
    geom_errorbar(data = df_all, 
                  aes(x=c(0.9, 1.9, 2.9),
                      ymin = fe_standardized + fe_stand_error * 2, 
                      ymax = fe_standardized - fe_stand_error * 2), 
                  width=0.04, colour = "blue", size=0.9) +
    
    geom_point(data = df_matched, 
               aes(x = c(1.1, 2.1, 3.1), y=fe_standardized),
               colour = "blue", size = 2.5, shape = 23, fill = "blue") +
    geom_errorbar(data = df_matched, 
                  aes(x=c(1.1, 2.1, 3.1),
                      ymin = fe_standardized + fe_stand_error * 2, 
                      ymax = fe_standardized - fe_stand_error * 2), 
                  width=0.04, colour = "blue", size=0.9) +
    
    aes(x = fct_inorder(parameters)) +
    coord_cartesian(ylim = c(-.15, .6), clip = 'off') +
    #theme(plot.margin = unit(c(5,3,4,3), "lines"))  +
    geom_hline(yintercept=c(0,3), linetype="dashed") +
    
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=.5)
    ) +
    
    labs(tag = "B", x = "", y = "Regression\nCoefficients") + 
    
    geom_text(aes(x=0.9,y=0.7,label="*")) +
    geom_text(aes(x=0.9,y=0.675,label="*")) +
    geom_text(aes(x=0.9,y=0.65,label="*")) +
    
    geom_text(aes(x=1.0,y=0.7,label="*")) +
    geom_text(aes(x=1.0,y=0.675,label="*")) +
    geom_text(aes(x=1.0,y=0.65,label="*")) +
    
    geom_text(aes(x=1.1,y=0.7,label="*")) +
    geom_text(aes(x=1.1,y=0.675,label="*")) +
    geom_text(aes(x=1.1,y=0.65,label="*")) +
    
    geom_text(aes(x=1.9,y=0.7,label="*")) +
    geom_text(aes(x=1.9,y=0.675,label="*")) +
    geom_text(aes(x=1.9,y=0.65,label="*")) +
    
    geom_text(aes(x=2.0,y=0.69,label="*")) +
    geom_text(aes(x=2.0,y=0.665,label="*")) +
    
    geom_text(aes(x=2.1,y=0.7,label="*")) +
    geom_text(aes(x=2.1,y=0.675,label="*")) +
    geom_text(aes(x=2.1,y=0.65,label="*")) +
    
    geom_text(aes(x=2.9,y=0.7,label="*")) +
    geom_text(aes(x=2.9,y=0.675,label="*")) +
    geom_text(aes(x=2.9,y=0.65,label="*")) +
    
    geom_text(aes(x=3.0,y=0.7,label="*")) +
    geom_text(aes(x=3.0,y=0.675,label="*")) +
    geom_text(aes(x=3.0,y=0.65,label="*")) +
    
    geom_text(aes(x=3.1,y=0.69,label="*")) +
    geom_text(aes(x=3.1,y=0.665,label="*")) +
    
    geom_text(size = 2.5, aes(x=1.3,y=0.58,
                              label="Fixed effects: Standardized vars")) +
    geom_segment(aes(x= 0.6,y=0.57,xend=0.68,yend=0.57), 
                 size = 1.05, colour = "blue") +
    
    geom_text(size = 2.5, aes(x=2.5,y=0.6, label="All Fi"), hjust = 0) +
    geom_text(size = 2.5, aes(x=2.5,y=0.55, label="Only XD Fi"), hjust = 0) +
    geom_text(size = 2.5, aes(x=2.5,y=0.5, label="Only XD Fi: matched"), hjust = 0) +
    
    geom_point(aes(x = 2.4, y=.6),
               colour = "blue", size = 2, shape = 21, fill = "blue") +
    geom_point(aes(x = 2.4, y=.55),
               colour = "blue", size = 2, shape = 22, fill = "blue") +
    geom_point(aes(x = 2.4, y=.5),
               colour = "blue", size = 2, shape = 23, fill = "blue") +
    
    aes(x = fct_inorder(parameters)) +
    scale_x_discrete("", labels = c(expression("Coauthors, " ~ beta[italic(alpha)]), 
                                    expression("Author age, " ~ beta[italic(tau)]), 
                                    expression("Cross-disc., " ~ beta[italic(I)]) ))
  return(fig5B)
}

getFigCD <- function(IsFigD) {
  # Read beta_i CSV
  if (IsFigD == TRUE) {
    df = read.csv("beta_i_xd_r2.csv")
  } else {
    df = read.csv("beta_i_all_r2.csv")
  }
  
  # beta_i vline
  bi_value = ""; pm_value = ""
  if (IsFigD == TRUE) {
    bi_value <- c(0.080, 0.112, 0.144) #TableS5.R
    pm_value <- c(-0.013, 0.031) #Fig5D.R
  } else {
    bi_value <- c(0.114, 0.145, 0.176) #TableS4.R
    pm_value <- c(-0.017, 0.035) #Fig5C.R
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
  #bi_gtext <- rbind(bi_gtext, data.frame(xval=bi_value[2]-0.015, yval=0.08, color="blue", label=paste0("Bi = ",bi_value[2])))
  bi_gtext <- rbind(bi_gtext, data.frame(xval=(bi_value[2]+0.015), yval=0.08, color="blue", 
                                         label=paste0("95% CI = (",bi_value[1]," , ",bi_value[3],")")))
  
  # Histogram
  hg_bw = ""; hg_xbreak = ""; hg_ybreak = ""; hg_xlim = ""; hg_ylim = "";
  if (IsFigD == TRUE) {
    hg_bw <- 0.008
    hg_label <- "D"
    hg_xbreak <- seq(-0.05,0.20,0.05)
    hg_ybreak <- seq(0.0,0.15,0.05)
    hg_xlim   <- c(-0.11, 0.18)
    hg_ylim   <- c(0.0, 0.18)
  } else {
    hg_bw <- 0.006
    hg_label <- "C"
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
    #annotate("text", x=-Inf, y=Inf, label = hg_label, parse=T, size=7) +
    labs(tag = hg_label) +
    theme(
          panel.background = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, size=0.5),
          plot.title = element_text(hjust = 0.5)) 
  return (hg)
}

# Call Plots
f5a = getFigA()
f5b = getFigB()
f5c = getFigCD(FALSE)
f5d = getFigCD(TRUE)

# Show Plots
library(gridExtra, warn.conflicts=F)
grid.arrange(ncol=2, f5a, f5b, f5c, f5d)
