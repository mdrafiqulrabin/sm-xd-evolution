setwd("H:/Ph.D/2nd Semester/statistical-method-in-research/Project/sm-xd-evolution")

library(ggplot2)
library(dplyr)
library(forcats)

fig4_data = read.csv("Data/Fig4/fig4.csv")
head(fig4_data)


df <- data.frame(Parameters = c('Br', 'B$1', 'Bn1', 'B$2', 'Bn2', 'BcPR', 'Bx'),
                 Standardized = c(-0.056, -0.036, 0.015, 0.082, -0.068, 0.026, 0.085),
                 Error = c(0.006, 0.02, 0.015, 0.014, 0.014, 0.012, 0.011))

fig4 <- ggplot(df, aes(x=Parameters, y=Standardized))

fig4 + geom_point(size = 2) +
  geom_errorbar( aes(ymin = Error + Standardized, 
                     ymax = Standardized - Error), width=0.1, linetype="solid", lwd = 1) +
    aes(x = fct_inorder(Parameters)) +
    labs(x = "", y = "    Standardized regression coefficients point 
       estimate with 95% confidence interval") +
    coord_cartesian(ylim = c(-.10, .10), clip = 'off') +
    theme(plot.margin = unit(c(5,3,5,3), "lines")) +
    geom_hline(yintercept=c(0,8), linetype="dashed") +
    
    geom_segment(aes(x= 0.75,y=0.15,xend=5.25,yend=0.15)) +
    geom_segment(aes(x=0.75,y=0.15,xend=0.75,yend=0.14)) +
    geom_segment(aes(x=5.25,y=0.15,xend=5.25,yend=0.14)) +
  
    geom_segment(aes(x= 5.75,y=0.15,xend=7.25,yend=0.15)) + 
    geom_segment(aes(x=5.75,y=0.15,xend=5.75,yend=0.14)) +
    geom_segment(aes(x=7.25,y=0.15,xend=7.25,yend=0.14)) +
    
    geom_text(aes(x=6.5,y=0.165,label="Network")) +
    geom_text(aes(x=3.0,y=0.165,label="CV")) +

    geom_text(aes(x=0.9,y=0.13,label="*")) +
    geom_text(aes(x=0.9,y=0.122,label="*")) +
    geom_text(aes(x=0.9,y=0.114,label="*")) +
    
    geom_text(aes(x=4.0,y=0.13,label="*")) +
    geom_text(aes(x=4.0,y=0.122,label="*")) +
    geom_text(aes(x=4.0,y=0.114,label="*")) +
    
    geom_text(aes(x=5.0,y=0.13,label="*")) +
    geom_text(aes(x=5.0,y=0.122,label="*")) +
    geom_text(aes(x=5.0,y=0.114,label="*")) +
    
    geom_text(aes(x=7.0,y=0.13,label="*")) +
    geom_text(aes(x=7.0,y=0.122,label="*")) +
    geom_text(aes(x=7.0,y=0.114,label="*")) +
    
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1)
    )  +
  
    scale_x_discrete("", labels = c(expression(beta[r]),expression(beta["$1"]), 
                                    expression(beta[N1]), expression(beta["$2"]), 
                                    expression(beta[N2]), expression(beta[c^PR]), 
                                    expression(beta[X])))
  
  
    
  
  
