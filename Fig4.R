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
                       ymax = Standardized - Error), width=0.2) +
    labs(x = "", y = "   Standardized regression coefficients point 
         estimate with 95% confidence interval") +
    aes(x = fct_inorder(Parameters)) +
    coord_cartesian(ylim = c(-.10, .10), clip = 'off') +
    theme(plot.margin = unit(c(5,3,5,3), "lines"))
    
    
    
