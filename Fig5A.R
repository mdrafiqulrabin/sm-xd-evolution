library(ggplot2)
library(dplyr)
library(forcats)

df <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                 fe_standardized = c(0.208, -0.0978, 0.145),
                 fe_stand_error = c(0.00365, 0.0187, 0.0235)
                 )

fig5A <- ggplot(df, aes(x=parameters, y=fe_standardized))

fig5A + 
  geom_point(size = 2) +
  geom_errorbar( aes(ymin = fe_standardized + fe_stand_error, 
                     ymax = fe_standardized - fe_stand_error), 
                 width=0.05) +
  aes(x = fct_inorder(parameters)) +
  labs(x = "", y = "Regression\nCoefficients") +
  coord_cartesian(ylim = c(-.15, .22), clip = 'off') +
  theme(plot.margin = unit(c(5,3,4,3), "lines"))  +
  geom_hline(yintercept=c(0,3), linetype="dashed") 
  
