library(ggplot2)
library(dplyr)
library(forcats)

df_all <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                 fe_standardized = c(0.208, -0.0978, 0.145),
                 fe_stand_error = c(0.00365, 0.0187, 0.0235))

df_xd <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                 fe_standardized = c(0.234, -0.0635, 0.112),
                 fe_stand_error = c(0.00588, 0.0261, 0.0234))

df_matched <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                      fe_standardized = c(0.373, 0.491, 0.135),
                      fe_stand_error = c(0.0333, 0.0519, 0.0471))


fig5B <- ggplot(df_xd, aes(x=parameters, y=fe_standardized))

fig5B + 
  geom_point(data = df_xd, 
             colour = "blue", size = 1.5) +
  geom_errorbar(aes(ymin = fe_standardized + fe_stand_error, 
                    ymax = fe_standardized - fe_stand_error),
                width=0.04, colour = "blue", size=0.9) +
  
  geom_point(data = df_all, 
             aes(x = c(0.9, 1.9, 2.9), y=fe_standardized),
             colour = "blue", size = 1.5) +
  geom_errorbar(data = df_all, 
                aes(x=c(0.9, 1.9, 2.9),
                    ymin = fe_standardized + fe_stand_error, 
                    ymax = fe_standardized - fe_stand_error), 
                width=0.04, colour = "blue", size=0.9) +

  geom_point(data = df_matched, 
           aes(x = c(1.1, 2.1, 3.1), y=fe_standardized),
           colour = "blue", size = 1.5) +
  geom_errorbar(data = df_matched, 
                aes(x=c(1.1, 2.1, 3.1),
                    ymin = fe_standardized + fe_stand_error, 
                    ymax = fe_standardized - fe_stand_error), 
                width=0.04, colour = "blue", size=0.9) +
  
  aes(x = fct_inorder(parameters)) +
  coord_cartesian(ylim = c(-.15, .6), clip = 'off') +
  theme(plot.margin = unit(c(5,3,4,3), "lines"))  +
  geom_hline(yintercept=c(0,3), linetype="dashed") 
  
  
