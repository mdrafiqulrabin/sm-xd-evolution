library(ggplot2)
library(dplyr)
library(forcats)

df <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                 fe_standardized = c(0.208, -0.0978, 0.145),
                 fe_stand_error = c(0.00365, 0.0187, 0.0235)
                 )
pooled_df = data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                       fe_standardized = c(0.189, -0.0564, 0.126),
                       fe_stand_error = c(0.00479, 0.00947, 0.0341)
                  )


fig5A <- ggplot(df, aes(x=parameters, y=fe_standardized))

fig5A + 
  geom_point(data = df, 
#             aes(x=c(0.9, 1.9, 2.9), y=fe_standardized),
             colour = "blue", size = 1.5) +
  geom_errorbar(aes(ymin = fe_standardized + fe_stand_error, 
                    ymax = fe_standardized - fe_stand_error),
#                    x=c(0.9, 1.9, 2.9),
                width=0.04, colour = "blue", size=0.9) +

  geom_point(data = pooled_df, 
             aes(x = c(1.2, 2.2, 3.2), y=fe_standardized),
             colour = "black", size = 1.5) +
  geom_errorbar(data = pooled_df, 
                aes(x=c(1.2, 2.2, 3.2),
                ymin = fe_standardized + fe_stand_error, 
                ymax = fe_standardized - fe_stand_error), 
                width=0.04, colour = "black", size=0.9) +
  
  coord_cartesian(ylim = c(-.15, .22), clip = 'off') +
  theme(plot.margin = unit(c(5,3,4,3), "lines"))  +
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
  
  aes(x = fct_inorder(parameters)) +
  scale_x_discrete("", labels = c(expression("Coauthors, beta[a]"), 
                                  expression("Author age, beta[t]"), 
                                  expression("Cross-disc., beta[I]"))) +

  labs(x = "", y = "Regression\nCoefficients") +
  geom_text(aes(x=1.85,y=0.21,label="All faculty, Fi"), size = 3.0) +
  geom_text(aes(x=2.1,y=0.18,label="Fixed effects: Standardized variables"), size = 2.5) +
  geom_text(aes(x=2.1,y=0.16,label="Pooled: Standardized variables       "), size = 2.5) +
  geom_segment(aes(x= 1.32,y=0.18,xend=1.4,yend=0.18), size = 1, colour = "blue") +
  geom_segment(aes(x= 1.32,y=0.155,xend=1.4,yend=0.155), size = 1, colour = "black")
  
  

                   