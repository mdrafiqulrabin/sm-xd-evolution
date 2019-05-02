
library(ggplot2, warn.conflicts=F)
library(dplyr, warn.conflicts=F)

df <- data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                 fe_standardized = c(0.206,  -0.0971, 0.145),
                 fe_stand_error  = c(0.00361, 0.0186, 0.0235)
                 )
pooled_df = data.frame(parameters = c("co-authors", "author_age", "cross-disc"),
                       fe_standardized = c(0.187,  -0.0560,  0.126),
                       fe_stand_error  = c(0.00474, 0.00940, 0.0341)
                  )

fig5A <- ggplot(df, aes(x=parameters, y=fe_standardized))

fig5A + 
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
  
  labs(x = "", y = "Regression\nCoefficients") +
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
                                  expression("Cross-disc., " ~ beta[italic(I)])
                                  ))
  
