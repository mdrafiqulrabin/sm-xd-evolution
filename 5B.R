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
  theme(plot.margin = unit(c(5,3,4,3), "lines"))  +
  geom_hline(yintercept=c(0,3), linetype="dashed") +
  
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)
  ) +
  
  labs(x = "", y = "Regression\nCoefficients") + 
  
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
  
  geom_text(aes(x=2.0,y=0.7,label="*")) +
  geom_text(aes(x=2.0,y=0.675,label="*")) +
  geom_text(aes(x=2.0,y=0.65,label="*")) +
  
  geom_text(aes(x=2.1,y=0.7,label="*")) +
  geom_text(aes(x=2.1,y=0.675,label="*")) +
  geom_text(aes(x=2.1,y=0.65,label="*")) +

  geom_text(aes(x=2.9,y=0.7,label="*")) +
  geom_text(aes(x=2.9,y=0.675,label="*")) +
  geom_text(aes(x=2.9,y=0.65,label="*")) +
  
  geom_text(aes(x=3.0,y=0.7,label="*")) +
  geom_text(aes(x=3.0,y=0.675,label="*")) +
  geom_text(aes(x=3.0,y=0.65,label="*")) +
  
  geom_text(aes(x=3.1,y=0.7,label="*")) +
  geom_text(aes(x=3.1,y=0.675,label="*")) +
  geom_text(aes(x=3.1,y=0.65,label="*")) +
  
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
  scale_x_discrete("", labels = c(expression("Coauthors, " + beta[alpha]), 
                                  expression("Author age, beta[t]"), 
                                  expression("Cross-disc., beta[I]")))

  
