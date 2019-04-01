library(ggplot2, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(forcats, warn.conflicts=F)

df <- data.frame(Parameters   = c('Br',   'B$1',  'BN1', 'B$2', 'BN2',  'BCPR', 'BX'),
                 Standardized = c(-0.056, -0.036, 0.015, 0.082, -0.068, 0.026,  0.085),
                 Error        = c(0.006,  0.020,  0.015, 0.014, 0.014,  0.012,  0.011))

fig4 <- ggplot(df, aes(x=Parameters, y=Standardized)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = (Error * 2) + Standardized, ymax = Standardized - (Error * 2)), 
                width = 0.1, linetype = "solid", lwd = 1) +
  aes(x = fct_inorder(Parameters)) +
  labs(x = "", y = "Standardized regression coefficients \n point estimate with 95% confidence interval") +
  coord_cartesian(ylim = c(-0.105, 0.105), clip = 'off') +
  theme(plot.margin = unit(c(5,3,5,3), "lines")) +
  geom_hline(yintercept=c(0,8), linetype="dashed") +
  
  geom_text(aes(x=3.0, y=0.175, label="CV")) +
  geom_text(aes(x=6.5, y=0.175, label="Network")) +
  
  geom_segment(aes(x=0.75, y=0.16, xend=5.25, yend=0.16)) +
  geom_segment(aes(x=0.75, y=0.16, xend=0.75, yend=0.15)) +
  geom_segment(aes(x=5.25, y=0.16, xend=5.25, yend=0.15)) +

  geom_segment(aes(x=5.75, y=0.16, xend=7.25, yend=0.16)) + 
  geom_segment(aes(x=5.75, y=0.16, xend=5.75, yend=0.15)) +
  geom_segment(aes(x=7.25, y=0.16, xend=7.25, yend=0.15)) +

  geom_text(aes(x=0.9, y=0.138, label="*")) +
  geom_text(aes(x=0.9, y=0.130, label="*")) +
  geom_text(aes(x=0.9, y=0.122, label="*")) +
  
  geom_text(aes(x=4.0, y=0.138, label="*")) +
  geom_text(aes(x=4.0, y=0.130, label="*")) +
  geom_text(aes(x=4.0, y=0.122, label="*")) +
  
  geom_text(aes(x=5.0, y=0.138, label="*")) +
  geom_text(aes(x=5.0, y=0.130, label="*")) +
  geom_text(aes(x=5.0, y=0.122, label="*")) +
  
  geom_text(aes(x=7.0, y=0.138, label="*")) +
  geom_text(aes(x=7.0, y=0.130, label="*")) +
  geom_text(aes(x=7.0, y=0.122, label="*")) +

  scale_x_discrete("", labels = c(expression(beta["r"]),expression(beta["$1"]), 
                                  expression(beta["N1"]), expression(beta["$2"]), 
                                  expression(beta["N2"]), expression(beta["C"^"PR"]), 
                                  expression(beta["X"]))) +
  
  theme(axis.line = element_line(colour = "black"),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=.5))

fig4
