library(ggplot2)
library(dplyr)

fig5C <- ggplot() +
  
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin = unit(c(5,3,4,3), "lines"))
fig5C