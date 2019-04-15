library(ggplot2, warn.conflicts=F)

x_temp = seq(0.0,0.2,0.01)
y_temp = seq(0.0,0.2,0.01)
df = data.frame(x_temp,y_temp)
fig5C <- ggplot(df, aes(x=x_temp, y=y_temp)) +
  scale_x_continuous(expand = c(-0.10, 0), breaks = seq(-0.05,0.20,0.05)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0.0,0.12,0.04)) +
  coord_cartesian(xlim = c(-0.10, 0.25), ylim = c(0.0, 0.15), clip = 'off') +
  xlab(expression("Cross-disciplinarity coefficient,"~beta[italic("I")]~"")) +
  ylab(expression("Prob. dist. P("~beta[italic("I")]~")")) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        plot.margin = unit(c(5,3,4,3), "lines"))
fig5C