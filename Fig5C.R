library(ggplot2, warn.conflicts=F)

# beta[I] vline
df_vline <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_vline) <- c("xval","color","type")
df_vline <- rbind(df_vline, data.frame(xval=0.10, color="blue" , type="line"))
df_vline <- rbind(df_vline, data.frame(xval=0.145, color="blue" , type="dashed"))
df_vline <- rbind(df_vline, data.frame(xval=0.19, color="blue" , type="line"))

# Dummy data
x_temp = seq(0.0,0.2,0.01)
y_temp = seq(0.0,0.2,0.01)
df = data.frame(x_temp,y_temp)

# Plot 5C
fig5C <- ggplot(df, aes(x=x_temp, y=y_temp)) +
  geom_vline(xintercept=c(0.0,0.10), linetype="dashed") +
  geom_vline(data = df_vline, aes(xintercept = xval, linetype = df_vline$type), 
             color = df_vline$color, lwd = 1) +
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