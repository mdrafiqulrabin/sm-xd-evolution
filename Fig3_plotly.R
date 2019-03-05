# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

# Data.Frame
df = read.csv("data/Faculty_GoogleScholar_Funding_Data_N4190.csv")
df = data.frame(df %>% select(XDIndicator, 
     min_year, KTotal, Chi, mean_of_IF, t_pubs_citations, PRCentrality))

dfBIO = df %>% filter(XDIndicator == "BIO")
dfCS  = df %>% filter(XDIndicator == "CS")
dfXD  = df %>% filter(XDIndicator == "XD")

# Common Method
getFig3bf <- function(xlog, ylog,
                    xfield, nbw,
                    xrange, yrange, 
                    xtext, ytext,
                    nf, slegend) {
  
  fitBIO=""; fitCS=""; fitXD=""
  if(xlog == TRUE) {
    fitBIO = density(log10(dfBIO[[xfield]]),bw = nbw[1])
    fitCS  = density(log10(dfCS[[xfield]]), bw = nbw[2])
    fitXD  = density(log10(dfXD[[xfield]]), bw = nbw[3])
  } else {
    fitBIO = density(nf * dfBIO[[xfield]],bw = nbw[1])
    fitCS  = density(nf * dfCS[[xfield]], bw = nbw[2])
    fitXD  = density(nf * dfXD[[xfield]], bw = nbw[3])
  }
  
  fig_3 <- plot_ly(x = fitBIO$x, y = fitBIO$y, name = "BIO", 
                   type = "scatter", mode = "lines", fill = "tozeroy",
                   line = list(color = "green"),
                   fillcolor = "toRGB('green', alpha=0.3)",
                   showlegend = slegend) %>%
    add_trace(x = fitCS$x, y = fitCS$y, name = "CS", 
              type = "scatter", mode = "lines", fill = "tozeroy",
              line = list(color = "magenta"),
              fillcolor = "toRGB('magenta', alpha=0.3)") %>%
    add_trace(x = fitXD$x, y = fitXD$y, name = "XD", 
              type = "scatter", mode = "lines", fill = "tozeroy",
              line = list(color = "grey"),
              fillcolor = "toRGB('grey', alpha=0.3)")
  
  if(ylog == TRUE) {
    fig_3 <- fig_3 %>% layout(yaxis = list(range = yrange, title = ytext, 
                      type = "log", exponentformat="E", showgrid=F),
           xaxis = list(range = xrange, title = xtext, showgrid=F))
  } else {
    fig_3 <- fig_3 %>% layout(yaxis = list(range = yrange, title = ytext, showgrid=F),
             xaxis = list(range = xrange, title = xtext, showgrid=F),
             legend = list(orientation="h", xanchor="center", x=0.5, y=1.1))
  }
  
  return(fig_3)
}

# Fig3-A: Probability distribution of the year of first publication.
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(min_year))
fig_3a <- getFig3bf(FALSE, FALSE, 
                    "min_year", c(2,2,2),
                    c(1954,2016), c(0.00,0.04),
                    "Year of first publication, y_i^0", 
                    "PDF(y_i^0)", 1, TRUE)
#fig_3a

# Fig3-B: Probability distribution of the total number of collaborators.
fig_3b <- getFig3bf(FALSE, TRUE, 
                  "KTotal", c(38,42,48),
                  c(0,2000), c(-6,-2),
                  "Total collaboration degree, K_i", 
                  "PDF(K_i)", 1, FALSE)
#fig_3b

# Fig3-C: Probability distribution of the fraction of the collaborators who are cross-disciplinary.
fig_3c <- getFig3bf(FALSE, TRUE,
                  "Chi", c(0.02,0.025,0.02),
                  c(0.0,1.0), c(-2,1), 
                  "Cross-disciplinarity, X_i", 
                  "PDF(X_i)", 1, FALSE)
#fig_3c

# Fig3-D: Probability distribution of the PageRank centrality scaled by number of F.
fig_3d <- getFig3bf(FALSE, TRUE,
                  "PRCentrality", c(0.3,0.3,0.3),
                  c(0,9), c(-4,0), 
                  "PageRank centrality, N_F * E_i^PR", 
                  "PDF(N_F * E_i^PR)", nrow(df), FALSE)
#fig_3d

# Fig3-E: Probability distribution of the mean impact factor of the publication record.
fig_3e <- getFig3bf(FALSE, TRUE,
                  "mean_of_IF", c(1,1,1),
                  c(0,29), c(-4,0), 
                  "Mean publication impact factor, -IF_i", 
                  "PDF(-IF_i)", 1, FALSE)
#fig_3e

# Fig3-F: Probability distribution of the total citations.
fig_3f <- getFig3bf(TRUE, TRUE,
                  "t_pubs_citations", c(0.18,0.18,0.18),
                  c(0,7), c(-4,0),
                  "Total career citation, log10 C_i", 
                  "PDF(log10 C_i)", 1, FALSE)
#fig_3f

# Show Plots
subplot(nrows=3, margin=0.07,
        fig_3a, fig_3b, 
        fig_3c, fig_3d, 
        fig_3e, fig_3f,
        titleX=T, titleY=T)
