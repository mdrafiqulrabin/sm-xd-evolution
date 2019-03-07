# Set working directory
setwd("~/Workspace/RStudio/sm-xd-evolution/")

# Import library
library(plyr, warn.conflicts=F)
library(dplyr, warn.conflicts=F)
library(scales, warn.conflicts=F)
library(readr, warn.conflicts=F)
library(ggplot2, warn.conflicts=F)
library(plotly, warn.conflicts=F)

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
                    xbreak, ybreak,
                    xtext, ytext, aText,
                    nf, slegend) {
  
  valBIO=""; valCS=""; valXD=""
  if(xlog == T) {
    valBIO = log10(dfBIO[[xfield]])
    valCS  = log10(dfCS[[xfield]])
    valXD  = log10(dfXD[[xfield]])
  } else {
    valBIO = nf * dfBIO[[xfield]]
    valCS  = nf * dfCS[[xfield]]
    valXD  = nf * dfXD[[xfield]]
  }
  
  denBIO = density(valBIO, bw = nbw[1])
  denCS  = density(valCS,  bw = nbw[2])
  denXD  = density(valXD,  bw = nbw[3])
  
  fig_3 <- plot_ly(x = denBIO$x, y = denBIO$y, name = "BIO", 
                   type = "scatter", mode = "lines", fill = "tozeroy",
                   line = list(color = "green"),
                   fillcolor = "toRGB('green', alpha=0.3)",
                   showlegend = slegend) %>%
    add_trace(x = denCS$x, y = denCS$y, name = "CS", 
              type = "scatter", mode = "lines", fill = "tozeroy",
              line = list(color = "magenta"),
              fillcolor = "toRGB('magenta', alpha=0.3)") %>%
    add_trace(x = denXD$x, y = denXD$y, name = "XD", 
              type = "scatter", mode = "lines", fill = "tozeroy",
              line = list(color = "grey"),
              fillcolor = "toRGB('grey', alpha=0.3)") %>%
    add_lines(x=mean(valBIO), line=list(color="green",dash='dot'), showlegend=F) %>% 
    add_lines(x=mean(valCS), line=list(color="magenta",dash='dot'), showlegend=F) %>% 
    add_lines(x=mean(valXD), line=list(color="black",dash='dot'), showlegend=F) %>%
    add_annotations(x=0.9, y=0.9, text=aText, xref="paper", yref="paper", showarrow=F)
  
  if(ylog == T) {
    fig_3 <- fig_3 %>% layout(yaxis = list(range = yrange, title = ytext, 
                      type = "log", exponentformat="E", 
                      autotick=F, dtick=ybreak, showgrid=F),
           xaxis = list(range = xrange, title = xtext, 
                        autotick=F, dtick=xbreak, showgrid=F),
           shapes = list(type="rect", x0=0, y0=0, x1=1, y1=1, 
                         xref="paper", yref="paper"))
  } else {
    fig_3 <- fig_3 %>% layout(yaxis = list(range = yrange, title = ytext, 
                          autotick=F, dtick=ybreak, showgrid=F),
             xaxis = list(range = xrange, title = xtext, 
                          autotick=F, dtick=xbreak, showgrid=F),
             shapes = list(type="rect", x0=0, y0=0, x1=1, y1=1, 
                           xref="paper", yref="paper"),
             legend = list(orientation="h", xanchor="center", x=0.5, y=1.1))
  }
  
  return(fig_3)
}

# Fig3-A: Probability distribution of the year of first publication.
fig_3a <- getFig3bf(F, F, 
                    "min_year", c(2,2,2),
                    c(1954,2016), c(0.00,0.04), 
                    10, 0.01,
                    "Year of first publication, y<sub>i</sub><sup>0</sup>", 
                    "PDF(y<sub>i</sub><sup>0</sup>)", 
                    "<b>A</b>", 1, T)
#fig_3a

# Fig3-B: Probability distribution of the total number of collaborators.
fig_3b <- getFig3bf(F, T, 
                  "KTotal", c(38,42,48),
                  c(0,1900), c(-6,-2), 
                  500, 1,
                  "Total collaboration degree, K<sub>i</sub>", 
                  "PDF(K<sub>i</sub>)", 
                  "<b>B</b>", 1, F)
#fig_3b

# Fig3-C: Probability distribution of the fraction of the collaborators who are cross-disciplinary.
fig_3c <- getFig3bf(F, T,
                  "Chi", c(0.02,0.025,0.02),
                  c(0.0,1.0), c(-2,1), 
                  0.2, 1,
                  "Cross-disciplinarity, X<sub>i</sub>", 
                  "PDF(X<sub>i</sub>)", 
                  "<b>C</b>", 1, F)
#fig_3c

# Fig3-D: Probability distribution of the PageRank centrality scaled by number of F.
fig_3d <- getFig3bf(F, T,
                  "PRCentrality", c(0.3,0.3,0.3),
                  c(0,9), c(-4,0), 
                  2, 1,
                  "PageRank centrality, N<sub>F</sub> * E<sub>i</sub><sup>PR</sup>", 
                  "PDF(N<sub>F</sub> * E<sub>i</sub><sup>PR</sup>)", 
                  "<b>D</b>", nrow(df), F)
#fig_3d

# Fig3-E: Probability distribution of the mean impact factor of the publication record.
fig_3e <- getFig3bf(F, T,
                  "mean_of_IF", c(1,1,1),
                  c(0,29), c(-4,0), 
                  5, 1,
                  "Mean publication impact factor, <span style='text-decoration: overline'>IF<sub>i</sub></span>", 
                  "PDF(<span style='text-decoration: overline'>IF<sub>i</sub></span>)", 
                  "<b>E</b>", 1, F)
#fig_3e

# Fig3-F: Probability distribution of the total citations.
fig_3f <- getFig3bf(T, T,
                  "t_pubs_citations", c(0.18,0.18,0.18),
                  c(0,6.2), c(-4,0), 
                  1, 1,
                  "Total career citation, log<sub>10</sub>C<sub>i</sub>", 
                  "PDF(log<sub>10</sub>C<sub>i</sub>)", 
                  "<b>F</b>", 1, F)
#fig_3f

# Show Plots
subplot(nrows=3, margin=0.07,
        fig_3a, fig_3b, 
        fig_3c, fig_3d, 
        fig_3e, fig_3f,
        titleX=T, titleY=T)
