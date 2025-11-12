# Details ----
#' 08_fig7.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Output: Final Figure (Fig 7)
#' Dependencies: 01_src_modelfxns
#' *Changes from original:
#' Pulled repetitive content from graphical parameters
#' -----------

source('src_modelfxns.R')
library(ggplot2)
library(cowplot)

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv)

A1 <- function(){
  plot(dfAT$SRL, dfAT$TLCorr,
       xlab="Standard Rostrum Length (mm)", 
       ylab="Total Length (mm)",
       xlim = c(50, 800),
       ylim = c(500, 4000),
       xaxp = c(100, 700, 3),
       yaxp = c(500, 3500, 3))
  title("A. cuspidata", font.main=3,
        adj = 0.225, line = -1.5)
  curve(AWhittyL, from = 100, to = 420,
        add=TRUE,
        col='darkblue',
        lwd=2, lty=5)
  curve(AFariaL, from=380, to=750, 
        add=TRUE, 
        col='orange',
        lwd=2, lty=5)
  abline(v = 400, lty = 3)
  abline(h = 2000, lty = 3)
  # legend("bottomright",
  #        legend = c("Faria, 2007 - Linear Model",
  #                   "Lear et al. 2023 - Female Equation",
  #                   "Whitty et al. 2014 - Table 2 Ratio",
  #                   "Whitty et al. 2014 - Power Model"),
  #        lty = c(2, 1, 2, 1),
  #        col =  c('orange', 'darkgreen', 'darkblue', 'lightblue'),
  #        theme(legend.text=element_text(size=12)))
}

C1 <- function(){
  plot(dfCT$SRL, dfCT$TLCorr,
       xlab="", 
       ylab="Total Length (mm)",
       xlim = c(50, 700),
       ylim = c(500, 4200),
       xaxp = c(100, 700, 3),
       yaxp = c(1000, 4000, 3))
  title("P. clavata", font.main=3,
        adj = 0.2, line = -1.5)
  curve(CWhittyP, from=100, to=530, 
        add=TRUE, lwd=2, col='lightblue')
  abline(v = 300, lty = 3)
  abline(h = 1600, lty = 3)
}

P1 <- function(){
  plot(dfPT$SRL, dfPT$TLCorr,
       xlab="", 
       ylab="",
       xlim = c(50, 1500),
       ylim = c(700, 6500),
       xaxp = c(200, 1400, 3),
       yaxp = c(500, 6500, 3))
  title("P. pristis", font.main=3,
        adj = 0.2, line = -1.5)
  # curve(PThorF, from=150, to=1450,
  #       add=TRUE, lwd=2, col='magenta')
  curve(PWhittyL, from=150, to=1420, 
        add=TRUE, lwd=2, lty=2, col='darkblue')
  abline(v = 350, lty = 3)
  abline(h = 1500, lty = 3)
}

Z1 <- function(){
  plot(dfZT$SRL, dfZT$TLCorr,
       xlab="", 
       ylab="",
       main="Pristis zijsron", font.main=3,
       xlim = c(50, 1500),
       ylim = c(700, 6000))
  curve(ZLearF, from=175, to=550,
        add=TRUE, lwd=2, col='darkgreen')
  curve(ZFariaL, from = 550, to = 1450,
        add=TRUE, 
        col='orange', lwd=2, lty=2)
  abline(v = 550, lty = 3)
  abline(h = 2250, lty = 3)
}

zmodel <-lm(TLCorr ~ SRL, data = dfZT)
ZBiskisLin <- function(SRL){
  TL <- 3.843*SRL + 98.3
  return(TL)
}

ZNew<-function(){
  plot(dfZT$SRL, dfZT$TLCorr,
       xlab="Standard Rostrum Length (mm)", 
       ylab="",
       xlim = c(50, 1500),
       ylim = c(700, 6000),
       xaxp = c(200, 1400, 3),
       yaxp = c(500, 5000, 3))
  title("P. zijsron", font.main=3,
        adj = 0.2, line = -1.5)
  curve(ZBiskisLin, from = 175, to = 1450,
        add=TRUE, 
        col='seagreen', lwd=2, lty=2)
  abline(v = 550, lty = 3)
  abline(h = 2250, lty = 3)
  lines(dfZT$SRL[order(dfZT$SRL)], predict(zmodel, interval = "confidence")[, "lwr"][order(dfZT$SRL)])
  lines(dfZT$SRL[order(dfZT$SRL)], predict(zmodel, interval = "confidence")[, "upr"][order(dfZT$SRL)])
}

FinPubs <- cowplot::plot_grid(C1, P1, A1, ZNew,
                              labels = "", ncol = 2)
FinPubs

ggsave(
  "LastFig.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 9,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
