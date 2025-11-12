# Details ----
#' 01_draw_pub_models.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-11
#' Purpose: Create graphic of published models
#' Dependencies: Functions described in sep source code (bc long)
#' Output: Fig3_SpecCurves.tiff
#' 
#' *Changes from original:
#'  Pulled out functions to sep. script
#'  Removed unnecessary commented out lines prev for testing
#'  Adding extra breaks for clarity re: save
#' -----------

source('src functions/src_modelfxns.R')

library(cowplot) #for plotting all
library(grid) #for text grobs
library(gridExtra) #for plot in grid format
library(ggplot2) #to save as fig

#Plots----
ACurve<-function(){
  y_ticksa <- seq(0, 4000, by = 1000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(200, 800), ylim = c(500, 4000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Anoxypristis cuspidata", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksa, labels = y_ticksa, cex.axis = 1.2)
  curve(AFariaL, add=TRUE, lwd=2, lty=2, col='orange')
  curve(AFariaP, add=TRUE, lwd=2, col='yellow')
  curve(APev, add=TRUE, lwd=2, lty=2, 
        col=rgb(0.2,0.4,0.1,0.7))
  curve(AWhittyL, add=TRUE, lwd=2, lty=2, col='dark blue')
  curve(AWhittyQ, add=TRUE, lwd=2, col='light blue')
  legend("bottomright",   # Position
         inset = 0.05, # Distance from the margin as a fraction of the plot region
         legend = c("Faria (2007) Linear", 
                    "Faria (2007) Power", 
                    "Peverell (2010)", 
                    "Whitty et al. (2014) Ratio", 
                    "Whitty et al. (2014)"),
         lty = c(2, 1, 2, 2, 1),
         col = c('orange', 
                 'yellow', 
                 rgb(0.2,0.4,0.1,0.7), 
                 'darkblue',
                 'lightblue'),
         theme(legend.text=element_text(size=12)))
}
CCurve<-function(){
  y_ticksc <- seq(0, 5000, by = 2000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(150, 800), ylim = c(750, 5000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Pristis clavata", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksc, labels = y_ticksc, cex.axis = 1.2)
  curve(CFariaL, add = TRUE, col='orange', lwd=2, lty=2)
  curve(CFariaP, add=TRUE, lwd=2, col='yellow')
  curve(CPev, add=TRUE, lwd=2, lty =2, col=rgb(0.2,0.4,0.1,0.7))
  curve(CThor, add=TRUE, lwd=2, col='plum')
  curve(CWhittyL, add=TRUE, lwd=2, lty=2, col='dark blue')
  curve(CWhittyP, add=TRUE, lwd=2, col='light blue')
  legend("bottomright",   # Position
         inset = 0.05, # Distance from the margin as a fraction of the plot region
         legend = c("Thorburn et al. (2008)"),
         lty = 1,
         col = c('plum'),
         theme(legend.text=element_text(size=12)))
}
PCurve<-function(){
  y_ticksp <- seq(0, 6000, by = 2000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(150, 1200), ylim = c(750, 6000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Pristis pristis", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksp, labels = y_ticksp, cex.axis = 1.2)
  curve(PFariaL, add =TRUE, lwd=2, lty=2, col='orange')
  curve(PFariaP, add=TRUE, lwd=2, col='yellow')
  curve(PMorg, add=TRUE, lwd=2, lty=2, col='red')
  curve(PPev, add=TRUE, col=rgb(0.2,0.4,0.1,0.7), 
        lwd=2, lty=2)
  curve(PThorF, add=TRUE, lwd=2, col='magenta')
  curve(PThorM, add=TRUE, lwd=2, col='purple')
  curve(PWhittyL, add=TRUE, lwd=2, lty=2, col='dark blue')
  curve(PWhittyP, add=TRUE, lwd=2, col='light blue')
  legend("bottomright",   # Position
         inset = 0.05,
         legend = c("Morgan et al. (2011)",
                    "Thorburn et al. (2007) Female",
                    "Thorburn et al. (2007) Male"),
         lty = c(2, 1, 1),
         col = c('red', 'magenta'),
         theme(legend.text=element_text(size=12)))
}
ZCurve<-function(){
  y_ticksz <- seq(0, 7000, by = 2000)  # Adjust the upper limit as needed
  plot(NULL, xlim = c(250, 1800), ylim = c(750, 7000), yaxt = "n",
       ylab = '', xlab = '',
       main = "Pristis zijsron", font.main = 3,
       cex.main = 1.25, cex.axis = 1.2)
  axis(2, at = y_ticksz, labels = y_ticksz, cex.axis = 1.2)
  curve(ZFariaL, add = TRUE, lwd=2, lty=2, col='orange')
  curve(ZFariaP, add=TRUE, lwd=2, col='yellow')
  curve(ZLearF, add=TRUE, lwd=2, col='darkgreen')
  curve(ZLearM, add=TRUE, lwd=2, col='lightgreen')
  curve(ZPev, add=TRUE, 
        col=rgb(0.2,0.4,0.1,0.7), lwd=2, lty=2)
  curve(ZWhittyL, add=TRUE, lwd=2, lty=2, col='darkblue')
  curve(ZWhittyP, add=TRUE, lwd=2, col='lightblue')
  legend("bottomright",   # Position
         inset = 0.05, # Distance from the margin as a fraction of the plot region
         legend = c("Lear et al. (2023) Female",
                    "Lear et al. (2023) Male"),
         lty = c(1, 1),
         col =  c('darkgreen', 'lightgreen'),
         theme(legend.text=element_text(size=12)))
}

##Check----
ACurve()
CCurve()
PCurve()
ZCurve()

##Together----
PubsC <- cowplot::plot_grid(
  ACurve,
  CCurve,
  PCurve,
  ZCurve,
  labels = c('a)', 'b)', 'c)', 'd)'),
  label_size= 15,
  label_fontface = "plain",
  hjust = -6,
  #vjust = 1,
  ncol = 2
)

x.grob <- textGrob("Standard Rostrum Length (SRL) (mm)", 
                   gp = gpar(fontsize=16),
                   vjust = -1)

y.grob <- textGrob("Total Length (TL) (mm)", 
                   gp = gpar(fontsize=16),
                   rot = 90,
                   vjust = .5)

BoxCurves <- grid.arrange(arrangeGrob(PubsC), left = y.grob, bottom = x.grob)

##Save Fig 3----
ggsave(
  "Fig3.png", #save as tiff for hires
  plot = BoxCurves,
  device = NULL,
  path = 'figs/',
  scale = 1,
  width = 12,
  height = 11,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
