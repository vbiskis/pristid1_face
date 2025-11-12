# Details ----
#' 06_residuals_fig5_ac.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Construct visual of model params (low net res, percent err, gradient)
#' Output: rds file to manipulate in Fig 4/5
#' Dependencies: plotting src code
#' *Changes from original:
#'  Just the graphics, simplified with src code now
#' -----------

source('src_plots_main.R')
all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv)

alevs <- c("Whitty Linear",
           "Faria Linear",
           "Faria Power",
           "Peverell",
           "Whitty Quad")

abarlabs <- c("WR", "FL", "FP", "P", "W")

aages <- c('YOY',
           'juvenile',
           'adult')

aplots <- base_plots(data = dfAR, pubs = alevs, ages = aages)

barac <- aplots[[1]]
dotac <- aplots[[2]]

#clean em up!!

#Bar----
BarAR <- barac +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Adult'), 
                    values =  c("#5ab4ac", "#f5f5f5", "#d8b365")) + 
  scale_x_discrete(labels = abarlabs) 

##Dot----
ACDP <- dotac +
  coord_cartesian(xlim =c(550, 4100)) +
  scale_color_manual(labels=c('Faria (2007) Linear',
                              'Faria (2007) Power',
                              'Peverell (2010)',
                              'Whitty et al. (2014) Ratio',
                              'Whitty et al. (2014) Quadratic'),
                     values = c('orange',
                                'yellow',
                                rgb(0.2,0.4,0.1,0.7),
                                'darkblue',
                                'lightblue')) +
  geom_vline(xintercept = c(560, 1385, 1820, 2200, 4090),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text", label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(560, 1385, 1820, 2200, 4090),
  #          y = c(50, 50, 50, 50, 50),
  #          size = c(4,4,4,4,4), angle = 90, vjust = -1) +
  geom_segment(x = 560, y = 62, xend = 1385, yend = 62,
               colour = "#5ab4ac",
               size = 3) +
  geom_segment(x = 1385, y = 62, xend = 1820, yend = 62,
               colour = "#f5f5f5",
               size = 3) +
  geom_segment(x = 1820, y = 62, xend = 2200, yend = 62,
               colour = "lightgrey",
               size = 3) +
  geom_segment(x = 2200, y = 62, xend = 4090, yend = 62,
               colour = "#d8b365",
               size = 3) +
  # annotate(geom = "label", label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((560+1385)/2, (1385+1820)/2, (1820+2200)/2, (2200+4090)/2),
  #          y = c(75, 75, 75, 75),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          size = c(4,4,4,4), angle = 0, vjust = 1) +
  theme(legend.position = "bottom")

###Pub Section Pic----
cowplot::plot_grid(BarAR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   ACDP + theme(plot.title = element_blank()),
                   labels = "", ncol = 1, rel_heights = c(1,2), align ="v")

saveRDS(BarAR, "acfig5a.rds")
saveRDS(ACDP, "acfig5b.rds")

ggsave(
  "AnoxyData.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
