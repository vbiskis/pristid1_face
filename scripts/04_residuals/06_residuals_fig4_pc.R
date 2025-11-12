# Details ----
#' 06_residuals_fig4_pc.R
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

source('src functions/src_plots_main.R')

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv)

clevs <- c("Whitty Power",
           "Faria Power",
           "Faria Linear",
           "Thorburn",
           "Whitty Linear",
           "Peverell")

cbarlabs <- c('W', 'FP', 'FL', 'T', 'WR', 'P')

cplots <- base_plots(data = dfCR, pubs = clevs, ages = all_ages)

barpc <- cplots[[1]]
dotpc <- cplots[[2]]

#clean em up!!

#Bar----
BarCR <- barpc +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = all_age_cols) + 
  scale_x_discrete(labels = cbarlabs) 

#Dot----
PCDP <- dotpc +
  coord_cartesian(xlim =c(500, 5050),
                  ylim = c(-30, 45)) +
  scale_color_manual(labels=c('Faria (2007) Linear',
                              'Faria (2007) Power', 
                              'Peverell (2010)',
                              'Thorburn et al. (2008)',
                              'Whitty et al. (2014) Ratio',
                              'Whitty et al. (2014) Power'),
                     values = c('orange',
                                'yellow',
                                rgb(0.2,0.4,0.1,0.7),
                                'plum',
                                'darkblue',
                                'lightblue')) +
  geom_vline(xintercept = c(810, 1100, 1950, 2600, 5080),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(860, 1140, 1990, 2640, 5120),
  #          y = c(38, 38, 38, 38, 38),
  #          size = c(3.5,3.5,3.5,3.5,3.5),
  #          angle = 90,
  #          vjust = -1) +
  geom_segment(x = 810, y = 48, xend = 1100, yend = 48,
               colour = "#5ab4ac",
               size = 3) +
  geom_segment(x = 1100, y = 48, xend = 1950, yend = 48,
               colour = "#f5f5f5",
               size = 3) +
  geom_segment(x = 1950, y = 48, xend = 2600, yend = 48,
               colour = "lightgrey",
               size = 3) +
  geom_segment(x = 2600, y = 48, xend = 5080, yend = 48,
               colour = "#d8b365",
               size = 3) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((810+1100)/2, (1100+1950)/2, (1950+2600)/2, (2600+5080)/2),
  #          y = c(57.5, 57.5, 57.5, 57.5),
  #          size = c(4,4,4,4),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          angle = 0,
  #          vjust = 1) +
  theme(legend.position = "bottom")

##Pub Section Pic----
cowplot::plot_grid(BarCR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   PCDP + theme(plot.title = element_blank()),
                   labels = "", ncol = 1, rel_heights = c(1,2), align ="v")

saveRDS(BarCR, "pcfig4a.rds")
saveRDS(PCDP, "pcfig4b.rds")

ggsave(
  "Fig4ab.png", #change to tiff for pub
  plot = last_plot(),
  device = NULL,
  path = "figs/fig4",
  scale = 1,
  width = 10,
  height = 7.5,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
