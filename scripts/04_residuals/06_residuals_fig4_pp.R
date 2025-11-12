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

##Base Plots----

plevs <- c("Whitty Linear",
  "Thorburn F",
  "Peverell",
  "Morgan",
  "Faria Linear",
  "Whitty Power",
  "Thorburn M",
  "Faria Power")

pbarlabs <- c('WR', 'TF', 'P', 'M', 'L', 'W', 'TM', 'FP')

pplots <- base_plots(data = dfPR, pubs = plevs, ages = all_ages)

barpp <- pplots[[1]]
dotpp <- pplots[[2]]

#clean em up!!

#Bar----
BarPR <- barpp +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values =  all_age_cols) + 
  scale_x_discrete(labels = pbarlabs) 

#Dot----
PPDP <- dotpp +
  scale_color_manual(labels = c('Faria (2007) Linear',
                              'Faria (2007) Power', 
                              'Morgan et al. (2011)',
                              'Peverell (2010)',
                              'Thorburn et al. (2007) - Female',
                              'Thorburn et al. (2007) - Male',
                              'Whitty et al. (2014) Ratio',
                              'Whitty et al. (2014) Power'),
                     values = c('orange',
                                'yellow',
                                'red',
                                rgb(0.2,0.4,0.1,0.7),
                                'magenta',
                                'purple',
                                'darkblue',
                                'lightblue')) +
  geom_vline(xintercept = c(760, 1200, 2700, 3000, 6400),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(800, 1240, 2740, 3040, 6440),
  #          y = c(45, 45, 45, 45, 45),
  #          size = c(3.5,3.5,3.5,3.5,3.5),
  #          angle = 90,
  #          vjust = -1) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((760+1200)/2, (1200+2700)/2, (2700+3000)/2, (3000+6400)/2),
  #          y = c(52, 52, 52, 52),
  #          size = c(4,4,4,4),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          angle = 0,
  #          vjust = 1)) +
  geom_segment(x = 760, y = 54.5, xend = 1200, yend = 54.5,
               colour = "#5ab4ac",
               size = 3) +
  geom_segment(x = 1200, y = 54.5, xend = 2700, yend = 54.5,
               colour = "#f5f5f5",
               size = 3) +
  geom_segment(x = 2700, y = 54.5, xend = 3000, yend = 54.5,
               colour = "lightgrey",
               size = 3) +
  geom_segment(x = 3000, y = 54.5, xend = 6400, yend = 54.5,
               colour = "#d8b365",
               size = 3) +
  theme(legend.position = "bottom")

##Pub Section Pic----
cowplot::plot_grid(BarPR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   PPDP + theme(plot.title = element_blank()),
                   labels = "", ncol = 1, rel_heights = c(1,2), align ="v")

saveRDS(BarPR, "ppfig4c.rds")
saveRDS(PPDP, "ppfig4d.rds")

ggsave(
  "Fig4cd.png", #change to tiff for pub
  plot = last_plot(),
  device = NULL,
  path = "figs/fig4",
  scale = 1,
  width = 12,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
