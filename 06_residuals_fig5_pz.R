# Details ----
#' 06_residuals_fig5_pz.R
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

zlevs <- c("Faria Linear",
           "Faria Power",
           "Peverell",
           "Whitty Linear",
           "Lear Female",
           "Lear Male",
           "Whitty Power")

zbarlabs <- c("FL", "FP", "P", "WR", "LF", "LM", "W")

zplots <- base_plots(data = dfZR, pubs = zlevs, ages = all_ages)

barpz <- zplots[[1]]
dotpz <- zplots[[2]]

#clean em up!!

#Bar----
BarZR <- barpz +
  scale_fill_manual(name = "Size Class",
                    labels = c('YOY', 'Juvenile', 'Sub-adult', 'Adult'), 
                    values = all_age_cols) + 
  scale_x_discrete(labels = zbarlabs) 

#Dot----
PZDP <- dotpz +
  coord_cartesian(xlim =c(800, 5400)) +
  scale_color_manual(labels=c('Faria (2007) Linear',
                              'Faria (2007) Power', 
                              "Lear et al. (2023) - Female",
                              "Lear et al. (2023) - Male",
                              'Peverell (2010)',
                              'Whitty et al. (2014) Ratio',
                              'Whitty et al. (2014) Power'),
                     values = c('orange',
                                'yellow',
                                rgb(0.2,0.4,0.1,0.7),
                                'darkgreen', 
                                'lightgreen',  
                                'darkblue',
                                'lightblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(760, 1300, 3000, 3800, 5400),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  # annotate(geom = "text",
  #          label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
  #          x = c(760, 1300, 3000, 3800, 5400),
  #          y = c(27, 27, 27, 27, 27),
  #          size = c(4,4,4,4,4),
  #          angle = 90,
  #          vjust = -1) +
  geom_segment(x = 760, y = 34, xend = 1300, yend = 34,
               colour = "#5ab4ac",
               linewidth = 3) +
  geom_segment(x = 1300, y = 34, xend = 3000, yend = 34,
               colour = "#f5f5f5",
               linewidth = 3) +
  geom_segment(x = 3000, y = 34, xend = 3800, yend = 34,
               colour = "lightgrey",
               linewidth = 3) +
  geom_segment(x = 3800, y = 34, xend = 5400, yend = 34,
               colour = "#d8b365",
               linewidth = 3) +
  # annotate(geom = "label",
  #          label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
  #          x = c((760+1300)/2, (1300+3000)/2, (3000+3800)/2, (3800+5400)/2),
  #          y = c(47, 47, 47, 47),
  #          size = c(4,4,4,4),
  #          fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
  #          colour = c("white", "black", "black", "white"),
  #          angle = 0,
  #          vjust = 1) +
  theme( legend.position = "bottom")

##Pub Section Pic----
cowplot::plot_grid(BarZR + theme(plot.title = element_blank(),
                                 legend.position = "none"),
                   PZDP + theme(plot.title = element_blank()),
                   labels = "", ncol = 1, rel_heights = c(1,2), align ="v")

saveRDS(BarZR, "pzfig5a.rds")
saveRDS(PZDP, "pzfig5b.rds")

ggsave(
  "ZijjyData.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 12,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
