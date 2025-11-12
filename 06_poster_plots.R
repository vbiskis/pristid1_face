# Details ----
#' 06_poster_plots.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Keep code from plot params that were printed for AMSA 
#' Dependencies: plotting src code
#' *Changes from original:
#'  N/A, just separated
#' -----------

#not for publishing
source('src_stats_helper.R')

###AMSAPoster----
PZDPoster <- ggplot(dfZR, mapping=aes(x=TLTrue, 
                                      y=StResS, 
                                      color=factor(Publication, 
                                                   levels = c("Faria Power",
                                                              "Lear Female",
                                                              "Whitty Linear",
                                                              "Faria Linear",
                                                              "Peverell",
                                                              "Lear Male",
                                                              "Whitty Power")))) +
  coord_cartesian(xlim =c(600, 5500)) +
  geom_point() +
  geom_hline(yintercept = c(-5, 0, 5),
             linetype = c('dotted', "longdash", 'dotted'),
             color = c('grey40', 'black', 'grey40')) +
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = -5.2, ymax= 5.2, alpha = 0.1, fill = 'grey40') +
  scale_color_manual(name = "Model",
                     labels=c('Faria (2007) Power', 
                              "Lear et al. (2023) - Female",
                              'Whitty et al. (2014) Ratio',
                              'Faria (2007) Linear', 
                              'Peverell (2010)',
                              "Lear et al. (2023) - Male",
                              'Whitty et al. (2014) Power'),
                     values = c('yellow',
                                'darkgreen', 
                                'darkblue',
                                'orange',
                                rgb(0.2,0.4,0.1,0.7), 
                                'lightgreen', 
                                'lightblue')) +
  stat_smooth(aes(x=TLTrue, y=StResS, color=Publication), 
              method = lm, se = FALSE) +
  geom_vline(xintercept = c(760, 1300, 3000, 3800, 5400),
             linetype = c('solid', 'solid', 'dotdash', 'solid', 'solid')) +
  annotate(geom = "text",
           label = c('Birth Size', 'One Year', ' ', 'Maturity', 'Max Size'),
           x = c(760, 1300, 3000, 3800, 5400),
           y = c(30, 30, 40, 30, 30),
           angle = 90,
           vjust = -1) +
  geom_segment(x = 760, y = 40, xend = 1300, yend = 40,
               colour = "#5ab4ac",
               linewidth = 2) +
  geom_segment(x = 1300, y = 40, xend = 3000, yend = 40,
               colour = "#f5f5f5",
               linewidth = 2) +
  geom_segment(x = 3000, y = 40, xend = 3800, yend = 40,
               colour = "lightgrey",
               linewidth = 2) +
  geom_segment(x = 3800, y = 40, xend = 5400, yend = 40,
               colour = "#d8b365",
               linewidth = 2) +
  annotate(geom = "label",
           label = c('YOY', 'Juvenile', 'Sub-Adult', 'Adult'),
           x = c((760+1300)/2, (1300+3000)/2, (3000+3800)/2, (3800+5400)/2),
           y = c(42, 42, 42, 42),
           fill = c("#5ab4ac", "#f5f5f5", "lightgrey", "#d8b365"),
           colour = c("white", "black", "black", "white"),
           angle = 0,
           vjust = 1) +
  theme(plot.title = element_text(face = "italic"),
        legend.key = element_rect(fill = "white"),
        legend.position = 'bottom') +
  labs(title = "Pristis zijsron",
       x = bquote("Total Length (mm)"),
       y = bquote("Percent Error"),
       color = "Publication")
PZDPoster

##ResBar----

ResBar <- cowplot::plot_grid(BarAR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             BarCR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             BarPR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             BarZR + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           legend.position = "none"), 
                             labels = "AUTO", 
                             ncol = 1, align ="v")
ResBar

ggsave(
  "ResBer.tiff",
  plot = ResBar,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 15,
  height = 9,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

##PooledDot----
BigDots <- cowplot::plot_grid(ACDP, PCDP,
                              PPDP, PZDP,
                              labels = "AUTO", ncol = 2, align ="v")

ggsave(
  "BigDots.tiff",
  plot = BigDots,
  device = NULL,
  path = NULL,
  scale = 1,
  width = 18,
  height = 8,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)