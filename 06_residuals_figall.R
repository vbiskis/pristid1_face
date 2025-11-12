# Details ----
#' 06_residuals_figall.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Print Figs 4/5 
#' Dependencies: N/A
#' *Changes from original:
#'  Pull in as RDS -> add to final figures
#' -----------

library(cowplot)
library(ggplot2)

clavplota <- readRDS("pcfig4a.rds")
pristplota <- readRDS("ppfig4a.rds")
clavplotb <- readRDS("pcfig4b.rds")
pristplotb <- readRDS("ppfig4b.rds")

#need legend in between!
leg1 <- get_legend(pristplota)
leg2 <- get_legend(pristplotb)

#align em
plotsL <- align_plots(clavplota + theme(axis.title.x = element_blank(),
                                     legend.position = "none"),
                      clavplotb + theme(plot.title = element_blank(),
                                   legend.position = "none"),
                      align = 'v', axis = 'l')

plotsR <- align_plots(pristplota + theme(axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     legend.position = "none"),
                      pristplotb + theme(plot.title = element_blank(),
                                   axis.title.y = element_blank(),
                                   legend.position = "none"),
                      align = 'v', axis = 'l')

#leg in middle
legR <- plot_grid(leg1,
                  NULL,
                  leg2,
                  rel_widths = c(1.1, 0.05, 1.4),
                  align = "h",
                  nrow = 1)

#top row bars
topPCP <- plot_grid(plotsL[[1]], 
                    plotsR[[1]], 
                    labels = c('a)', 'c)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.3),
                    align = 'h')

#bottom row dots
botPCP <- plot_grid(plotsL[[2]],
                    plotsR[[2]], 
                    labels = c('b)', 'd)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.3))

cowplot::plot_grid(topPCP,
                   botPCP,
                   #legR,
                   rel_heights = c(1,2),
                   align = 'h',
                   ncol = 1)

ggsave(
  "Fig4.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)

#too big
#print legends sep and paste in

##Fig5----
anoxplota <- readRDS("acfig5a.rds")
zijplota <- readRDS("pzfig5a.rds")
anoxplotb <- readRDS("acfig5b.rds")
zijplotb <- readRDS("pzfig5b.rds")

#need legend in between!
leg3 <- get_legend(zijplota)
leg4 <- get_legend(zijplotb)

#align em
plotsL1 <- align_plots(anoxplota + theme(axis.title.x = element_blank(),
                                        legend.position = "none"),
                       anoxplotb + theme(plot.title = element_blank(),
                                        legend.position = "none"),
                      align = 'v', axis = 'l')

plotsR1 <- align_plots(zijplota + theme(axis.title.x = element_blank(),
                                         axis.title.y = element_blank(),
                                         legend.position = "none"),
                       zijplotb + theme(plot.title = element_blank(),
                                         axis.title.y = element_blank(),
                                         legend.position = "none"),
                      align = 'v', axis = 'l')

#leg in middle
legF <- plot_grid(leg3,
                  NULL,
                  leg4,
                  rel_widths = c(1.1, 0.05, 1.4),
                  align = "h",
                  nrow = 1)

#top row bars
topACP <- plot_grid(plotsL1[[1]], 
                    plotsR1[[1]], 
                    labels = c('a)', 'c)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.5),
                    align = 'h')

#bottom row dots
botPZP <- plot_grid(plotsL1[[2]],
                    plotsR1[[2]], 
                    labels = c('b)', 'd)'),
                    label_size = 13,
                    label_fontface = "plain",
                    rel_widths = c(1, 1.5))

cowplot::plot_grid(topACP,
                   botPZP,
                   #legF,
                   rel_heights = c(1,2),
                   align = 'h',
                   ncol = 1)

ggsave(
  "Fig5.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 8,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
