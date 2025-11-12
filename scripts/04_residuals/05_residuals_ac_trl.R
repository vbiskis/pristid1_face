# Details ----
#' 05_residuals_ac_trl.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: TRL vs SRL input in model success
#' Dependencies: src fxn
#' *Changes from original:
#'  Separating scripts, automated graphs
#' -----------

pacman::p_load("dplyr", "tidyverse", "broom")
source('src functions/src_plots_svt.R')

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv) 

## Side quest: TRL vs SRL----
dfAR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>% #pev and whitty linear no diff
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      paired = TRUE,
                      conf.level = 0.99)))

dfAR %>%  #not sig - except quadratic
  group_by(Publication) %>% 
  do(tidy(t.test(.$StResS,
                 .$StResT,
                 mu = 0,
                 paired = TRUE,
                 conf.level = 0.99)))

# define models and order----
a_labels <- c('Whitty et al. 2014 - L',
              'Faria et al. 2007 - L',
              'Peverell, 2010',
              'Faria et al. 2007 - P',
              'Whitty et al. 2014 - Q')

a_order <- c('Whitty Linear',
              'Faria Linear',
              'Peverell',
              'Faria Power',
              'Whitty Quad')

# get the figs----

StBarARS <- stacked_bar(dfAR, varname = "StNetResS", y_max = 52,
                        y_title = "Net Percent Error",
                        pub_order = a_order, 
                        pub_labels = a_labels,
                        model_types = c(Lin = "stripe", Pow = "none", Quad = "circle"),
                        title_text = "SRL Input")

StBarART <- stacked_bar(dfAR, varname = "StNetResT", y_max = 52,
                        y_title = "Net Percent Error",
                        pub_order = a_order, 
                        #pub_labels = a_labels,
                        model_types = c(Lin = "stripe", Pow = "none", Quad = "circle"),
                        title_text = "TRL Input")

cowplot::plot_grid(StBarARS + theme(axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    legend.position = "none"),
                   StBarART,
                   labels = "auto", ncol = 1, align ="v",
                   rel_heights = c(.85,1))

ggsave(
  "App_ACInput.png",
  plot = last_plot(),
  device = NULL,
  path = 'figs/morpho input/',
  scale = 1,
  width = 10,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  #dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
