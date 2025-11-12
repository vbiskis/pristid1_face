# Details ----
#' 05_residuals_pc_trl.R
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
dfCR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>% #just Thorburn (TRL input)
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      paired = TRUE,
                      conf.level = 0.99)))

# define models and order----
c_order <- c("Faria Power", "Whitty Power", 
             "Faria Linear", "Thorburn",  
             "Whitty Linear", "Peverell")

c_labels <- c(expression(paste(bold("Faria et al. (2007) - Power"))),
              expression(paste(bold("Whitty et al. (2014) - Power"))),
              "Faria et al. (2007) - Linear",
              "Thorburn et al. (2008)",
              "Whitty et al. (2014) - Ratio",
              "Peverell (2010)")

# get the figs----
StBarCRS <- stacked_bar(dfCR, varname = "StNetResS", y_max = 47,
                        y_title = "Net Percent Error",
                        pub_order = c_order, 
                        pub_labels = c_labels,
                        title_text = "SRL Input")

StBarCRT <- stacked_bar(dfCR, varname = "StNetResT", y_max = 47, 
                        y_title = "Net Percent Error",
                        pub_order = c_order, 
                        pub_labels = c_labels,
                        title_text = "TRL Input")

cowplot::plot_grid(StBarCRS + theme(axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    legend.position = "none"),
                   StBarCRT,
                   labels = "auto", ncol = 1, align ="v",
                   rel_heights = c(.85,1))

ggsave(
  "App_PCInput.png",
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
