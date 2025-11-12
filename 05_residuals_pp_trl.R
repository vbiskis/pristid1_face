# Details ----
#' 05_residuals_pp_trl.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: TRL vs SRL input in model success
#' Dependencies: src fxn
#' *Changes from original:
#'  Separating scripts, automated graphs
#' -----------

source('src_plots_svt.R')
pacman::p_load("dplyr", "tidyverse", "broom")

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv) 

## Side quest: TRL vs SRL----
dfPR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>% #interesting - just Morgan and Faria
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      paired = TRUE,
                      conf.level = 0.99)))

# define models and order----
p_order <- c("Thorburn F", "Whitty Linear", "Peverell", "Morgan",
             "Faria Linear", "Whitty Power", "Thorburn M", "Faria Power")

p_labels <- c(
  expression(paste(bold("Thorburn et al. (2008) - Female"))),
  expression(paste(bold("Whitty et al. (2014) - Ratio"))),
  "Peverell (2010)",
  expression(paste(italic("*Morgan et al. (2011)"))),
  "Faria et al. (2007) - Linear",
  "Whitty et al. (2014) - Power",
  "Thorburn et al. (2008) - Male",
  "Faria et al. (2007) - Power"
)

# get the figs----
StBarPRS <- stacked_bar(dfPR, varname = "StNetResS", y_max = 35,
                               y_title = "Net Percent Error",
                               pub_order = p_order, 
                               pub_labels = p_labels,
                               title_text = "SRL Input")

StBarPRT <- stacked_bar(dfPR, varname = "StNetResT", 
                        y_title = "Net Percent Error",
                        pub_order = p_order, 
                        pub_labels = p_labels,
                        title_text = "TRL Input")

cowplot::plot_grid(StBarPRS + theme(axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    legend.position = "none"),
                   StBarPRT,
                   labels = "auto", ncol = 1, align ="v",
                   rel_heights = c(.85,1))

ggsave(
  "PrissTvS.tiff",
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
