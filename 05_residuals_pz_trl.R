# Details ----
#' 04_residuals_pc_trl.R
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
dfZR %>%  #ttest - calc vs true SRL
  group_by(Publication) %>% #pev no diff
  do(tidy(wilcox.test(.$NetRes_S,
                      .$NetRes_T,
                      mu = 0,
                      paired = TRUE,
                      conf.level = 0.99)))

# define models and order----
z_order <- c("Faria Power", "Lear Female", "Whitty Linear",
             "Faria Linear", "Lear Male", "Peverell", "Whitty Power")

z_labels <- c("Faria et al. (2007) - Power",
  "Lear et al. (2023) - Female",
  expression(paste(bold("Whitty et al. (2014) - Ratio"))),
  expression(paste(bold("Faria et al. (2007) - Linear"))),
  "Lear et al. (2023) - Male",
  "Peverell (2010)",
  "Whitty et al. (2014) - Power"
)

# get the figs----
StBarZRS <- stacked_bar(dfZR, varname = "StNetResS", y_max = 22,
                        y_title = "Net Percent Error",
                        pub_order = z_order, 
                        pub_labels = z_labels,
                        title_text = "SRL Input")

StBarZRT <- stacked_bar(dfZR, varname = "StNetResT", y_max = 25,
                        y_title = "Net Percent Error",
                        pub_order = z_order, 
                        pub_labels = z_labels,
                        title_text = "TRL Input")

cowplot::plot_grid(StBarZRS + theme(axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    legend.position = "none"),
                   StBarZRT,
                   labels = "auto", ncol = 1, align ="v",
                   rel_heights = c(.85,1))

ggsave(
  "ZijTvS.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = 10,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
