# Details ----
#' src_resid_plots.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Output: Function to visualise residuals against functions
#' Dependencies: N/A
#' *Changes from original:
#' Pulled repetitive content from graphical parameters
#' -----------

library(pacman)
pacman::p_load("ggplot2", "ggpattern", "cowplot")

theme_set(theme(plot.title = element_text(size = 14, face = "italic"),
                  legend.key = element_rect(fill = "white"),
                  axis.text = element_text(size = 11),
                  legend.text = element_text(size = 11),
                  legend.title = element_text(size = 12),
                  axis.title = element_text(size = 12)))

resid_plots <- function(data, models, y_max = 3800) {
  
  plots <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    
    plots[[i]] <- ggplot(data, aes(x = SRL, y = TLCorr)) +
      geom_smooth(aes_string(x = "SRL", y = model$tl_col), 
                  method = model$method, se = FALSE, color = "lightgrey") +
      geom_segment(aes_string(xend = "SRL", yend = model$tl_col), alpha = 0.2) +
      ylim(0, y_max) +
      geom_point(aes_string(color = model$resid_col)) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red") +
      geom_point(aes(y = TLCorr), shape = 1) +
      labs(
        x = "SRL", 
        y = "Total Length", 
        title = model$name,
        color = "Residuals"
      ) +
      theme_bw()
  }
  
  return(plots)
}
