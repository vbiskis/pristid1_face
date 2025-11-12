# Details ----
#' src_mainplots.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Output: Function to visualise Figs 4/5
#' Dependencies: N/A
#' *Changes from original:
#' Pulled repetitive content from graphical parameters
#' -----------

library(pacman)
pacman::p_load("ggplot2", "ggpattern", "cowplot")

all_ages <- c('YOY', 'juvenile', 'sub-adult', 'adult')
all_age_cols <- c("#5ab4ac", "#f5f5f5", "56B4E9", "#d8b365")

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

theme_set(theme(plot.title = element_text(size = 14, face = "italic"),
                legend.key = element_rect(fill = "white"),
                axis.text = element_text(size = 11),
                legend.text = element_text(size = 11),
                legend.title = element_text(size = 12),
                axis.title = element_text(size = 12)))

base_plots <- function(data, pubs, ages) {
  
  # Bar plot
  bar_data <- data_summary(data, "Residuals_SRL", c("Publication", "Age_Class"))
  bar_data$Age_Class <- factor(bar_data$Age_Class, levels = ages)
  
  bar <- ggplot(bar_data, aes(x = factor(Publication, level = pubs),
                              y = Residuals_SRL, fill = Age_Class)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    geom_errorbar(aes(ymin = Residuals_SRL - sd, ymax = Residuals_SRL + sd),
                  width = 0.2, position = position_dodge(0.9)) +
    labs(x = "Model", y = "Residuals (mm)") +
    theme_minimal()
  
  # Dot plot
  dot <- ggplot(data, aes(x = TLTrue, y = StResS, color = Publication)) +
    geom_point(size = 0.75) +
    geom_hline(yintercept = c(-5, 0, 5),
               linetype = c('dotted', 'longdash', 'dotted'),
               color = c('grey40', 'black', 'grey40')) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -5.2, ymax = 5.2,
             alpha = 0.1, fill = 'grey40') +
    stat_smooth(method = lm, se = FALSE) +
    labs(x = "Total Length (mm)", y = "Percent Error (%)", color = "Model") +
    theme_minimal()
  
  return(list(bar, dot))
}

#then can modify!