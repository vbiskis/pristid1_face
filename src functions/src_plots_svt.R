# Details ----
#' src_plotting.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Output: Function to compare model success w TRL vs SRL
#' Dependencies: N/A
#' *Changes from original:
#' Pulled repetitive content from graphical parameters
#' -----------

library(pacman)
pacman::p_load("ggplot2", "ggpattern", "cowplot")

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

stacked_bar <- function(data, varname, y_title, y_max = 47, 
                               pub_order = NULL, pub_labels = NULL,
                               model_types = c(Lin = "stripe", Pow = "none"),
                               title_text = "") {

  # Create stacked bar plot for net percent error
  
  # data: dataframe (dfPR, dfCR, etc.)
  # varname: column to plot ("StNetResS" or "StNetResT")
  # y_max: y-axis limit
  # pub_order: vector of model names in ORDER (if NULL, uses default sort)
  # pub_labels: vector of labels for x-axis (with expressions for bold/italic)
  
  # Summarize data
  plot_data <- data_summary(data, varname = varname,
                            groupnames = c("Publication", "Maturity", "ModelT"))
  plot_data$Maturity <- as.factor(plot_data$Maturity)
  
  # For SRL, sort by the variable
  if (is.null(pub_order)) {
    pub_order <- unique(plot_data$Publication[order(plot_data[[varname]], decreasing = TRUE)])
  }
  
  # If no labels specified, use publication names as-is (gross)
  if (is.null(pub_labels)) {
    pub_labels <- pub_order
  }
  
  # Create plot
  pattern_labels <- names(model_types)
  pattern_labels <- gsub("Lin", "Linear", pattern_labels)
  pattern_labels <- gsub("Pow", "Power", pattern_labels)
  pattern_labels <- gsub("Quad", "Quadratic", pattern_labels)
  
  # Create plot
  p <- ggplot(plot_data[order(plot_data$Maturity, decreasing = TRUE), ],
              aes(x = factor(Publication, level = pub_order),
                  y = .data[[varname]],
                  fill = factor(Maturity, levels = c("Mature", "Immature")),
                  pattern = ModelT)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_bar_pattern(stat = "identity", position = "stack",
                     color = "black", pattern_fill = "black",
                     pattern_angle = 45, pattern_density = 0.09,
                     pattern_spacing = 0.05, pattern_key_scale_factor = 0.6) +
    scale_pattern_manual(values = model_types,
                         labels = pattern_labels) +
    geom_text(aes(label = paste(round(.data[[varname]], 1), "±", round(sd, 1))),
              position = position_stack(), hjust = 1.2, size = 3, 
              fontface = 'bold', color = "white") +
    labs(title = title_text, 
         x = "Publication", 
         y = y_title, 
         pattern = "Model Type") +
    theme_minimal() +
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill = guide_legend(override.aes = list(pattern = "none"))) +
    theme(legend.position = c(0.82, 0.25)) +
    coord_flip() +
    ylim(0, y_max) +
    scale_fill_manual(name = "Maturity",
                      labels = c('Mature', 'Immature'),
                      values = c("grey40", "grey70"))
  
  # Add custom labels if provided
  if (!is.null(pub_labels)) {
    p <- p + scale_x_discrete(labels = pub_labels)
  }
  
  return(p)
}

