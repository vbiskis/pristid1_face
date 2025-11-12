# Details ----
#' MorphoHelper.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-11
#' Purpose: Loads data, packages + 3 stats functions
#' Output: All df in GlobalEnv.
#' *Changes from original:
#'  pacman used to load in packages
#'  purrr used to bring in data from sep tabs in excel + convert to df
#' -----------

library(pacman)
pacman::p_load("purrr", "tidyverse", "dplyr", "broom",
               "ggplot2", "ggpubr", "vtable", "rstatix", "car")

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv) #hell ya

#function for checking normality
check_norm <- function(dat, cols = NULL,
                       plots = TRUE, summary = TRUE) {
  
  df_name <- deparse(substitute(dat))
  
  # If cols is NULL, check all numeric columns
  if(is.null(cols)) {
    cols <- names(dat)[sapply(dat, is.numeric)]
  }
  
  if(length(cols) == 1) {   # Single column
    col_data <- dat[[cols]]
    
    if(plots) {
      print(ggqqplot(col_data, main = paste(df_name, "-", cols)))
      print(ggdensity(col_data, main = paste(df_name, "-", cols)))
    }
    
    test <- shapiro.test(col_data)
    cat("\n", df_name, "-", cols, ": p =", round(test$p.value, 4),
        ifelse(test$p.value > 0.05, "✓ Normal", "✗ Non-normal"), "\n")
    return(invisible(test))
  }
  
  results <- data.frame()  # Multiple columns
  for(col in cols) {
    if(col %in% names(dat)) {
      col_data <- dat[[col]]
      
      # Create plots for each column if requested
      if(plots) {
        print(ggqqplot(col_data, main = paste(df_name, "-", col)))
        print(ggdensity(col_data, main = paste(df_name, "-", col)))
      }
      
      test <- shapiro.test(col_data)
      results <- rbind(results, data.frame(
        Column = col,
        W = round(test$statistic, 4),
        p = round(test$p.value, 4),
        Normal = ifelse(test$p.value > 0.05, "✓", "✗")
      ))
    }
  }
  
  cat("\n=== Normality Check:", df_name, "===\n")
  print(results)
  cat("Normal:", sum(results$Normal == "✓"), "/", nrow(results), "\n")
  
  invisible(results)
}

#do multiple paired tests simultaneously
pair_test <- function(df, x, y, 
                      test = "t", paired = TRUE, 
                      group_by = NULL) {
  
  if (!is.null(group_by)) {
    df <- df %>% group_by(across(all_of(group_by)))
  }
  
  #also the option for no-para
  test_fxn <- if (test == "wilcox") wilcox.test else t.test
  
  df %>%
    do(tidy(test_fxn(pull(., !!sym(x)),
                     pull(., !!sym(y)),
                     mu = 0,
                     alternative = "two.sided",
                     paired = paired,
                     exact = FALSE,
                     conf.level = 0.99))) %>%
    {if (!is.null(group_by)) ungroup(.) else .}
}

#to test multiple anovas / t tests simultaneously
asso_test <- function(dvs, data, test_var = "Scale", #wrote for ImgJ but can be for whatever
         test = "aov", separate = TRUE) {
  
  results <- list() #to store and report
  
  if (separate) { # Default = run each test_var separately
    for (dv in dvs) {
      for (tv in test_var) {
        formula <- as.formula(paste(dv, "~", tv))
        
        if (test == "t") {
          results[[paste(dv, "~", tv)]] <- t.test(formula, data = data)
        } else if (test == "wilcox") {
          results[[paste(dv, "~", tv)]] <- wilcox.test(formula, data = data)
        } else if (test == "kw") {
          results[[paste(dv, "~", tv)]] <- kruskal.test(formula, data = data)
        } else {
          results[[paste(dv, "~", tv)]] <- summary(aov(formula, data = data))
        }
      }
    }
  } else { # Combine all test_vars into one formula
    for (dv in dvs) {
      formula <- as.formula(paste(dv, "~", paste(test_var, collapse = " + ")))
      results[[paste(dv, "~", paste(test_var, collapse = "+"))]] <- summary(aov(formula, data = data))
    }
  }
  return(results)
}

