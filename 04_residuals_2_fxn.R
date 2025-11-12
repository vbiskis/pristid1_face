# Details ----
#' 04_residuals_2_fxn.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Input:  df + species
#' Output: One row per publication/model with all test stats
#' Dependencies: N/A
#' *Brand new
#' -----------

# we're going to make x 4 functions here to test each assumption
# and wrap in a single script!

library(pacman)
pacman::p_load("tidyverse", "dplyr", "broom")

all_morpho <- readRDS("morphodat.rds")
list2env(all_morpho, envir = .GlobalEnv) #hell ya

morpho_tests <- function(dat, sf_spec) {
  
  # Test 1---- 
  # RAW RESIDUALS - Wilcoxon test (Calc. vs True TL)
  
  test1 <- dat %>%
    group_by(Publication) %>%
    do(tidy(wilcox.test(.$TLTrue,
                        .$TLCalc_SRL,
                        mu = 0,
                        alt = "two.sided",
                        paired = TRUE,
                        conf.level = 0.95))) %>%
    ungroup() %>%
    select(Publication, p.value, statistic) %>%
    rename(same_p = p.value,
           same_t = statistic)
  
  # Test 2---- 
  # NET RESIDUALS - Lowest Mean Error (and SE)
  
  test2 <- dat %>%
    group_by(Publication) %>%
    summarise(
      N = n(),
      net_mean = round(mean(NetRes_S, na.rm = TRUE), 2),
      net_se = round(sd(NetRes_S, na.rm = TRUE) / sqrt(n()), 2),
      .groups = "drop"
    )
  
  # Test 3---- 
  # PERCENT ERROR - 0 Gradient (St.Res shouldn't change with increased TL)

  test3 <- dat %>%
    group_by(Publication) %>%
    do({
      model <- lm(StResS ~ TLTrue, data = .)
      summary_model <- summary(model)
      
      tibble(
        Publication = first(.$Publication),
        gradient = round(model$coefficients[2], 4),
        gradient_se = round(summary_model$coefficients[2, 2], 4)
      )
    }) %>%
    ungroup()
  
  # Test 4 ---- 
  # NET PERCENT ERROR - by Maturity (+ SE)

  test4 <- dat %>%
    group_by(Publication) %>%
    summarise(
      pe_mean_j = round(mean(StNetResS[Maturity == "Immature"], na.rm = TRUE), 1),
      pe_se_j = round(sd(StNetResS[Maturity == "Immature"], na.rm = TRUE) / sqrt(n()), 1),
      pe_mean_ad = round(mean(StNetResS[Maturity == "Mature"], na.rm = TRUE), 1),
      pe_se_ad = round(sd(StNetResS[Maturity == "Mature"], na.rm = TRUE) / sqrt(n()), 1),
      .groups = "drop"
    )
  
  # Now Merge Em----
  
  model_info <- dat %>%
    group_by(Publication) %>%
    summarise(mod_type = first(ModelT), .groups = "drop")
  
  combined <- test1 %>%
    left_join(test2, by = "Publication") %>%
    left_join(test3, by = "Publication") %>%
    left_join(test4, by = "Publication") %>%
    left_join(model_info, by = "Publication") %>%
    mutate(
      Species = sf_spec, #just for output
      same_p = round(same_p, 2),
      same_t = round(same_t, 2)
    ) %>%
    select(Species, Publication, N, everything())
  
  return(combined)
}
