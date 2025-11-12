# Details ----
#' 00_read_data.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-11
#' Purpose: Separates data from xlsx to list of all dfs
#' Output: morphodat.rds
#' 
#' *Changes from original:
#'  pacman used to load in packages
#'  purrr used to bring in data from sep tabs in excel 
#'  + convert to df
#' -----------

library(pacman)
pacman::p_load("readxl", "purrr")

file_path <- "Morpho-Master.xlsx"

#to save to enviro----
# walk2(
#   c('dfI', 'dfI1', 'dfI2', 'dfA', 'dfC', 'dfP', 'dfZ', 'df1', 'df2', 
#     'dfAT', 'dfAR', 'dfZT', 'dfZR', 'dfPT', 'dfPR', 'dfCT', 'dfCR'),
#   c('Image J Comp', 'IJ1', 'IJ2', 
#     'AC', 'PC', 'PP', 'PZ', 
#     'AllSpec', 'AllRes',
#     'AnoxyTest - ALL', 'Anoxy Resid', 
#     'ZijsronTest - ALL', 'Zijsron Resid',
#     'PristisTest - ALL', 'Pristis Resid', 
#     'ClavataTest - ALL', 'Clavata Resid'),
#   ~assign(.x, 
#           data.frame(read_xlsx(file_path, sheet = .y)), 
#           envir = .GlobalEnv)
# )

#but we want RDS----
all_dfs <- map2(
  c('dfI', 'dfI1', 'dfI2', 'dfA', 'dfC', 'dfP', 'dfZ', 'df1', 'df2', 
    'dfAT', 'dfAR', 'dfZT', 'dfZR', 'dfPT', 'dfPR', 'dfCT', 'dfCR'),
  c('Image J Comp', 'IJ1', 'IJ2', 'AC', 'PC', 'PP', 'PZ', 
    'AllSpec', 'AllRes', 'AnoxyTest - ALL', 'Anoxy Resid', 
    'ZijsronTest - ALL', 'Zijsron Resid', 'PristisTest - ALL', 
    'Pristis Resid', 'ClavataTest - ALL', 'Clavata Resid'),
  ~data.frame(read_xlsx(file_path, sheet = .y))
) %>% set_names(c('dfI', 'dfI1', 'dfI2', 'dfA', 'dfC', 'dfP', 'dfZ', 
                  'df1', 'df2', 'dfAT', 'dfAR', 'dfZT', 'dfZR', 
                  'dfPT', 'dfPR', 'dfCT', 'dfCR'))

# Save all dataframes as one RDS file
saveRDS(all_dfs, "morphodat.rds")
