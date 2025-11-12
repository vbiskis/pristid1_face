# pristid1_morpho

This repo contains R code re: the analyses for the published manuscript:

> Biskis, V. N., Townsend, K. A., Morgan, D. L., Lear, K. O., Holmes, B. J., & Wueringer, B. E. (2025). [Fine-tuning established morphometric models through citizen science data] *Conservation Science and Practice*, e13308;
(https://doi.org/10.1111/csp2.13308)

------------------------------------------------------------------------
## Author: Nikki Biskis

PhD Candidate UniSC | SARA
Date: 2025-11-11

------------------------------------------------------------------------

## Contents

pristid1_morpho

    └── scripts             <- code used in the analysis 
    └── functions           <- functions written by author VNB   
    └── figs                <- .png files of figures in main text   
        ├── fig4/fig5       <- a/b files separated, with legends
        ├── residual plots  <- alternative visuals of Fig 4/5
        ├── morpho input    <- added visuals of SRL vs TRL


## Notes

All code was initially written in October 2023.
Minor changes in figure output for publication in June 2024.
While the statistics have not changed, the structure was...lacking.
*(This was my first entirely-R analysis.)*

The following modifications were completed Nov 2025 for clarity/reproducibility:

1. Made multiple src codes
  + src_modelfxns = reorganised models from published literature
  + src_stats_helper = loads data and packages in
       - includes 3 check fxns to repeat everywhere (normality, paired tests, running multiple params)
  + src_morpho_tests = runs through all 4 testing criteria from manuscript on models (as in Table 2)
  + src_plots = x3 for drawing a repetitive graphs per species
       - main = bar + dot plots
       - svt = SRL vs TRL input
       - resid = visualisation of residual dist compared to graphs
  
2. Changed packages where relevant for efficiency
     - This is noted at the beginning of each script
     - Repetitive lines -> created a function / looped with *purrr*
    
3. Separated out lengthy code (e.g. published model functions).
     - Brought in where needed only, shortening script and run time
  
4. Made sure all scripts run independently...
     - Where a large object created (ggplot), saved as rds for later loading

## Questions & feedback?

Please submit an issue, or email your questions to Nikki Biskis: nikkibiskis@gmail.com

---
