# Details ----
#' 05_residuals_pc_app_plots.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Check trends with maturity, model type etc.
#' Dependencies: residual function + plotting src code
#' *Changes from original:
#'  Automation
#'  Look at all pubs/tests + a few postHoc tests/graphics
#' -----------

source('04_residuals_2_fxn.R')
source('src_plots_resid.R')

# Run it----
res_PC <- morpho_tests(dfCR, "P. clavata")

## Post-Hoc----
t.test(dfCT$SNStFL, dfCT$SNStFP,
       alt = "two.sided", conf.level = 0.95)

t.test(dfCT$SNStFP, dfCT$SNStWC,
       alt = "two.sided", conf.level = 0.95) #slightly less err in Whitty, not sig

t.test(dfCT$SNStWC, dfCT$SNStWL,
       alt = "two.sided", conf.level = 0.95)

## Interaction Plot----
interaction.plot(x.factor     = dfCR$ModelT,
                 trace.factor = dfCR$Maturity,
                 response     = dfCR$StNetResS,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

# power is the way to go
# always work best for juveniles

library(car)
modelCT = lm(StNetResS ~ ModelT + Maturity + ModelT:Maturity, data = dfCR)
Anova(modelCT, type = "II") #model yes - maturity no

## Appdx: Residual Plot----

pcmods <- list( #define pc model details
  list(
    name = "Peverell, 2010", tl_col = "TL_Pev_SRL", 
    resid_col = "ResidP", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Linear", tl_col = "TL_FL_SRL", 
    resid_col = "ResidFL", method = "lm"
  ),
  list(
    name = "Faria, 2007 - Power", tl_col = "TL_FP_SRL", 
    resid_col = "ResidFP", method = "loess"
  ),
  list(
    name = "Thorburn, 2008 - Power", tl_col = "TL_Thor_SRL", 
    resid_col = "ResidT", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Power", tl_col = "TL_WP_SRL", 
    resid_col = "ResidWC", method = "loess"
  ),
  list(
    name = "Whitty et al. 2014 - Ratio", tl_col = "TL_WL_SRL", 
    resid_col = "ResidWL", method = "lm"
  )
)

pc_plots <- resid_plots(dfCT, pcmods)

# Access individual plots
Ca <- pc_plots[[1]]
Cb <- pc_plots[[2]]
Cc <- pc_plots[[3]]
Cd <- pc_plots[[4]]
Ce <- pc_plots[[5]]
Cf <- pc_plots[[6]]

CResGALL <- cowplot::plot_grid(Cb, 
                               Cf + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Ca + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Cc,
                               Ce + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               Cd + theme(axis.text.y = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          axis.title.y = element_blank() ),
                               labels = "auto", ncol = 3, align ="v")

ggsave(
  "ClavataRes.tiff",
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
