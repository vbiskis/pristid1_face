# Details ----
#' 04_residuals_table.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Output: Table 2
#' *Changes from original:
#'  just separated into its own script
#' -----------
source('src_stats_helper.R')
library(flextable)

FinalTab <- df2 %>% select(Species, Publication, Model_Type, Maturity, TLTrue, SRL_Calc, NetRes_S, slogSRL, logNetSRL, NPESRL, PESRL)
OrgAll <- FinalTab %>% 
  group_by(Species, Publication) %>%
  summarise(                                                         # only the below summary columns will be returned
    N = sum(t.test(TLTrue, SRL_Calc, paired = TRUE, conf.level = 0.95)$parameter,1),
    same_pred_p = round(wilcox.test(TLTrue, SRL_Calc, paired = TRUE, conf.level = 0.95)$p.value, digits = 2), #ttest attempt
    same_pred_t = round(wilcox.test(TLTrue, SRL_Calc, paired = TRUE, conf.level = 0.95)$statistic, digits = 2),
    net_mean = round(mean(NetRes_S, na.rm = T), digits = 2),     # mean net res
    net_se = round(sd(NetRes_S, na.rm = T)/sqrt(n()), digits = 2),  # standard deviation net res
    gradient = round(lm(PESRL ~ TLTrue)$coefficients[2], digits = 4),
    gradient_se = round(summary(lm(PESRL ~ TLTrue))$coefficients[4], digits = 4),
    pe_mean_j = round(mean(NPESRL[Maturity == "Immature"], na.rm = T), digits = 1),     # mean net res
    pe_se_j = round(sd(NPESRL[Maturity == "Immature"]/sqrt(n()), na.rm = T), digits = 1),
    pe_mean_ad = round(mean(NPESRL[Maturity == "Mature"], na.rm = T), digits = 1),     # mean net res
    pe_se_ad = round(sd(NPESRL[Maturity == "Mature"]/sqrt(n()), na.rm = T), digits = 1),
    model = first(Model_Type))
OrgAll

flexALL <- flextable(OrgAll)
flexALL %>%
  autofit()
flexALL <- flexALL %>% 
  set_header_labels(Species = "", # Rename the columns in original header row
                    Publication = "Model", 
                    N = "N",                  
                    same_pred_p = "p",
                    same_pred_t = "W",
                    net_mean = "Mean",
                    net_se = "SE",
                    gradient = "Gradient",
                    gradient_se = "SE",
                    pe_mean_j = "Mean",
                    pe_se_ju = "SE",
                    pe_mean_ad = "Mean",
                    pe_se_ad = "SE",
                    model = "Model Type")%>% 
  add_header_row(
    top = TRUE,    # New header goes on top of existing header row
    values = c("", # will be for species
               "", # for model
               "", # for n
               "1. Calculated = Predicted", "",
               "2. Low Net Residuals (mm)", "",         
               "3. Zero Gradient", "",
               "4a. Low Error Juveniles (%)", "",
               "4b. Low Error Adults (%)", "",
               "") #will be for model type
    ) %>% 
  colformat_num(j = c(6,7), digits = 1) %>%
  colformat_num(j = 8, digits = 3) %>%
  merge_at(i = 1, j = 4:5, part = "header") %>% # Horizontally merge columns 3 to 4 in new header row
  merge_at(i = 1, j = 6:7, part = "header") %>%  
  merge_at(i = 1, j = 8:9, part = "header") %>%  
  merge_at(i = 1, j = 10:11, part = "header") %>%  
  merge_at(i = 1, j = 12:13, part = "header")     

border_style = officer::fp_border(color="black", width=1)

flexALL <- flexALL %>%
  border_remove() %>%  
  # add horizontal lines via a pre-determined theme setting
  theme_booktabs() %>% 
  # add vertical lines to separate Recovered and Died sections
  flextable::align(align = "center", part = "all") %>%
  merge_v(j = 1, target = NULL, part = "body", combine = FALSE) %>%
  vline(part = "all", j = 3, border = border_style) %>%   
  vline(part = "all", j = 5, border = border_style) %>%   
  vline(part = "all", j = 7, border = border_style) %>%   
  vline(part = "all", j = 9, border = border_style) %>%   
  vline(part = "all", j = 13, border = border_style) %>%      
  fontsize(i = 1, size = 11, part = "header") %>%   # adjust font size of header
  bold(i = 1, bold = TRUE, part = "header") %>%     # adjust bold face of header
  bg(i = 1:5, part = "body", bg = "gray95")  %>%
  bg(i = 6:11, part = "body", bg = "white")  %>%
  bg(i = 12:19, part = "body", bg = "gray95")  %>%
  bg(i = 20:25, part = "body", bg = "white")  %>%
  bg(j = 4:5, i = ~ same_pred_p >= 0.1, part = "body", bg = "palegreen3") %>% 
  bg(j = 6:7, i = ~ net_mean <= 100, part = "body", bg = "palegreen3") %>% 
  bg(j = 8:9, i = ~ abs(gradient) <= 0.002, part = "body", bg = "palegreen3") %>%
  bg(j = 10:11, i = ~ pe_mean_j <= 6.5, part = "body", bg = "palegreen3") %>%
  bg(j = 12:13, i = ~ pe_mean_ad <= 6.5, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = 2, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = 9, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = 26, part = "body", bg = "palegreen3") %>%
  bg(j = 14, i = ~ pe_mean_ad <= 6, part = "body", bg = "palegreen3") %>%
  italic(j = 1) %>%
  width(j=1, width = 1.2) %>% 
  width(j=2, width = 1.7) %>%
  width(j=4:12, width = 0.5)
flexALL

print(flexALL, preview = "docx") # Word document to modify!
save_as_image(flexALL, path = "bigtable.png")
