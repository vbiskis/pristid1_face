# Details ----
#' 03_ratios_4extra1.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Extra trends not included in paper
#' Output: Density graphs, LJR and other ratios
#' *Changes from original:
#'  split into multiple scripts
#' -----------

source('src_stats_helper.R')

theme_set(theme_classic() +
            theme(axis.text.x = element_text(size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  legend.title = element_text(size = 14),
                  legend.text = element_text(face = "italic", size = 13)))

#DensityPlot
df1 %>%
  ggplot(aes(x=LJR_SRL, fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = "LJR/SRL")

df1 %>%
  ggplot(aes(x=SRL_TRL, fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = "SRL/TRL", y = "Density")

df1 %>%
  ggplot(aes(x=SRL_TL, fill=Species))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(x = "SRL/TL", y = "Density")

ggplot(df1, aes(x=Species, y=LJR_SRL, color=Species)) + 
  geom_boxplot() +
  facet_grid(~ Maturity) +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        strip.text.x = element_text(size = 14)) +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) +
  labs(y = "LJR/SRL")

ggsave(
  "LJRSRL.tiff",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)
