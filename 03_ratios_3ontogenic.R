# Details ----
#' 03_ratios_3ontogenic.R
#' Paper: [Fine-tuning established morphometric models...]
#' DOI: 10.1111/csp2.13308
#' Author: Nikki Biskis
#' Date: 2025-11-12
#' Purpose: Isometry tests
#' Output: Appendix S4
#' *Changes from original:
#'  split into multiple scripts
#' -----------

source('src_stats_helper.R')

theme_set(theme_classic() +
            theme(strip.text = element_text(face = "italic", size = 12), 
                  axis.text = element_text(face = "italic", size = 11),
                  axis.title = element_text(size = 12),
                  legend.position = "none"))

IsoS<- ggplot(df1, 
              aes(x=log(TL_curve), y=log(SRL), color = Species)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_regline_equation(label.x = 6.5,
                        label.y = 7, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 6.5,
                        label.y = 6.7, aes(label = ..rr.label..)) +
  scale_x_continuous(limits = c(6,9)) +
  scale_y_continuous(limits = c(4.5,7.5)) +
  facet_grid(~ Species) +
  labs(y = "log SRL", x = 'log TL') +
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

IsoT<- ggplot(df1, 
              aes(x=log(TL_curve), y=log(TRL), color = Species)) + 
  geom_point() +
  stat_smooth(method="lm") + 
  stat_regline_equation(label.x = 6.5,
                        label.y = 7, aes(label = ..eq.label..)) +
  stat_regline_equation(label.x = 6.5,
                        label.y = 6.7, aes(label = ..rr.label..)) +
  scale_x_continuous(limits = c(6,9)) +
  scale_y_continuous(limits = c(4.5,7.5)) +
  facet_grid(~ Species) +
  labs(y = "log TRL", x = 'log TL')+
  scale_color_manual(values = c("blue4","gold2", "red3", "darkgreen")) 

IsoAllo <- cowplot::plot_grid(IsoS + theme(axis.title.x = element_blank()),
                              IsoT + theme(axis.title.x = element_blank()),
                              labels = c('a)',
                                         'b)',
                                         'c)',
                                         'd)'),
                              label_fontface = 'plain',
                              ncol = 1)
IsoAllo

ggsave(
  "IsoAllo.Tiff",
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
