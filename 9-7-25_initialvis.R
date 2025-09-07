# create an initial raw data visualization to look at sample size. for the purpose of 
# determining if it is neccessary to do any molecular sexing of samples
library(ggplot2)
vis1 <- ggplot(initial_vis, aes(x = avgC_capture_day, y = bleed1_final_cort, color = sex)) +
  geom_point(alpha = 0.5) +     
  coord_cartesian(ylim = c(0, 105)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(
    values = c("Male" = "#1F77B4", "Female" = "#AEC7E8")
  ) +
  theme_minimal()

ggsave("9-7-25_rawdata.png", plot = vis1, width = 8, height = 6, dpi = 300)
