# plot: sectoral value added per capita over gdp per capita ----
df_plot <- filter(df, spatial %in% g20) %>% select(spatial, temporal,
                                                    va_agr_pc, va_ind_pc,
                                                    va_ser_pc, gdp_pc)

df_plot <- df_plot[complete.cases(df_plot), ]

df_plot <- melt(df_plot, id.vars = c("spatial", "temporal", "gdp_pc"))

# order countries according to their 2013 GDP levels
df_plot_recent <- filter(df_plot, temporal == 2013, variable == "va_ind_pc") %>%
                    arrange(desc(gdp_pc))

df_plot$spatial <- ordered(df_plot$spatial, levels = df_plot_recent$spatial)

ggplot() +
  geom_point(data = df_plot, aes(x = gdp_pc, y = value, colour = variable)) +
  # geom_hline(yintercept = 10) +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom") +
  ylab("") +
  xlab("") +
  facet_wrap( ~ spatial, scales = "free", nrow = 4)
ggsave(filename = "plots/va_sec_pc_over_gdp_pc.png", height = 20, width = 16, units = "cm")

# plot growth rates over GDP per capita ---
df_plot <- select(df, spatial, temporal, va_agr_pc_gr, va_ind_pc_gr, va_ser_pc_gr, gdp_pc, gdp_pc_gr) %>%
             filter(spatial %in% g20, gdp_pc_gr >= 0)

plot_alpha = 0.5

ggplot() +
  theme_bw() +
  ylab("sectoral growth rate") +
  # agriculture
  geom_point(data = df_plot, aes(x = gdp_pc, y = va_agr_pc_gr), colour = "green",
             alpha = plot_alpha) +
  geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_agr_pc_gr), method = lm, colour = "green") +
  # industry
  # geom_point(data = df_plot, aes(x = gdp_pc, y = va_ind_pc_gr), colour = "blue",
  #            alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_ind_pc_gr), method = lm, colour = "blue") +
  # services
  # geom_point(data = df_plot, aes(x = gdp_pc, y = va_ser_pc_gr), colour = "red",
  #            alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_ser_pc_gr), method = lm, colour = "red")
ggsave(filename = "plots/va_sec_pc_gr_over_gdp_pc.png", width = 14, height = 14,
       units = "cm")