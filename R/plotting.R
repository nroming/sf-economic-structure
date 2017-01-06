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

df_plot$variable <- gsub("va_agr_pc", "Agriculture", df_plot$variable, fixed = TRUE)
df_plot$variable <- gsub("va_ind_pc", "Industry", df_plot$variable, fixed = TRUE)
df_plot$variable <- gsub("va_ser_pc", "Services", df_plot$variable, fixed = TRUE)

ggplot() +
  geom_point(data = df_plot, aes(x = gdp_pc, y = value, colour = variable)) +
  # geom_hline(yintercept = 10) +
  theme_bw(base_size = 11) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  scale_colour_brewer(type = "qual", palette = 2) +
  ylab("Sectoral value added per capita [k USD2005]") +
  xlab("GDP per capita [k USD2005]") +
  facet_wrap( ~ spatial, scales = "free")
ggsave(filename = "output/figures/va_sec_pc_over_gdp_pc_by_country.png", height = 17, width = 17,
       units = "cm")

ggplot() +
  geom_point(data = df_plot, aes(x = gdp_pc, y = value, colour = variable)) +
  # geom_hline(yintercept = 10) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  scale_colour_brewer(type = "qual", palette = 2) +
  ylab("Sectoral value added per capita") +
  xlab("GDP per capita")
ggsave(filename = "output/figures/va_sec_pc_over_gdp_pc.png", height = 16, width = 28,
       units = "cm")

# plot growth rates over GDP per capita ---
df_plot <- select(df, spatial, temporal, va_agr_pc_gr, va_ind_pc_gr,
                  va_ser_pc_gr, gdp_pc, gdp_pc_gr) %>%
  filter(spatial %in% g20, gdp_pc_gr >= 0)

plot_alpha = 0.5

ggplot() +
  theme_bw() +
  ylab("sectoral growth rate") +
  # agriculture
  geom_point(data = df_plot, aes(x = gdp_pc, y = va_agr_pc_gr),
             colour = "green", alpha = plot_alpha) +
  geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_agr_pc_gr), method = lm,
              colour = "green") +
  # industry
  # geom_point(data = df_plot, aes(x = gdp_pc, y = va_ind_pc_gr), colour = "blue",
  #            alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_ind_pc_gr), method = lm, colour = "blue") +
  # services
  # geom_point(data = df_plot, aes(x = gdp_pc, y = va_ser_pc_gr), colour = "red",
  #            alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_ser_pc_gr), method = lm, colour = "red")
  ggsave(filename = "output/figures/va_sec_pc_gr_over_gdp_pc.png", width = 14, height = 14,
         units = "cm")


# absolute value added per sector for all countries ----
tmp_plot_scen <- filter(result, scenario == "SSP2", temporal >= 2015, temporal <= 2100) %>%
  select(scenario, spatial, temporal, gdp, va_agr, va_ind, va_ser)

tmp_plot_hist <- filter(result, scenario == "history", temporal < 2015) %>%
  select(scenario, spatial, temporal, gdp, va_agr, va_ind, va_ser)

tmp_plot <- rbind(tmp_plot_hist, tmp_plot_scen)

rm(tmp_plot_scen, tmpe_plot_hist)

tmp_plot <- melt(tmp_plot, id.vars = c("scenario", "spatial", "temporal"))

countries <- sort(c(country_ref, as.character(unique(result$spatial))))

num_pages <- length(countries) %/% 20

start_country <- 1

pdf("output/figures/country_results_total.pdf")
for(i in seq(num_pages)){

  tmp_plot_area <- filter(tmp_plot, variable != "gdp",
                          spatial %in% countries[start_country:(start_country + 19)])
  tmp_plot_line <- filter(tmp_plot, variable == "gdp",
                          spatial %in% countries[start_country:(start_country + 19)])

  start_country <- start_country + 20

  p <- ggplot()
  p <- p + geom_area(data = tmp_plot_area, aes(x = temporal, y = value, fill=variable))
  p <- p + geom_line(data = tmp_plot_line, aes(x = temporal, y = value))
  p <- p + ylab("bn USD2005/yr")
  p <- p + xlab("")
  p <- p + theme_bw(base_size = 9)
  p <- p + theme(legend.position = "none")
  p <- p + facet_wrap(~spatial, scales = "free")
  print(p)
}
dev.off()

# G20 only
tmp_plot_scen <- filter(result, scenario == "SSP2", temporal >= 2015,
                        temporal <= 2100, spatial %in% g20) %>%
  select(scenario, spatial, temporal, gdp, va_agr, va_ind, va_ser)

tmp_plot_hist <- filter(result, scenario == "history", temporal < 2015,
                        spatial %in% g20) %>%
  select(scenario, spatial, temporal, gdp, va_agr, va_ind, va_ser)

tmp_plot <- rbind(tmp_plot_hist, tmp_plot_scen)

rm(tmp_plot_scen, tmpe_plot_hist)

tmp_plot <- melt(tmp_plot, id.vars = c("scenario", "spatial", "temporal"))

tmp_plot_area <- filter(tmp_plot, variable != "gdp")
tmp_plot_line <- filter(tmp_plot, variable == "gdp")

ggplot() +
  geom_area(data = tmp_plot_area, aes(x = temporal, y = value, fill=variable)) +
  geom_line(data = tmp_plot_line, aes(x = temporal, y = value)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  ylab("bn USD2005/yr") +
  xlab("") +
  ggtitle("Total sectoral value added") +
  theme_bw(base_size = 9) +
  theme(legend.position = "none") +
  facet_wrap(~spatial, scales = "free")
ggsave("output/figures/country_results_total_G20.png", width = 24, height = 12, units = "cm")

## GDP and sectoral value added per capita for all countries ----
tmp_plot_scen <- filter(result, scenario == "SSP2", temporal >= 2015, temporal <= 2100) %>%
  select(scenario, spatial, temporal, gdp_pc, va_agr_pc, va_ind_pc, va_ser_pc)

tmp_plot_hist <- filter(result, scenario == "history", temporal < 2015) %>%
  select(scenario, spatial, temporal, gdp_pc, va_agr_pc, va_ind_pc, va_ser_pc)

tmp_plot <- rbind(tmp_plot_hist, tmp_plot_scen)

rm(tmp_plot_scen, tmpe_plot_hist)

tmp_plot <- melt(tmp_plot, id.vars = c("scenario", "spatial", "temporal"))

countries <- sort(as.character(unique(result$spatial)))

num_pages <- length(countries) %/% 20

start_country <- 1

pdf("output/figures/country_results_capita.pdf")
for(i in seq(num_pages)){

  tmp_plot_area <- filter(tmp_plot, variable != "gdp_pc",
                          spatial %in% countries[start_country:(start_country + 19)])
  tmp_plot_line <- filter(tmp_plot, variable == "gdp_pc",
                          spatial %in% countries[start_country:(start_country + 19)])

  start_country <- start_country + 20

  p <- ggplot()
  p <- p + geom_area(data = tmp_plot_area, aes(x = temporal, y = value, fill=variable))
  p <- p + geom_line(data = tmp_plot_line, aes(x = temporal, y = value))
  p <- p + ylab("bn USD2005/yr")
  p <- p + xlab("")
  p <- p + theme_bw(base_size = 9)
  p <- p + theme(legend.position = "none")
  p <- p + facet_wrap(~spatial, scales = "free")
  print(p)
}
dev.off()

# G20 only
tmp_plot_scen <- filter(result, scenario == "SSP2", temporal >= 2015,
                        temporal <= 2100, spatial %in% g20) %>%
  select(scenario, spatial, temporal, gdp_pc, va_agr_pc, va_ind_pc, va_ser_pc)

tmp_plot_hist <- filter(result, scenario == "history", temporal < 2015,
                        spatial %in% g20) %>%
  select(scenario, spatial, temporal, gdp_pc, va_agr_pc, va_ind_pc, va_ser_pc)

tmp_plot <- rbind(tmp_plot_hist, tmp_plot_scen)

rm(tmp_plot_scen, tmp_plot_hist)

tmp_plot <- melt(tmp_plot, id.vars = c("scenario", "spatial", "temporal"))

tmp_plot_area <- filter(tmp_plot, variable != "gdp_pc")
tmp_plot_line <- filter(tmp_plot, variable == "gdp_pc")

ggplot() +
  geom_area(data = tmp_plot_area, aes(x = temporal, y = value, fill=variable)) +
  geom_line(data = tmp_plot_line, aes(x = temporal, y = value)) +
  scale_fill_brewer(type = "qual", palette = 2) +
  ylab("k USD2005/yr") +
  xlab("") +
  ggtitle("Per capita value added") +
  theme_bw(base_size = 9) +
  theme(legend.position = "none") +
  facet_wrap(~spatial, scales = "free")
ggsave("output/figures/country_results_capita_G20.png", width = 24, height = 12, units = "cm")

# absolute value added per sector for all regions ----
tmp_plot_scen <- filter(result_reg, scenario == "SSP2", temporal >= 2015, temporal <= 2100) %>%
  select(scenario, spatial, temporal, gdp, va_agr, va_ind, va_ser)

tmp_plot_hist <- filter(result_reg, scenario == "history", temporal < 2015) %>%
  select(scenario, spatial, temporal, gdp, va_agr, va_ind, va_ser)

tmp_plot <- rbind(tmp_plot_hist, tmp_plot_scen)

rm(tmp_plot_scen, tmpe_plot_hist)

tmp_plot <- melt(tmp_plot, id.vars = c("scenario", "spatial", "temporal"))

tmp_plot_area <- filter(tmp_plot, variable != "gdp")
tmp_plot_line <- filter(tmp_plot, variable == "gdp")

ggplot() +
  geom_area(data = tmp_plot_area, aes(x = temporal, y = value, fill=variable)) +
  geom_line(data = tmp_plot_line, aes(x = temporal, y = value)) +
  ylab("bn USD2005/yr") +
  geom_vline(xintercept = 2015) +
  xlab("") +
  theme_bw(base_size = 9) +
  theme(legend.position = "none") +
  facet_wrap(~ spatial, scales = "free")
ggsave(filename = "output/figures/regions_results_total.pdf", width = 20, height = 28, units = "cm")

# regional shares
tmp_scen <- filter(result_reg, temporal >= 2015, temporal <= 2100, scenario == "SSP2") %>%
    select(scenario, temporal, spatial, share_agr, share_ind,
          share_ser)

tmp_hist <- filter(result_reg, temporal < 2015, scenario == "history") %>%
    select(scenario, temporal, spatial, share_agr, share_ind,
          share_ser)

tmp <- rbind(tmp_hist, tmp_scen)

rm(tmp_hist, tmp_scen)

tmp <- melt(tmp, id.vars = c("scenario", "spatial", "temporal"))

tmp <- rename_var(tmp, "share_agr", "Agriculture")
tmp <- rename_var(tmp, "share_ind", "Industry")
tmp <- rename_var(tmp, "share_ser", "Services")

ggplot() +
  geom_area(data = tmp, aes(x = temporal, y = value, fill = variable)) +
  ylab("Sectoral shares") +
  xlab("") +
  geom_vline(xintercept = 2015) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none") +
  scale_fill_brewer(type = "qual", palette = 6) +
  facet_wrap(~ spatial)
ggsave(filename = "output/figures/regions_sectoral_shares.png", width = 20, height = 28,
       units = "cm")

# historical FE intensities by sector ----
countries <- c("GBR", "FRA", "USA", "ITA", "ESP", "CHN")

iea <- filter(idata, source_id == "IEA_2014",
              spatial %in% countries,
              # temporal <= 2012,
              temporal >= 1990,
              variable %in% paste("Final Energy",
                                   rep(c("Agriculture", "Industry", "Services"), 3),
                                   rep(c("Solids", "Liquids", "Gases", "Heat", "Electricity"), 3), sep = "|"))

iea <- group_by(iea, source_id, model, scenario, spatial, temporal, variable, unit) %>%
  summarise(value = sum(value, na.rm = TRUE))

iea$variable <- gsub("Final Energy|", "", iea$variable, fixed = TRUE)
iea$variable <- gsub("Agriculture|", "agr_", iea$variable, fixed = TRUE)
iea$variable <- gsub("Industry|", "ind_", iea$variable, fixed = TRUE)
iea$variable <- gsub("Services|", "ser_", iea$variable, fixed = TRUE)
iea$variable <- gsub("Solids", "solid", iea$variable, fixed = TRUE)
iea$variable <- gsub("Liquids", "liquid", iea$variable, fixed = TRUE)
iea$variable <- gsub("Gases", "gas", iea$variable, fixed = TRUE)
iea$variable <- gsub("Heat", "heat", iea$variable, fixed = TRUE)
iea$variable <- gsub("Electricity", "elec", iea$variable, fixed = TRUE)

iea <- dcast(iea, scenario + spatial + temporal ~ variable)

tmp <- filter(df_hist, spatial %in% countries) %>%
  select(scenario, spatial, temporal, va_ind, va_agr, va_ser)

iea <- inner_join(iea, tmp)

iea <- mutate(iea, fei_agr_elec = agr_elec/va_agr,
              fei_agr_solid = agr_solid/va_agr,
              fei_agr_liquid = agr_liquid/va_agr,
              fei_agr_gas = agr_gas/va_agr,
              fei_agr_heat = agr_heat/va_agr,
              fei_agr_tot = (agr_elec + agr_solid + agr_liquid + agr_gas + agr_heat)/va_agr,

              fei_ind_elec = ind_elec/va_ind,
              fei_ind_solid = ind_solid/va_ind,
              fei_ind_liquid = ind_liquid/va_ind,
              fei_ind_gas = ind_gas/va_ind,
              fei_ind_heat = ind_heat/va_ind,
              fei_ind_tot = (ind_elec + ind_solid + ind_liquid + ind_gas + ind_heat)/va_ind,


              fei_ser_elec = ser_elec/va_ser,
              fei_ser_solid = ser_solid/va_ser,
              fei_ser_liquid = ser_liquid/va_ser,
              fei_ser_gas = ser_gas/va_ser,
              fei_ser_heat = ser_heat/va_ser,
              fei_ser_tot = (ser_elec + ser_solid + ser_liquid + ser_gas + ser_heat)/va_ser
)

iea <- select(iea, spatial, temporal, fei_agr_elec:fei_ser_tot)

iea <- melt(iea, id.vars = c("spatial", "temporal"))

iea <- filter(iea, !is.na(value))
iea <- filter(iea, value != 0)

iea$variable <- gsub("fei_", "", iea$variable, fixed = TRUE)

iea$sector <- NA
iea[grepl("agr", iea$variable), "sector"] <- "Agriculture"
iea[grepl("ind", iea$variable), "sector"] <- "Industry"
iea[grepl("ser", iea$variable), "sector"] <- "Services"

iea$variable <- gsub("agr_", "", iea$variable, fixed = TRUE)
iea$variable <- gsub("ind_", "", iea$variable, fixed = TRUE)
iea$variable <- gsub("ser_", "", iea$variable, fixed = TRUE)

iea$variable <- gsub("elec", "Electricity", iea$variable, fixed = TRUE)
iea$variable <- gsub("gas", "Gases", iea$variable, fixed = TRUE)
iea$variable <- gsub("heat", "Heat", iea$variable, fixed = TRUE)
iea$variable <- gsub("liquid", "Liquids", iea$variable, fixed = TRUE)
iea$variable <- gsub("solid", "Solids", iea$variable, fixed = TRUE)
iea$variable <- gsub("tot", "Total", iea$variable, fixed = TRUE)

iea <- mutate(iea, value = value * 1e6)

iea_tot <- filter(iea, variable == "Total")
iea <- filter(iea, variable != "Total")

ggplot() +
  geom_line(data = iea, aes(x = temporal, y = value, group = variable,
                            colour = variable), size = 1) +
  geom_line(data = iea_tot, aes(x = temporal, y = value), size = 1, colour = "black") +
  theme_bw(base_size = 9) +
  scale_colour_brewer(type = "qual", palette = 2) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  xlab("") +
  ylab("GJ/bn USD2005") +
  facet_grid(spatial ~ sector, scales = "free")
ggsave(file.path("output/figures", "FEI.png"), width = 18, height = 27, units = "cm")

# AR5 final energy demand pathways ----
tmp <- filter(idata, source_id == "AR5",
              scenario %in% c("LIMITS-RefPol", "LIMITS-RefPol-450"),
              variable %in% c("Final Energy|Electricity",
                              "Final Energy|Gases",
                              "Final Energy|Heat",
                              "Final Energy|Liquids",
                              "Final Energy|Solids"),
              temporal %in% c(2010, 2100),
              model != "AIM-Enduse[Backcast] 1.0")

tmp2010 <- filter(tmp, temporal == 2010)
tmp2100 <- filter(tmp, temporal == 2100)

tmp2010 <- filter(tmp2010, scenario == "LIMITS-RefPol") %>%
  mutate(scenario = "2010")

tmp <- rbind(tmp2010, tmp2100)

tmp <- change_scenario(tmp, "LIMITS-RefPol", "BAU")
tmp <- change_scenario(tmp, "LIMITS-RefPol-450", "2°C")

tmp$variable <- gsub("Final Energy|", "", tmp$variable, fixed = TRUE)
tmp$scenario <- ordered(tmp$scenario, levels = c("2010", "BAU", "2°C"))

ggplot() +
  geom_bar(data = tmp, aes(x = scenario, y = value, group = variable, fill = variable), stat = "identity") +
  theme_bw(base_size = 12) +
  ylab(unique(tmp$unit)) +
  xlab("") +
  ggtitle("Global final energy demand in 2100 with and without climate policy") +
  facet_wrap(~ model) +
  scale_fill_brewer(type = "qual", palette = 6)
ggsave("output/figures/AR5_FE.png", width = 28, height = 15, units = "cm")
