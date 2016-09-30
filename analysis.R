library(IDA)
library(reshape2)
library(ggplot2)

rm(list = ls())

# create output directories
dir.create("plots", showWarnings = FALSE)

# prepare data ----
# vector of variables
# ATTENTION: when adding variables here, you have to make sure that the corresponding units are included in the filtering of 'idata' below
vars <- c("gdp" = "GDP",
          "va_agr" = "Value Added|Agriculture",
          "va_ind" = "Value Added|Industry",
          "va_man" = "Value Added|Manufacturing",
          "va_ser" = "Value Added|Services",
          "pop" = "Population",
          "area" = "Area")

# G20 memberstates (ATTENTION: other EU members not yet present, also: no
# sectoral data present for Canada)
g20 <- c("DEU", "ARG", "AUS", "BRA", "CHN", "FRA", "GBR", "IND", "IDN", "ITA", "JPN", "CAN", "MEX", "RUS", "SAU", "ZAF", "KOR", "TUR", "USA")

wdi <- filter(idata, source_id == "WDI_2015",
              variable %in% vars, unit %in% c("bn USD2005/yr", "million", "km2"))

for (var in vars){
  wdi <- rename_var(wdi, var, names(vars)[vars == var])
}

# store units
units <- distinct(wdi, variable, unit)

wdi <- dcast(wdi, spatial + temporal ~ variable)

# compute per capita values
wdi <- mutate(wdi, gdp_pc = gdp / pop,
                   va_agr_pc = va_agr / pop,
                   va_ind_pc = va_ind / pop,
                   va_man_pc = va_man / pop,
                   va_ser_pc = va_ser / pop,
                   va_agrind_pc = va_agr_pc + va_ind_pc,
                   pop_dens = pop / area
                  )

# calculate growth rates
wdi <- group_by(wdi, spatial) %>%
          mutate(va_agr_pc_gr = lag(va_agr_pc, n = 0, order_by = temporal) / lag(va_agr_pc, n = 1, order_by = temporal) - 1,
                 va_ind_pc_gr = lag(va_ind_pc, n = 0, order_by = temporal) / lag(va_ind_pc, n = 1, order_by = temporal) - 1,
                 va_agrind_pc_gr = lag(va_agrind_pc, n = 0, order_by = temporal) / lag(va_agrind_pc, n = 1, order_by = temporal) - 1,
                 va_ser_pc_gr = lag(va_ser_pc, n = 0, order_by = temporal) / lag(va_ser_pc, n = 1, order_by = temporal) - 1,
                 gdp_pc_gr = lag(gdp_pc, n = 0, order_by = temporal) / lag(gdp_pc, n = 1, order_by = temporal) - 1) %>%
          ungroup()

# plot: sectoral value added per capita over gdp per capita ----
df_plot <- filter(wdi, spatial %in% g20) %>% select(spatial, temporal,
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
df_plot <- select(wdi, spatial, temporal, va_agr_pc_gr, va_ind_pc_gr, va_ser_pc_gr, gdp_pc, gdp_pc_gr) %>%
             filter(spatial %in% g20, gdp_pc_gr >= 0)

plot_alpha = 0.5

ggplot() +
  theme_bw() +
  ylab("sectoral growth rate") +
  # agriculture
  geom_point(data = df_plot, aes(x = gdp_pc, y = va_agr_pc_gr), colour = "green",
             alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_agr_pc_gr), method = lm, colour = "green") +
  # industry
  geom_point(data = df_plot, aes(x = gdp_pc, y = va_ind_pc_gr), colour = "blue",
             alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_ind_pc_gr), method = lm, colour = "blue") +
  # services
  geom_point(data = df_plot, aes(x = gdp_pc, y = va_ser_pc_gr), colour = "red",
             alpha = plot_alpha) +
  # geom_smooth(data = df_plot, aes(x = gdp_pc, y = va_ser_pc_gr), method = lm, colour = "red")
ggsave(filename = "plots/va_sec_pc_gr_over_gdp_pc.png", width = 14, height = 14,
       units = "cm")

# add recession dummy
wdi$recession <- NA
wdi[wdi$gdp_pc_gr > 0 & !(is.na(wdi$gdp_pc_gr)), "recession"] <- 0
wdi[wdi$gdp_pc_gr <= 0 & !(is.na(wdi$gdp_pc_gr)), "recession"] <- 1

# estimation
model_agr <- lm(va_agr_pc_gr ~ gdp_pc + I(gdp_pc^2) + factor(spatial) +
                  recession + pop_dens, data = filter(wdi, spatial %in% g20))
model_ind <- lm(va_ind_pc_gr ~ gdp_pc + I(gdp_pc^2) + factor(spatial) +
                  recession + pop_dens, data = filter(wdi, spatial %in% g20))
model_agrind <- lm(va_agrind_pc_gr ~ gdp_pc + I(gdp_pc^2) + factor(spatial) +
                  recession, data = filter(wdi, spatial %in% g20))
model_agrind_nofe <- lm(va_agrind_pc_gr ~ gdp_pc + I(gdp_pc^2) +
                  recession, data = filter(wdi, spatial %in% g20))

