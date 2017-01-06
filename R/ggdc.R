library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)

rm(list = ls())

options(scipen = 999)

ggdc <- read.dta(unzip("data/10sd_jan15_2014.zip", "10SD_jan15.dta", exdir = "cache"))

# make the ggdc NRmisc compatible ----
# drop columns
ggdc$Region <- NULL
ggdc$Regioncode <-  NULL

# rename columns
ggdc <- rename(ggdc, spatial = Country,
                     variable  = Variable,
                     temporal  = Year)

# determine covered sectors
sectors_10 <- names(ggdc)[4:13]
sectors_3 <- c("AGR", "IND", "SER")

# check if numbers add up to the given sum columm
check_sum = FALSE
if(check_sum){
  ggdc$SUM_check <- rowSums(ggdc[c("AGR", "MIN", "MAN", "PU", "CON", "WRT",
                                   "TRA", "FIRE", "GOV", "OTH")], na.rm = TRUE)

  ggdc <- mutate(ggdc, SUM_deviate = (SUM-SUM_check)/SUM * 100)

  # how much deviation is acceptable?
  # given in percent
  deviation <- 0.1
  if(any(ggdc$SUM_deviate > deviate, na.rm = TRUE)) stop("Deviation is greater than ", deviation, " percent")
}

# compute sector shares
ggdc <- mutate_each(ggdc, funs(SHARE = ./SUM), -spatial, -temporal, -variable)

# convert to long format
ggdc <- melt(ggdc, id.vars = c("spatial", "variable", "temporal"), variable.name = "sector")

# make it IDA compatible
ggdc <- mutate(ggdc, variable = paste(variable, sector, sep="_"),
               sector = NULL,
               model = "history",
               source_id = "GGDC 10-sector database",
               scenario = "history",
               unit = NA)

# adding units
ggdc[ggdc$variable %in% c(paste0("VA_", sectors_10), "VA_SUM"), "unit"] <- "LCU/yr"
ggdc[ggdc$variable %in% c(paste0("VA_Q05_", sectors_10), "VA_Q05_SUM"), "unit"] <- "LCU2005/yr"
ggdc[ggdc$variable %in% c(paste0("VA_Q10_", sectors_10), "VA_Q10_SUM"), "unit"] <- "LCU2010/yr"
ggdc[ggdc$variable %in% c(paste0("VA_Q91_", sectors_10), "VA_Q91_SUM"), "unit"] <- "LCU1991/yr"
ggdc[grepl("EMP", ggdc$variable) & !grepl("SHARE", ggdc$variable), "unit"] <- "thousand"
ggdc[grepl("SHARE", ggdc$variable, fixed = TRUE), "unit"] <- "1"

# aggregate GGDC data to the three sector level ----
# store units
ggdc_units <- select(ggdc, variable, unit) %>% distinct()

ggdc <- dcast(ggdc, source_id + model + scenario + spatial + temporal ~ variable)
ggdc <- mutate(ggdc, VA_IND = VA_MIN + VA_MAN + VA_PU + VA_CON,
                VA_SER = VA_WRT + VA_TRA + VA_FIRE + VA_GOV + VA_OTH,
                VA_Q10_IND = VA_Q10_MIN + VA_Q10_MAN + VA_Q10_PU + VA_Q10_CON,
                VA_Q10_SER = VA_Q10_WRT + VA_Q10_TRA + VA_Q10_FIRE + VA_Q10_GOV
                + VA_Q10_OTH,
                VA_Q05_IND = VA_Q05_MIN + VA_Q05_MAN + VA_Q05_PU + VA_Q05_CON,
                VA_Q05_SER = VA_Q05_WRT + VA_Q05_TRA + VA_Q05_FIRE + VA_Q05_GOV
                + VA_Q05_OTH,
                VA_Q91_IND = VA_Q91_MIN + VA_Q91_MAN + VA_Q91_PU + VA_Q91_CON,
                VA_Q91_SER = VA_Q91_WRT + VA_Q91_TRA + VA_Q91_FIRE + VA_Q91_GOV
                + VA_Q91_OTH)

ggdc <- melt(ggdc, id.vars = c("source_id", "model", "scenario", "spatial",
                                 "temporal"))

ggdc <- left_join(ggdc, ggdc_units, by = "variable")

# assign the respective units to the GGDC data
ggdc[ggdc$variable == "VA_IND", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_MIN", "unit"]
ggdc[ggdc$variable == "VA_SER", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_MIN", "unit"]
ggdc[ggdc$variable == "VA_Q10_IND", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_Q10_MIN", "unit"]
ggdc[ggdc$variable == "VA_Q10_SER", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_Q10_MIN", "unit"]
ggdc[ggdc$variable == "VA_Q05_IND", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_Q05_MIN", "unit"]
ggdc[ggdc$variable == "VA_Q05_SER", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_Q05_MIN", "unit"]
ggdc[ggdc$variable == "VA_Q91_IND", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_Q91_MIN", "unit"]
ggdc[ggdc$variable == "VA_Q91_SER", "unit"] <-
  ggdc_units[ggdc_units$variable == "VA_Q91_MIN", "unit"]

# compare sector (shares) in different prices ----
country <- "USA"
tmp_va_curr <- filter(ggdc, variable %in% c(paste0("VA_", sectors_3)), spatial == country)
tmp_va_2005 <- filter(ggdc, variable %in% c(paste0("VA_Q05_", sectors_3)), spatial == country)

tmp_va_2005$variable <- gsub("Q05_", "", tmp_va_2005$variable, fixed = TRUE)

tmp_va_2005 <- mutate(tmp_va_2005, prices = "2005 prices")
tmp_va_curr <- mutate(tmp_va_curr, prices = "current prices")

tmp <- rbind(tmp_va_2005, tmp_va_curr)

ggplot() +
  geom_area(data = tmp, aes(x = temporal, y = value, group = variable, fill = variable)) +
  theme_bw() +
  facet_wrap(~prices)
ggsave(file.path("figs", paste0("comp_sector_levels_", unique(tmp$spatial), ".png")), width = 16, height = 12, units = "cm")

tmp_va_curr <- filter(ggdc, variable %in% c(paste0("VA_", sectors_3, "_SHARE")), spatial == country)
tmp_va_2005 <- filter(ggdc, variable %in% c(paste0("VA_Q05_", sectors_3, "_SHARE")), spatial == country)

tmp_va_2005$variable <- gsub("Q05_", "", tmp_va_2005$variable, fixed = TRUE)

tmp_va_2005 <- mutate(tmp_va_2005, prices = "2005 prices")
tmp_va_curr <- mutate(tmp_va_curr, prices = "current prices")

tmp <- rbind(tmp_va_2005, tmp_va_curr)

ggplot() +
  geom_area(data = tmp, aes(x = temporal, y = value, group = variable, fill = variable)) +
  theme_bw() +
  facet_wrap(~prices)
ggsave(file.path("figs", paste0("comp_sector_shares_", unique(tmp$spatial), ".png")), width = 16, height = 12, units = "cm")