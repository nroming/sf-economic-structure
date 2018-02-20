# This script replaces the data formerly provided by the IDA package
# code was taken from 'data/load_and_prepare_data.R' from commit 883c7fc7

# compatibility log
# replace directory 'data-raw' with 'data'
# replace reference to 'WDI 2015' with 'WDI 2016'

# compatibility code (not in IDA)
idata_n <- data.frame()

# WDI 2015 ----
cat("Reading in WDI 2015 data.\n")
WDI_2015 <- read_csv(unz("data/WDI_csv_2015.zip", "WDI_Data.csv"))

WDI_vars <- filter(read_excel("data/Variables.xlsx", sheet = "WDI_2015"),
                   !(is.na(variable)))

# merge WDI data with variable selection
WDI_2015 <- left_join(WDI_vars, WDI_2015)

# rename 'Country Code' column to 'wb' to make comparisons with the
# countrycode_data dataframe from the countrycode package more easy
names(WDI_2015)[names(WDI_2015) == "Country Code"] <- "wb"

# only keep country and global values, drop regional aggregates
WDI_2015 <- filter(WDI_2015, wb %in% c(codelist$wb, "WLD"))

# drop columns
WDI_2015 <- select(WDI_2015, variable, is_unit, target_unit, conversion, wb, 9:64)

# rename
WDI_2015 <- rename(WDI_2015, spatial = wb)

# melt dataframe
WDI_2015 <- melt(WDI_2015, id.vars = c("spatial", "variable", "is_unit", "target_unit",
                             "conversion"),
            variable.name = "temporal")

# so far, temporal is a factor, not good
WDI_2015$temporal <- as.numeric(as.character(WDI_2015$temporal))

# drop NAs
WDI_2015 <- filter(WDI_2015, !(is.na(value)))

# harmonize units
WDI_2015 <- group_by(WDI_2015, spatial, variable, is_unit, target_unit, conversion, temporal) %>%
                mutate(value = eval(parse(text = paste(value, conversion)))) %>%
                rename(unit = target_unit) %>% ungroup()

WDI_2015 <- select(WDI_2015, spatial, temporal, variable, unit, value)

WDI_2015$model <- "WDI_2015"

WDI_2015$scenario <- "history"

WDI_2015$source_id <- "WDI_2015"

idata_n <- rbind(idata_n, WDI_2015)

# IEA energy data ----
ktoe2EJ <- 0.000041868
cat("Reading IEA energy data (2014 edition).")
iea <- read_csv(unz("data/IEA_2014.zip", "IEA_2014/IEA2014.csv"), na = c("..", "x"))

iea$TIME <- gsub("2013E", "2013", iea$TIME)
iea$TIME <- as.integer(iea$TIME)

map_reg <- read_excel("data/mappings.xlsx", sheet = "regions")

# primary energy
# some PE types need aggregation
coal <- c("HARDCOAL", "BROWN", "ANTCOAL", "COKCOAL", "BITCOAL", "SUBCOAL",
          "LIGNITE", "PATFUEL", "OVENCOKE", "GASCOKE", "COALTAR", "BKB",
          "GASWKSGS", "COKEOVGS", "BLFURGS", "OGASES", "PEAT", "PEATPROD")
oil <- c("CRNGFEED", "CRUDEOIL", "NGL", "REFFEEDS", "ADDITIVE", "NONCRUDE")
oil_products <- c("REFINGAS", "ETHANE", "LPG", "NONBIOGASO", "AVGAS", "JETGAS",
         "NONBIOJETK", "OTHKERO", "NONBIODIES", "RESFUEL", "NAPHTA", "WHITESP",
         "LUBRIC", "BITUMEN", "PARWAX", "PETCOKE", "ONONSPEC")
renewables <- c("HYDRO", "GEOTHERM", "SOLARPV", "SOLARTH", "TIDE", "WIND")
biomass <- c("MUNWASTER", "PRIMSBIO", "BIOGASES", "BIOGASOL", "BIODIESEL",
             "OBIOLIQ", "RENEWNS", "CHARCOAL")

pe <- filter(iea, FLOW == "TPES", PRODUCT %in% c(coal, oil, oil_products, "NATGAS",
                                                  "NUCLEAR", renewables,
                                                  biomass))

# add variable column which can also be used for aggregation
pe$variable <- NA
pe[pe$PRODUCT %in% coal, "variable"] <- "Primary Energy|Coal"
pe[pe$PRODUCT %in% c(oil, oil_products), "variable"] <- "Primary Energy|Oil"
pe[pe$PRODUCT == "NATGAS", "variable"] <- "Primary Energy|Gas"
pe[pe$PRODUCT == "NUCLEAR", "variable"] <- "Primary Energy|Nuclear"
pe[pe$PRODUCT %in% renewables, "variable"] <- "Primary Energy|Non-Biomass Renewables"
pe[pe$PRODUCT %in% biomass, "variable"] <- "Primary Energy|Biomass"

pe <- group_by(pe, COUNTRY, TIME, variable) %>%
        summarise(value = sum(ktoe, na.rm = TRUE)) %>% ungroup() %>%
        mutate(value = value * ktoe2EJ, unit = "EJ/yr", source_id = "IEA_2014",
               model = "IEA", scenario = "history") %>%
        rename(temporal = TIME, IEA_country = COUNTRY)

# special treatment of nuclear, which is contained in the data with its heat content
pe[pe$variable == "Primary Energy|Nuclear", "value"] <- pe[pe$variable == "Primary Energy|Nuclear", "value"] / 3

pe <- inner_join(pe, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

idata_n <- rbind(idata_n, pe)

# final energy
fe <- filter(iea, FLOW == "TFC", PRODUCT == "TOTAL") %>%
        mutate(variable = "Final Energy|Total",
                ktoe = ktoe * ktoe2EJ,
                unit = "EJ/yr",
                model = "IEA",
                scenario = "history",
                source_id = "IEA_2014") %>%
        select(-PRODUCT, -FLOW) %>%
        rename(value = ktoe, temporal = TIME, IEA_country = COUNTRY)

fe <- inner_join(fe, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

idata_n <- rbind(idata_n, fe)

# coal used for electricity
# no autoproducers (AUTOELEC, AUTOCHP)
coal_elec <- filter(iea, FLOW %in% c("MAINELEC",  "MAINCHP"), PRODUCT %in% coal)

# values are all negative
coal_elec <- group_by(coal_elec, COUNTRY, TIME) %>%
                summarise( value = sum(ktoe, na.rm = TRUE)) %>%
                mutate(value = value  * -ktoe2EJ,
                       variable = "Primary Energy|Coal|w/o CCS|Electricity",
                       unit = "EJ/yr",
                       model = "IEA",
                       scenario = "history",
                       source_id = "IEA_2014") %>%
                rename(temporal = TIME, IEA_country = COUNTRY) %>% ungroup()

coal_elec <- inner_join(coal_elec, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

# attach to data
idata_n <- rbind(idata_n, coal_elec)

# including autoproducers (AUTOELEC, AUTOCHP)
coal_elec1 <- filter(iea, FLOW %in% c("MAINELEC",  "MAINCHP", "AUTOELEC", "AUTOCHP"), PRODUCT %in% coal)

# values are all negative
coal_elec1 <- group_by(coal_elec1, COUNTRY, TIME) %>%
                summarise( value = sum(ktoe, na.rm = TRUE)) %>%
                mutate(value = value  * -ktoe2EJ,
                       variable = "Primary Energy|Coal|w/o CCS|Electricity_1",
                       unit = "EJ/yr",
                       model = "IEA",
                       scenario = "history",
                       source_id = "IEA_2014") %>%
                rename(temporal = TIME, IEA_country = COUNTRY) %>% ungroup()

coal_elec1 <- inner_join(coal_elec1, select(map_reg, IDA, IEA_country)) %>% select(-IEA_country) %>% rename(spatial = IDA)

# attach to data
idata_n <- rbind(idata_n, coal_elec1)

# final energy aggregation to the usual classes
# this mapping is taken from Antoine Levesque's code for PIK's energy demand
# generator (EDGE)
iea_liquid = c("CRUDEOIL", "NGL", "CRNGFEED",
               "BIODIESEL", "NONBIODIES", "OTHKERO",
               "RESFUEL", "AVGAS", "JETGAS",
               "NONBIOJETK", "REFINGAS",
               "PARWAX", "ONONSPEC", "WHITESP", "NAPHTHA",
               "BITUMEN", "LUBRIC", "NONBIOGASO",
               "BIOGASOL", "OBIOLIQ", "COALTAR","REFFEEDS", "NONCRUDE",
               "LPG", "ADDITIVE")
iea_heat = c("HEAT", "GEOTHERM", "SOLARTH")
iea_gas = c("NATGAS", "GASWKSGS", "COKEOVGS","ETHANE",
            "BLFURGS", "OGASES", "BIOGASES")
iea_solid = c("HARDCOAL", "BROWN", "PATFUEL",
              "MUNWASTER", "MUNWASTEN", "INDWASTE",
              "RENEWNS", "BKB", "ANTCOAL", "COKCOAL",
              "BITCOAL", "SUBCOAL", "LIGNITE","PETCOKE",
              "OVENCOKE", "PEAT", "PEATPROD",
              "PRIMSBIO", "CHARCOAL", "OILSHALE",
              "GASCOKE")
iea_elec = c("ELECTR")

iea_agriculture <- c("AGRICULT", "FISHING")
iea_industry <- c("TOTIND")
iea_services <- c("COMMPUB")

iea_fe_by_sector <- filter(iea, FLOW %in% c(iea_agriculture, iea_industry,
                                            iea_services),
                           PRODUCT %in% c(iea_solid, iea_liquid, iea_gas,
                                          iea_heat, iea_elec))

# merge subsectors
iea_fe_by_sector <- mutate(iea_fe_by_sector,
                           FLOW = gsub("FISHING", "AGRICULT", FLOW))

# rename sectors
iea_fe_by_sector[iea_fe_by_sector$FLOW == "AGRICULT", "FLOW"] <- "Agriculture"
iea_fe_by_sector[iea_fe_by_sector$FLOW == "TOTIND", "FLOW"] <- "Industry"
iea_fe_by_sector[iea_fe_by_sector$FLOW == "COMMPUB", "FLOW"] <- "Services"


# replace the FE subtypes
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_solid, "PRODUCT"] <- "Solids"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_liquid, "PRODUCT"] <- "Liquids"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_gas, "PRODUCT"] <- "Gases"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_heat, "PRODUCT"] <- "Heat"
iea_fe_by_sector[iea_fe_by_sector$PRODUCT %in% iea_elec, "PRODUCT"] <- "Electricity"

# aggregation of FE types by sector, country, year
iea_fe_by_sector <- group_by(iea_fe_by_sector, COUNTRY, TIME, FLOW, PRODUCT) %>%
  summarise(value = sum(ktoe, na.rm = TRUE) * ktoe2EJ) %>%
  rename(temporal = TIME) %>%
  mutate(variable = paste("Final Energy", FLOW, PRODUCT, sep = "|"),
         unit = "EJ/yr",
         source_id = "IEA_2014",
         model = "IEA",
         scenario = "history") %>%
  ungroup()

iea_fe_by_sector <- mutate(iea_fe_by_sector,
                           spatial = countrycode(COUNTRY, "country.name", "iso3c")) %>%
  filter(!is.na(spatial)) %>%
  select(-COUNTRY, -FLOW, -PRODUCT)

idata_n <- rbind(idata_n, iea_fe_by_sector)

rm(iea, iea_fe_by_sector)

# SSP scenario database -----
cat("Reading in SSP database.\n")
SSP <- read_csv(unz("data/SspDb_country_data_2013-06-12.csv.zip",
                    "SspDb_country_data_2013-06-12.csv"), progress = TRUE)

# load PPP-MER exchange rates
ppp_mer <- read_excel("data/OECD-WB PPP-MER2005_conversion_rates.xlsx",
                      sheet = "OECD-WB PPP-MER Conversionrates")

names(ppp_mer) <- c("spatial", "xr")

names(SSP) <- tolower(names(SSP))

SSP <- rename(SSP, spatial = region)

SSP <- melt(SSP, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

SSP$value <- as.numeric(SSP$value)

SSP <- na.omit(SSP)

SSP$source_id <- "SSP"

# unit conversions
SSP[SSP$unit == "billion US$2005/yr", "unit"] <- "bn PPP2005/yr"
SSP[SSP$variable == "GDP|PPP", "variable"] <- "GDP"

# calculate GDP in MER
mer <- filter(SSP, variable == "GDP")
mer <- inner_join(mer, ppp_mer, by = "spatial")

mer <- mutate(mer, value = value * xr, unit = "bn USD2005/yr") %>%
          select(-xr)

SSP <- rbind(SSP, mer)

# only keep necessary data:
# scenario group 'SSPX_v9_130325' (replace 'X' with 1 through 5) contains GDP and Population data from the OECD.
# Since they used IIASA total population numbers as input, we only keep the GDP
# numbers from these group of scenarios
# scenario group 'SSPX_v9_130115' (replace 'X' with 1 through 5) contains GDP,
# total population and detailed population structure by sex and education from
# IIASA and urbanizations scenarios from NCAR. We drop the GDP numbers since the
# OECD GDP numbers are the SSP reference scenarios used in the scientific
# community
gdp <- filter(SSP, model == "OECD Env-Growth", variable == "GDP")
pop <- filter(SSP, model %in% c("NCAR", "IIASA-WiC POP"), variable != "GDP")

SSP <- rbind(gdp, pop)

# clean scenario names: remove version and submission information
SSP$scenario <- substr(SSP$scenario, start = 1, stop = 4)

idata_n <- rbind(idata_n, SSP)

# AR5 database ----
cat("Reading in AR5 data.\n")
AR5 <- read_csv(unz("data/ar5_public_version102_compare_20150629-130000.csv.zip",
                    "ar5_public_version102_compare_compare_20150629-130000.csv"),
                progress = TRUE)

names(AR5) <- tolower(names(AR5))

AR5 <- rename(AR5, spatial = region)

AR5 <- melt(AR5, id.vars = c("model", "scenario", "spatial", "variable", "unit"),
            variable.name = "temporal")

AR5$value <- as.numeric(AR5$value)

AR5 <- filter(AR5, !(is.na(value)))

AR5$source_id <- "AR5"

# variable renaming
AR5[AR5$variable == "GDP|MER", "variable"] <- "GDP"

# unit renaming
AR5[AR5$unit == "US$2005/t CO2", "unit"] <- "USD2005/t CO2"
AR5[AR5$unit == "billion US$2005/yr", "unit"] <- "bn USD2005/yr"
AR5[AR5$unit == "?C", "unit"] <- "°C"

# unit conversions
AR5[AR5$unit == "kt N2O/yr", "value"] <-
  AR5[AR5$unit == "kt N2O/yr", "value"] / 1e3
AR5[AR5$unit == "kt N2O/yr", "unit"] <- "Mt N2O/yr"

idata_n <- rbind(idata_n, AR5)

idata_n$source_id <- as.factor(idata_n$source_id)
idata_n$spatial <- as.factor(idata_n$spatial)
idata_n$scenario <- as.factor(idata_n$scenario)
idata_n$temporal <- as.integer(idata_n$temporal)
idata_n$variable <- as.factor(idata_n$variable)
idata_n$unit <- as.factor(idata_n$unit)
idata_n$model <- as.factor(idata_n$model)

# additional clean-up (not in IDA)
rm(WDI_2015, coal_elec, coal_elec1, fe, map_reg, pe, WDI_vars, SSP, AR5)
