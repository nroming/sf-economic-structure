prestimation <- function(x = df, ref_country = country_ref,
                         formula_agr, formula_ind, formula_ser,
                         debug_function = FALSE){

  if(debug_function){
    browser()
  }

  # relevel countries so that the US (or another country) are used as reference
  # for the fixed effects estimation below
  x = mutate(x, spatial = relevel(spatial, ref = ref_country))

  # split up data into separate dataframes for historic and scenario data
  x_hist <- filter(x, scenario == "history")
  x_scen <- filter(x, scenario != "history")

  # for some countries (e.g. Aruba (ABW)) only historic population is available.
  # need to filter out such cases as we need a list of countries for which
  # estimation is actually possible to limit the scenario dataset, which is used
  # for prediction, to these
  x_hist <- filter(x_hist, !is.na(gdp), !is.na(pop))

  # attach information about country area
  country_area <- filter(x_hist, temporal == 2010) %>%
    select(spatial, area) %>%
    distinct() %>%
    na.omit()

  # drop old area column (only NAs)
  x_scen <- select(x_scen, -area)

  # attach country area information
  x_scen <- inner_join(x_scen, country_area, by = c("spatial"))

  # calculate population density
  x_scen <- mutate(x_scen, pop_dens = pop/area)

  # limit scenario data to countries that are present in the historic data
  x_scen <- filter(x_scen, !(spatial %in% setequal(unique(x_scen$spatial),
                                                   unique(x_hist$spatial))))

  model_agr <- lm(formula = formula_agr, data = x_hist)
  model_ind <- lm(formula = formula_ind, data = x_hist)
  model_ser <- lm(formula = formula_ser, data = x_hist)

  # due to the fixed effect prediction can only be done for countries for which
  # estimation has been carried out
  countries_ind <- c(ref_country, as.character(unique(model_ind$model$spatial)))
  countries_agr <- c(ref_country, as.character(unique(model_agr$model$spatial)))
  countries_ser <- c(ref_country, as.character(unique(model_ser$model$spatial)))

  # check if there are fixed effects present for agriculture and predict accordingly
  if(length(countries_agr) > 1){
    x_scen[x_scen$spatial %in% countries_agr, "va_agr_pc"] <-
      predict(model_agr, newdata = filter(x_scen, spatial %in% countries_agr))
  } else {
    x_scen$va_agr_pc = predict(model_agr, newdata = x_scen)
  }

  # check if there are fixed effects present for industry and predict accordingly
  if(length(countries_ind) > 1){
    x_scen[x_scen$spatial %in% countries_ind, "va_ind_pc"] <-
      predict(model_ind, newdata = filter(x_scen, spatial %in% countries_ind))
  } else {
    x_scen$va_ind_pc = predict(model_ind, newdata = x_scen)
  }

  # check if there are fixed effects present for services and predict accordingly
  if(length(countries_ser) > 1){
    x_scen[x_scen$spatial %in% countries_ser, "va_ser_pc"] <-
      predict(model_ser, newdata = filter(x_scen, spatial %in% countries_ser))
  } else {
    x_scen$va_ser_pc = predict(model_ser, newdata = x_scen)
  }

  # compute service sector as residual and level values
  x_scen <- mutate(x_scen,
                   va_agr = va_agr_pc * pop,
                   va_ind = va_ind_pc * pop,
                   va_ser = va_ser_pc * pop)

  df <- rbind(x_hist, x_scen)

  # compute GDP(pC) resulting from projections and deviation
  df <- mutate(df, gdp_pc_pred = va_agr_pc + va_ind_pc + va_ser_pc,
               gdp_pred = va_agr + va_ind + va_ser,
               gdp_pc_dev = gdp_pc - gdp_pc_pred,
               gdp_dev = gdp - gdp_pred)

  df <- arrange(df, spatial)

  # create list containing results
  result <- list("data" = df,
                 "model_agr" = model_agr,
                 "model_ind" = model_ind,
                 "model_ser" = model_ser)

  return(result)
}

plot_country_results <- function(x, level, scen_hist = "history",
                                 scen_fut = "SSP2",
                                 t_present = 2015,
                                 t_max = 2100,
                                 debug_function = FALSE,
                                 ref_country = country_ref){

  if(debug_function){
    browser()
  }

  # select appropriate columns
  switch(level,
         "total" = {
           # keep only columns of interest
           x <- select(x, scenario, spatial, temporal, gdp, va_agr, va_ind,
                       va_ser)

           # differentiate between historical and scenario data
           x_hist = filter(x, scenario == scen_hist,
                           temporal < t_present)

           x_scen = filter(x, scenario == scen_fut,
                           temporal >= t_present, temporal <= t_max)
         },
         "capita" = {
           # keep only columns of interest
           x <- select(x, scenario, spatial, temporal, gdp_pc, va_agr_pc,
                       va_ind_pc, va_ser_pc)

           # differentiate between historical and scenario data
           x_hist = filter(x, scenario == scen_hist,
                           temporal < t_present)

           x_scen = filter(x, scenario == scen_fut,
                           temporal >= t_present, temporal <= t_max)
         })

  # join data together again
  x <- rbind(x_hist, x_scen)

  x <- melt(x, id.vars = c("scenario", "spatial", "temporal"))

  countries <- sort(as.character(unique(x$spatial)))

  num_pages <- length(countries) %/% 20

  start_country <- 1

  pdf_path <- file.path("output/figures", paste0("country_results_", level,
                                                 ".pdf"))

  pdf(pdf_path)
  for(i in seq(num_pages)){

    switch(level,
           "total" = {
             x_area <- filter(x, variable != "gdp",
                              spatial %in% countries[start_country:(start_country + 19)])
             x_line <- filter(x, variable == "gdp",
                              spatial %in% countries[start_country:(start_country + 19)])
           },
           "capita" = {
             x_area <- filter(x, variable != "gdp_pc",
                              spatial %in% countries[start_country:(start_country + 19)])
             x_line <- filter(x, variable == "gdp_pc",
                              spatial %in% countries[start_country:(start_country + 19)])
           })


    start_country <- start_country + 20

    p <- ggplot()
    p <- p + geom_area(data = x_area, aes(x = temporal, y = value, fill=variable))
    p <- p + geom_line(data = x_line, aes(x = temporal, y = value))
    p <- p + ylab("")
    p <- p + xlab("")
    p <- p + theme_bw(base_size = 9)
    p <- p + theme(legend.position = "none")
    p <- p + facet_wrap(~ spatial, scales = "free")
    print(p)
  }
  dev.off()
}

# include functionality that was previously taken from the IDA package
source("R/IDA_independency_functions.R")
