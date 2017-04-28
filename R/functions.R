prestimation <- function(x = df, ref_country = settings$country_ref,
                         formula_agr, formula_ind, formula_ser,
                         debug_function = FALSE,
                         prestimate_levels = settings$lhs_levels){

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

  # estimation
  model_agr <- lm(formula = formula_agr, data = x_hist)
  model_ind <- lm(formula = formula_ind, data = x_hist)
  model_ser <- lm(formula = formula_ser, data = x_hist)

  # due to the fixed effect prediction can only be done for countries for which
  # estimation has been carried out
  countries_ind <- c(ref_country, as.character(unique(model_ind$model$spatial)))
  countries_agr <- c(ref_country, as.character(unique(model_agr$model$spatial)))
  countries_ser <- c(ref_country, as.character(unique(model_ser$model$spatial)))

  countries <- list("agr" = countries_agr,
                    "ind" = countries_ind,
                    "ser" = countries_agr)

  # prediction
if(prestimate_levels){
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
} else {
    # check if there are fixed effects present for agriculture and predict accordingly
  if(length(countries_agr) > 1){
    x_scen[x_scen$spatial %in% countries_agr, "va_agr_share"] <-
      predict(model_agr, newdata = filter(x_scen, spatial %in% countries_agr))
  } else {
    x_scen$va_agr_share = predict(model_agr, newdata = x_scen)
  }

  # check if there are fixed effects present for industry and predict accordingly
  if(length(countries_ind) > 1){
    x_scen[x_scen$spatial %in% countries_ind, "va_ind_share"] <-
      predict(model_ind, newdata = filter(x_scen, spatial %in% countries_ind))
  } else {
    x_scen$va_ind_share = predict(model_ind, newdata = x_scen)
  }

  # check if there are fixed effects present for services and predict accordingly
  if(length(countries_ser) > 1){
    x_scen[x_scen$spatial %in% countries_ser, "va_ser_share"] <-
      predict(model_ser, newdata = filter(x_scen, spatial %in% countries_ser))
  } else {
    x_scen$va_ser_share = predict(model_ser, newdata = x_scen)
  }

  # compute service sector as residual and level values
  x_scen <- mutate(x_scen,
                   va_agr = va_agr_share * gdp,
                   va_ind = va_ind_share * gdp,
                   va_ser = va_ser_share * gdp)

  df <- rbind(x_hist, x_scen)
}


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
                 "model_ser" = model_ser,
                 "countries" = countries,
                 "formula_agr" = formula_agr,
                 "formula_ind" = formula_ind,
                 "formula_ser" = formula_ser)

  return(result)
}

plot_country_results <- function(x, level, scen_hist = "history",
                                 scen_fut = "SSP2",
                                 t_present = 2015,
                                 t_max = 2100,
                                 debug_function = FALSE,
                                 ref_country = country_ref,
                                 run_settings = settings){

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

  pdf_path <- file.path(run_settings$outdir, "figures", paste0("country_results_", level,
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

# this function creates directory structure
prepare_run <- function(settings_list){

  # create right hand side of formulas
  rhs <- list()

  for (m in 1:length(settings_list$regressors)){
    tmp <-  combn(settings_list$regressors, m, simplify = FALSE)

    tmp <- lapply(tmp, paste0, collapse = " + ")

    rhs <- append(rhs, tmp)
  }

  rhs <- as.character(rhs)

  # create directory structure
  # output folder for the specific run
  outdir <- file.path("output", settings_list$exp_name)

  if(!dir.exists(file.path(outdir, "figures"))){
    dir.create(file.path(outdir, "figures"), recursive = TRUE, showWarnings = FALSE)
  }

  if(!dir.exists(file.path(outdir, "data"))){
    dir.create(file.path(outdir, "data"), recursive = TRUE, showWarnings = FALSE)
  }

  # append to settings_list
  settings_list$rhs <- rhs
  settings_list$outdir <- outdir

  # write out settings to disk
  sink(file.path(outdir, "settings.txt"))
  print(settings_list)
  sink()

  return(settings_list)
}

plot_hist_fit_pred <- function(x, country, end_year){
  # preallocate data frames
  xdf <- data.frame()
  tmp_hist <- data.frame()
  tmp_scen <- data.frame()
  tmp_fit <- data.frame()
  model_stats <- data.frame()

  for(run in names(x)){
    xdf_loop <- x[[run]]$data
    xdf_loop$run <- run

  tmp_hist_loop <- filter(xdf_loop, spatial == country, scenario == "history")
  tmp_scen_loop <- filter(xdf_loop, spatial == country, scenario != "history", temporal <= end_year)

  tmp_fit_loop <- filter(xdf_loop, spatial %in% country, scenario == "history",
                    temporal <= end_year)

  tmp_fit_loop$va_agr_pc_fit <- predict(x[[run]]$model_agr, newdata = tmp_fit_loop)
  tmp_fit_loop$va_ind_pc_fit <- predict(x[[run]]$model_ind, newdata = tmp_fit_loop)
  tmp_fit_loop$va_ser_pc_fit <- predict(x[[run]]$model_ser, newdata = tmp_fit_loop)

  # combine results
  xdf <- rbind(xdf, xdf_loop)
  tmp_hist <- rbind(tmp_hist, tmp_hist_loop)
  tmp_scen <- rbind(tmp_scen, tmp_scen_loop)
  tmp_fit <- rbind(tmp_fit, tmp_fit_loop)

  # model statistics
  model_stats_loop <- data.frame("run" = rep(run, 3),
                                 "sector" = c("agr", "ind", "ser"),
                                 "equation" = NA,
                                 "y_label" = max(tmp_scen_loop$va_ser_pc, na.rm = TRUE))

  # equation
  model_stats_loop[model_stats_loop$sector == "agr", "equation"] <-
    deparse(x[[run]]$formula_agr, width.cutoff = 200)
  model_stats_loop[model_stats_loop$sector == "ind", "equation"] <-
    deparse(x[[run]]$formula_ind, width.cutoff = 200)
  model_stats_loop[model_stats_loop$sector == "ser", "equation"] <-
    deparse(x[[run]]$formula_ser, width.cutoff = 200)

  model_stats <- rbind(model_stats, model_stats_loop)
  }
# browser()
  p <- ggplot() +
    #agriculture
    geom_point(data = tmp_hist, aes(x = gdp_pc, y = va_agr_pc), colour = "green") +
    geom_line(data = tmp_scen, aes(x = gdp_pc, y = va_agr_pc, group = scenario,
                                   colour = scenario)) +
    geom_line(data = tmp_fit, aes(x = gdp_pc, y = va_agr_pc_fit), colour = "green") +
    # industry
    geom_point(data = tmp_hist, aes(x = gdp_pc, y = va_ind_pc), colour = "brown") +
    geom_line(data = tmp_scen, aes(x = gdp_pc, y = va_ind_pc, group = scenario,
                                   colour = scenario)) +
    geom_line(data = tmp_fit, aes(x = gdp_pc, y = va_ind_pc_fit), colour = "brown") +
    #services
    geom_point(data = tmp_hist, aes(x = gdp_pc, y = va_ser_pc), colour = "blue") +
    geom_line(data = tmp_scen, aes(x = gdp_pc, y = va_ser_pc, group = scenario,
                                   colour = scenario)) +
    geom_line(data = tmp_fit, aes(x = gdp_pc, y = va_ser_pc_fit), colour = "blue") +
    theme_light() +
    annotate("text", x = 0, y = max(model_stats[model_stats$sector == "agr", "y_label"]),
             label = model_stats[model_stats$sector == "agr", "equation"], size = 1, hjust = 0) +
        annotate("text", x = 0, y = 0.9*max(model_stats[model_stats$sector == "ind", "y_label"]),
             label = model_stats[model_stats$sector == "ind", "equation"], size = 1, hjust = 0) +
        annotate("text", x = 0, y = 0.8*max(model_stats[model_stats$sector == "ser", "y_label"]),
             label = model_stats[model_stats$sector == "ser", "equation"], size = 1, hjust = 0) +
    facet_wrap(~ run) +
    ggtitle(paste(country, "until", end_year))

  print(p)
  return(p)
}

# https://gist.github.com/stevenworthington/3178163
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then
# load them into the R session.
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}