# This script replaces the functions formerly provided by the IDA package

# code below was taken from 'R/interpolate_missing_years.R' from commit 883c7fc7
#' Interpolation of missing years in a dataset
#'
#' @param df A compatible dataframe
#' @param stepwidth A unit for the output stepwidth in years
#' @param method One of 'linear' or 'spline' (see \code{\link[zoo]{na.approx}})
#' @return A compatible dataframe
#' @export
interpolate_missing_years <- function(df, stepwidth = 1, method = "linear"){
  # detect length of temporal data
  if (any(nchar(df$temporal)) > 4){
    stop("Temporal dimension does not seem to be in years!")
  }

  time_target <- seq(min(df$temporal, na.rm = TRUE), max(df$temporal, na.rm = TRUE), by = stepwidth)

  # preallocate empty target dataframe containing all possible combinations of
  # column values in the original dataframe except the original values
  tmp <- expand.grid(source_id = unique(df$source_id),
                     model = unique(df$model),
                     scenario = unique(df$scenario),
                     variable = unique(df$variable),
                     temporal = as.integer(time_target),
                     spatial = unique(df$spatial),
                     unit = unique(df$unit))

  #expand the original dataframe with the new years
  df <- left_join(tmp, df, by = c("source_id", "model", "scenario", "variable",
                                  "temporal", "spatial", "unit"))

  # get into the right order so that the interpolations functions from the
  # zoo-package work
  df <- arrange(df, source_id, model, scenario, spatial, variable, temporal)

  if (method == "linear"){
    # linear approximation of missing values

    # FIXME: this gives back a strange error:
    # Error: incompatible size (2), expecting 101 (the group size) or 1
    # (actual numbers differ)
    # http://stackoverflow.com/questions/27920690/linear-interpolation-using-dplyr
    df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
      filter(!all(is.na(value))) %>% ungroup()
    df <- mutate(df, value = zoo::na.approx(value))

    # df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
    #     mutate(value = zoo::approx(value)) %>% ungroup()
  } else if (method == "spline"){
    df <- group_by(df, source_id, model, scenario, spatial, variable, unit) %>%
      ungroup()
    df <- mutate(df, value = zoo::na.spline(value))
  } else {
    stop("No valid method of interpolation provided (must be one of 'linear' or 'spline')!")
  }

  return(df)
}

# code below was taken from 'R/utilities.R' from commit 883c7fc7
#' Renames a variable
#'
#' @param df A compatible dataframe
#' @param var Name of the variable to be renamed
#' @param new_name New name of the variable
#' @return A dataframe with the new instead of the old variable name
#' @export
rename_var <- function(df, var, new_name){
  tmp <- filter(df, variable == var)
  df <- filter(df, !(variable == var))

  tmp$variable <- new_name

  df <- rbind(df, tmp)

  return(df)
}

#' Renames scenarios, e.g. to get nicer names for plots
#'
#' @param df A compatible dataframe
#' @param old_name new_name Old and new scenario name, repectively
#' @return A compatible dataframe with the respective scenario changes accordingly
#' @import dplyr
#' @export
change_scenario <- function(df, old_name, new_name){
  # copy original data
  df_orig <- df

  # create apprpriate subset and remove the same data from the original data for
  # later reattachment of changed data
  df <- filter(df, scenario == old_name)
  df_orig <- filter(df_orig, scenario != old_name)

  df <- mutate(df, scenario = new_name)

  df <- rbind(df, df_orig)

  return(df)
}