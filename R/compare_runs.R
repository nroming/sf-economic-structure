if(!exists("g20")) source("R/settings.R")

# create directory for comparisons ----
dir.create(file.path("output", "comparison"))

# find runs in ouput directory ----
runs <- dir("output/runs")

result_all <- list()

# extract necessary information ----
for(run in runs){
  path_to_run <- file.path("output/runs", run)

  # load result
  run_result <- readRDS(file.path(path_to_run, "data", "result_list.rda"))

  # add run
  result_all[run] <- list(run_result)

}

countries <- g20

for (country in countries){
  try(p <- plot_hist_fit_pred(x  = result_all, country = country, end_year = 2030))
  try(ggsave(plot = p, filename = file.path("output", "comparison",
                                        paste0("fit_", country, ".png")),
         width = 18, height = 10, units = "cm"))
}

