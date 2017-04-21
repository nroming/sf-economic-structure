# create directory for comparisons ----
dir.create(file.path("output", "comparison"))

# find runs in ouput directory ----
runs <- dir("output")

# exclude some directories
runs <- runs[-grepl("common", runs)]
runs <- runs[-grepl("comparison", runs)]

# extract necessary information ----
for(run in runs){
  path_to_run <- file.path("output", run)

  # load result
  run_result <- readRDS(file.path(path_to_run, "data", "result_list.rda"))

  for (c in c("USA", "IND", "CHN", "NGA")){
    p <- plot_hist_fit_pred(x  = run_result, country = c, end_year = 2020)
    ggsave(plot = p, filename = file.path("output", "comparison",
                                          paste0("fit_", run, "_", c, ".png")))
  }



}
