# find runs in ouput directory ----
runs <- dir("output")

runs <- runs[-grepl("common", runs)]

# extract necessary information ----
for(run in runs){
  path_to_run <- file.path("output", run)

  # load result
  run_result <- readRDS(file.path(path_to_run, "data", "result_list.rda"))

  for (c in c("USA", "IND", "CHN", "BGL")){
    p <- plot_hist_fit_pred(x  = run_result, country = c, end_year = 2030)
    ggsave(plot = p, filename = file.path(path_to_run, "figures",
                                          paste0("fit_", c, ".png")))
  }


}
