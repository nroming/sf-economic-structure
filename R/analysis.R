# prepare data ----
# moved to separate script
source("R/prepare_data.R")

# adjust historical sectorial value added so that their sum matches the GDP
if(settings$force_sector_match_gdp){
  df <- mutate(df, va_agr = va_agr/(va_agr + va_ind + va_ser) * gdp,
               va_ind = va_ind/(va_agr + va_ind + va_ser) * gdp,
               va_ser = va_ser/(va_agr + va_ind + va_ser) * gdp,
               va_agr_pc = va_agr_pc/(va_agr_pc + va_ind_pc + va_ser_pc) * gdp_pc,
               va_ind_pc = va_ind_pc/(va_agr_pc + va_ind_pc + va_ser_pc) * gdp_pc,
               va_ser_pc = va_ser_pc/(va_agr_pc + va_ind_pc + va_ser_pc) * gdp_pc)
}

# estimation ----
# determine formulas
formula_agr <- paste("va_agr_pc", "~", settings$rhs)
formula_ind <- paste("va_ind_pc", "~", settings$rhs)
formula_ser <- paste("va_ser_pc", "~", settings$rhs)

# preallocate lsit for results
models_agr <- list()
models_ind <- list()
models_ser <- list()

# preallocate dataframes for adjusted R-squared
rsq_agr <- data.frame()
rsq_ind <- data.frame()
rsq_ser <- data.frame()

# model selection ----
for (i in 1:length(settings$rhs)){
  models_agr[[i]] <- lm(as.formula(formula_agr[i]), data = df)
  models_ind[[i]] <- lm(as.formula(formula_ind[i]), data = df)
  models_ser[[i]] <- lm(as.formula(formula_ser[i]), data = df)

  rsq_agr[i, "formula"] <- formula_agr[i]
  rsq_agr[i, "r_sq_adj"] <- summary(models_agr[[i]])$adj.r.squared

  rsq_ind[i, "formula"] <- formula_ind[i]
  rsq_ind[i, "r_sq_adj"] <- summary(models_ind[[i]])$adj.r.squared

  rsq_ser[i, "formula"] <- formula_ser[i]
  rsq_ser[i, "r_sq_adj"] <- summary(models_ser[[i]])$adj.r.squared
}

# select best model for each sector
best_agr <- filter(rsq_agr, r_sq_adj == max(r_sq_adj)) %>% select(formula) %>%
  as.character() %>% as.formula()
best_ind <- filter(rsq_ind, r_sq_adj == max(r_sq_adj)) %>% select(formula) %>%
  as.character() %>% as.formula()
best_ser <- filter(rsq_ser, r_sq_adj == max(r_sq_adj)) %>% select(formula) %>%
  as.character() %>% as.formula()

# estimation ---
result_list <- prestimation(x = df,
                            formula_agr = best_agr,
                            formula_ind = best_ind,
                            formula_ser = best_ser)

result <- result_list$data

# write out result
write.xlsx(result, file = file.path(settings$outdir, "data/result.xlsx"))
saveRDS(result_list, file = file.path(settings$outdir, "data/result_list.rda"))

# post-processing ----
source("R/post-processing.R")


# regional aggregation ----
result_reg <- inner_join(result, map_region, by = "spatial") %>%
  group_by(scenario, temporal, reg11) %>%
  summarise_each(funs(sum, "sum", sum(., na.rm = TRUE)), gdp, pop, va_ind, va_ser, va_agr) %>%
  ungroup() %>%
  rename(spatial = reg11)

names(result_reg) <- gsub("_sum", "", names(result_reg), fixed = TRUE)

# compute regional sector shares
result_reg <- mutate(result_reg, sum_va = va_agr + va_ind + va_ser,
                     share_agr = va_agr/sum_va,
                     share_ind = va_ind/sum_va,
                     share_ser = va_ser/sum_va)

# plotting ----
if(settings$plotting) source("R/plotting.R")
