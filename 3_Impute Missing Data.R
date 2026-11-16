#### Set iterations
# For data exploration, 5
# For preregistered analyses, 100
n_imputations = 5



#### Load packages
library(here)
library(mice)
library(tidyverse)
`%+%` <- paste0



#### Load data
pimh <- readRDS(here("Data", "PIMH - 2012-2019", "pimh_with_geo.rds"))
pih <- readRDS(here("Data", "PIH - 2019-Current", "pih_with_geo.rds"))



#### Impute
prep_for_mice <- function(data) {
  data %>%
    mutate(
      across(c(dep_self_label_now, dep_self_label_past), as.factor) # Plays nice with mice
    ) %>%
    as.data.frame() # Plays nice with mice
}

exclude_from_mice <- c(
  "user_id", "session_id", "dataset", "date", "year", "decimal_year_centered",
  "country", "zip", "zcta", "county", "response_time", "res_ratio", "referral",
  "race", "depression" %+% 1:7, "saw_dep_self_label", "dep_self_label_ever",
  "political_id_numeric"
)

run_mice <- function(data, print = F) {

  data <- prep_for_mice(data)

  variables_to_exclude <- intersect(exclude_from_mice, names(data))

  meth <- make.method(data)
  meth[variables_to_exclude] <- "" # Do not impute
  if(print) print(meth)

  pred <- make.predictorMatrix(data)
  pred[, variables_to_exclude] <- 0 # Do not use for imputation
  pred[variables_to_exclude, ] <- 0 # Do not use for imputation
  if(print) print(pred)

  if(print) return()

  futuremice(
    data,
    m = n_imputations,
    method = meth,
    predictorMatrix = pred,
    parallelseed = 745802,
    n.core = parallel::detectCores(logical = FALSE) - 1
  )

}

# Undo prep_for_mice() conversions after imputation
restore_types <- function(data) {
  data %>%
    mutate(
      across(c(dep_self_label_now, dep_self_label_past), ~ .x == "TRUE"),
      political_id_numeric = as.numeric(political_id) # Recompute (convert to numeric) from imputed political_id
    )
}

# Impute data, including `mice` output
pimh_imputed <- run_mice(pimh)
pih_imputed <- run_mice(pih)

# Complete data, for modeling
pimh_completed <- complete(pimh_imputed, action = "all") %>% map(restore_types)
pih_completed <- complete(pih_imputed, action = "all") %>% map(restore_types)
all_completed <- map2(pimh_completed, pih_completed, bind_rows)

# Stack data, for some plots (not for modeling)
pimh_stacked <- complete(pimh_imputed, action = "long") %>% restore_types()
pih_stacked <- complete(pih_imputed, action = "long") %>% restore_types()
all_stacked <- bind_rows(pimh_stacked, pih_stacked)



#### Save data
# Save imputed
saveRDS(pimh_imputed, here("Data", "PIMH - 2012-2019", "pimh_imputed.rds"))
saveRDS(pih_imputed, here("Data", "PIH - 2019-Current", "pih_imputed.rds"))

# Save completed
saveRDS(pimh_completed, here("Data", "PIMH - 2012-2019", "pimh_completed.rds"))
saveRDS(pih_completed, here("Data", "PIH - 2019-Current", "pih_completed.rds"))
saveRDS(all_completed, here("Data", "PIMH + PIH Combined", "all_completed.rds"))

# Save stacked
saveRDS(pimh_stacked, here("Data", "PIMH - 2012-2019", "pimh_stacked.rds"))
saveRDS(pih_stacked, here("Data", "PIH - 2019-Current", "pih_stacked.rds"))
saveRDS(all_stacked, here("Data", "PIMH + PIH Combined", "all_stacked.rds"))
