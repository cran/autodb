## ----setup--------------------------------------------------------------------
library(autodb)

## ----check_diagrammer---------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  show <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
  maybe_plot <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
}else{
  show <- print
  maybe_plot <- function(x) invisible(NULL)
}

## ----example_data_frame_with_NAs----------------------------------------------
df_nas <- data.frame(
  patient = c(1L, 2L, 3L, 4L),
  trial_entry_date = as.Date(c("2022/05/02", "2022/06/06", "2022/04/01", "2022/03/19")),
  trial_exit_date = as.Date(c(NA, NA, "2022/10/07", NA))
)
knitr::kable(df_nas)

## -----------------------------------------------------------------------------
show(autodb(df_nas))

## ----example_data_frame_with_NAs_nullably_normalised--------------------------
ds_trial <- database_schema(
  relation_schema(
    list(
      patient = list(c("patient", "trial_entry_date"), list("patient")),
      patient_exit = list(c("patient", "trial_exit_date"), list("patient"))
    ),
    names(df_nas)
  ),
  list(list("patient_exit", "patient", "patient", "patient"))
)

# approach 1: decompose, then remove
ideal_db <- decompose(df_nas, ds_trial)
records(ideal_db)$patient_exit <- subset(
  records(ideal_db)$patient_exit,
  !is.na(trial_exit_date)
)

# approach 2: create and insert
ideal_db2 <- create(ds_trial) |>
  insert(df_nas, relations = "patient") |>
  insert(subset(df_nas, !is.na(trial_exit_date)), relations = "patient_exit")

stopifnot(identical(ideal_db2, ideal_db))

## -----------------------------------------------------------------------------
show(ideal_db)

## ----example_data_frame_with_interval_option----------------------------------
df_options <- data.frame(
  id = 1:20,
  value = c(2.3, 2.3, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
  lower_bound = c(NA_real_, NA_real_, NA_real_, 2.4, 0, 1, 0, 5.6, 2.4, 5.3, 5.3, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 5.6, 2.4),
  upper_bound = c(NA_real_, NA_real_, NA_real_, 7.1, 10, 10, 13.1, 25.8, 10, 13.1, 10, 25.8, 25.8, 25.8, 25.8,13.1, 13.1, 25.8, 25.8, 25.8),
  interval_distribution = factor(c(NA, NA, NA, "uniform", "uniform", "uniform", "uniform", "uniform", "Beta", "Beta", "Beta", "Beta", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "PERT", "PERT", "PERT", "PERT")),
  param1 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 2, 2, 2.1, 2, 2, 2, 1, 2, 2),
  param2 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, 1, 1, NA, NA, NA, NA)
)
knitr::kable(df_options)

## ----example_data_frame_with_interval_option_db-------------------------------
db_options <- autodb(df_options)

## -----------------------------------------------------------------------------
show(db_options)

## ----example_data_frame_with_interval_option_nulls----------------------------
df_options_presence <- df_options[vapply(df_options, anyNA, logical(1))]
df_options_presence[] <- lapply(df_options_presence, Negate(is.na))
names(df_options_presence) <- paste0(names(df_options_presence), "_present")
df_options_with_presence <- cbind(df_options, df_options_presence)

## ----example_data_frame_with_interval_option_nulls_db-------------------------
db_options_with_presence <- autodb(df_options_with_presence)

## -----------------------------------------------------------------------------
show(db_options_with_presence)

## ----example_data_frame_with_interval_option_nulls_rel------------------------
knitr::kable(records(db_options_with_presence)$value_present)

## ----example_data_frame_with_interval_option_nulls_distribution_rel-----------
knitr::kable(records(db_options_with_presence)$interval_distribution)

