## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(autodb)

## -----------------------------------------------------------------------------
summary(ChickWeight)

## -----------------------------------------------------------------------------
db <- autodb(ChickWeight)
db

## -----------------------------------------------------------------------------
db_text <- gv(db)
cat(db_text)

## ----check_diagrammer---------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  show <- function(x) DiagrammeR::grViz(gv(x))
  maybe_plot <- function(x) DiagrammeR::grViz(gv(x))
}else{
  show <- print
  maybe_plot <- function(x) invisible(NULL)
}

## ----db_plot------------------------------------------------------------------
maybe_plot(db)

## -----------------------------------------------------------------------------
deps <- discover(ChickWeight, accuracy = 1, progress = TRUE)

## -----------------------------------------------------------------------------
deps

## -----------------------------------------------------------------------------
detset(deps)
dependant(deps)
attrs_order(deps)

## -----------------------------------------------------------------------------
schema <- synthesise(deps)
schema

## -----------------------------------------------------------------------------
maybe_plot(schema)

## -----------------------------------------------------------------------------
knitr::kable(as.data.frame(Titanic))

## -----------------------------------------------------------------------------
show(autodb(as.data.frame(Titanic)))

## -----------------------------------------------------------------------------
titanic_deps_freqonly <- discover(as.data.frame(Titanic), 1, exclude = "Freq")
titanic_deps_freqonly

## -----------------------------------------------------------------------------
identical(titanic_deps_freqonly, discover(as.data.frame(Titanic), 1, exclude_class = "numeric"))

## -----------------------------------------------------------------------------
show(autodb(as.data.frame(Titanic), exclude = "Freq"))

## -----------------------------------------------------------------------------
titanic_deps <- discover(as.data.frame(Titanic), 1)
titanic_deps

## -----------------------------------------------------------------------------
titanic_deps[dependant(titanic_deps) == "Freq"]

## -----------------------------------------------------------------------------
linked_schema <- autoref(schema)
linked_schema

## -----------------------------------------------------------------------------
normalise(deps)

## -----------------------------------------------------------------------------
maybe_plot(linked_schema)

## ----chickWeight_db2_plot-----------------------------------------------------
db2 <- decompose(ChickWeight, linked_schema)
show(db2)

## ----chickWeights_rejoin------------------------------------------------------
rejoined <- rejoin(db)
summary(rejoined)
identical(rejoined, ChickWeight)
df_equiv(rejoined, ChickWeight)

## ----nudge_classes------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  DiagrammeR::grViz(gv(nudge, name = "nudge"))
}else{
  summary(nudge)
}

## ----nudge_database-----------------------------------------------------------
nudge_deps <- discover(
  nudge,
  accuracy = 1,
  exclude = c("n_study", "n_comparison", "n_control", "n_intervention"),
  exclude_class = "numeric"
)
nudge_schema <- normalise(nudge_deps, remove_avoidable = TRUE)
show(nudge_schema)

## ----nudge_publication_check--------------------------------------------------
nudge_database <- decompose(nudge, nudge_schema)
nudge_title_relation <- records(nudge_database)$title
nudge_pid_duplicates <- unique(nudge_title_relation$publication_id[
  duplicated(nudge_title_relation$publication_id)
])
knitr::kable(subset(nudge_title_relation, publication_id %in% nudge_pid_duplicates))

## ----nudge_reference_check----------------------------------------------------
nudge_reference_duplicates <- unique(nudge_title_relation$reference[
  duplicated(nudge_title_relation$reference)
])
knitr::kable(subset(nudge_title_relation, reference %in% nudge_reference_duplicates))

## ----nudge_filter-------------------------------------------------------------
nudge_deps_filtered <- nudge_deps[
  lengths(detset(nudge_deps)) == 1 |
    vapply(
      detset(nudge_deps),
      \(ds) length(setdiff(ds, c("publication_id", "reference"))) != 1,
      logical(1)
    )
]
nudge_schema_filtered <- normalise(nudge_deps_filtered, remove_avoidable = TRUE)
show(nudge_schema_filtered)

## ----nudge_sizes--------------------------------------------------------------
nudge_deps_size <- discover(nudge[, startsWith(names(nudge), "n_")], 1)
nudge_deps_size
nudge_deps_final <- c(nudge_deps_filtered, nudge_deps_size)
nudge_schema_final <- normalise(nudge_deps_final, remove_avoidable = TRUE)
nudge_database_final <- decompose(nudge, nudge_schema_final)
show(nudge_schema_final)

## ----nudge_size_check---------------------------------------------------------
knitr::kable(unique(subset(
  nudge,
  n_comparison != n_control + n_intervention,
  c(
    es_id,
    reference,
    title,
    n_study,
    n_comparison,
    n_control,
    n_intervention
  )
)))

## ----nudge_clean_database-----------------------------------------------------
show(nudge_database_final)

## ----nudge_publication_badfilter----------------------------------------------
nudge_schema_relfiltered <- nudge_schema[
  !grepl("publication_id_", names(nudge_schema), fixed = TRUE) &
    !grepl("_publication_id", names(nudge_schema), fixed = TRUE) &
    !grepl("reference_", names(nudge_schema), fixed = TRUE) &
    !grepl("_reference", names(nudge_schema), fixed = TRUE)
]

## ----nudge_publication_badfilter_gv-------------------------------------------
show(nudge_schema_relfiltered)

## ----nudge_publication_badfilter_autoref--------------------------------------
identical(autoref(nudge_schema_relfiltered), nudge_schema_relfiltered)

## ----nudge_badfilter_example_fds----------------------------------------------
example_fds <- functional_dependency(
  list(
    list("title", "reference"),
    list(c("reference", "type_experiment"), "location"),
    list(c("title", "type_experiment"), "location")
  ),
  c("title", "reference", "type_experiment", "location")
)
example_fds

## ----nudge_badfilter_example_transitive---------------------------------------
show(normalise(example_fds, ensure_lossless = FALSE))

## ----nudge_badfilter_example_nontransitive------------------------------------
show(normalise(example_fds[-2], ensure_lossless = FALSE))

## ----nudge_approximate_cheat--------------------------------------------------
nudge_approx_cheat_database_schema <- discover(
  nudge,
  accuracy = 1 - 2/nrow(nudge),
  exclude = c("n_study", "n_comparison", "n_control", "n_intervention"),
  exclude_class = "numeric"
) |>
  normalise()
show(nudge_approx_cheat_database_schema)

## ----nudge_approximate--------------------------------------------------------
nudge_approx_database_schema <- discover(
  nudge,
  accuracy = 0.99,
  exclude = c("n_study", "n_comparison", "n_control", "n_intervention"),
  exclude_class = "numeric"
) |>
  normalise()
show(nudge_approx_database_schema)

## ----nudge_approximate_reduced------------------------------------------------
show(reduce(nudge_approx_database_schema, "es_id"))

## ----avoid_setup--------------------------------------------------------------
avoid_deps <- functional_dependency(
  list(
    list("A", "B"),
    list("B", "A"),
    list(c("A", "C"), "D"),
    list(c("A", "C"), "E"),
    list(c("B", "D"), "C")
  ),
  attrs_order = c("A", "B", "C", "D", "E")
)
avoid_deps
avoid_schema <- normalise(avoid_deps)
show(avoid_schema)

## ----avoid_remove-------------------------------------------------------------
avoid_schema_removed <- normalise(
  avoid_deps,
  remove_avoidable = TRUE
)
show(avoid_schema_removed)

## ----example_data_frame_with_interval_option----------------------------------
df_options <- data.frame(
  id = 1:20,
  value = c(2.3, 2.3, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
  lower_bound = c(NA_real_, NA_real_, NA_real_, 2.4, 0, 1, 0, 5.6, 2.4, 5.3, 5.3, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 5.6, 2.4),
  upper_bound = c(NA_real_, NA_real_, NA_real_, 7.1, 10, 10, 13.1, 25.8, 10, 13.1, 10, 25.8, 25.8, 25.8, 25.8,13.1, 13.1, 25.8, 25.8, 25.8),
  interval_distribution = c(NA, NA, NA, "uniform", "uniform", "uniform", "uniform", "uniform", "Beta", "Beta", "Beta", "Beta", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "PERT", "PERT", "PERT", "PERT"),
  param1 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 2, 2, 2.1, 2, 2, 2, 1, 2, 2),
  param2 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, 1, 1, NA, NA, NA, NA)
)
df_options$interval_distribution <- factor(df_options$interval_distribution)
knitr::kable(df_options)

## ----example_data_frame_with_interval_option_db-------------------------------
db_options <- autodb(df_options)
show(db_options)

## ----example_data_frame_with_interval_option_nulls----------------------------
df_options_with_presence <- data.frame(
  id = df_options$id,
  value = df_options$value,
  value_present = !is.na(df_options$value),
  lower_bound = df_options$lower_bound,
  lower_bound_present = !is.na(df_options$lower_bound),
  upper_bound = df_options$upper_bound,
  upper_bound_present = !is.na(df_options$upper_bound),
  interval_distribution = df_options$interval_distribution,
  interval_distribution_present = !is.na(df_options$interval_distribution),
  param1 = df_options$param1,
  param1_present = !is.na(df_options$param1),
  param2 = df_options$param2,
  param2_present = !is.na(df_options$param2)
)

## ----example_data_frame_with_interval_option_nulls_db-------------------------
db_options_with_presence <- autodb(df_options_with_presence)
show(db_options_with_presence)

## ----example_data_frame_with_interval_option_nulls_rel------------------------
knitr::kable(records(db_options_with_presence)$value_present)

## ----example_data_frame_with_interval_option_nulls_distribution_rel-----------
knitr::kable(records(db_options_with_presence)$interval_distribution)

## ----example_data_frame_with_interval_option_p1absent_db----------------------
db_options_with_presence_p1absent <- autodb(subset(
  df_options_with_presence,
  !param1_present
))
show(db_options_with_presence_p1absent)

## ----example_data_frame_with_interval_option_p1present_db---------------------
show(autodb(subset(
  df_options_with_presence,
  param1_present
)))

## ----example_data_frame_with_interval_option_p1absent_constants---------------
knitr::kable(records(db_options_with_presence_p1absent)$constants)

## ----example_data_frame_with_NAs----------------------------------------------
df_nas <- data.frame(
  patient = c(1L, 2L, 3L, 4L),
  trial_entry_date = as.Date(c("2022/05/02", "2022/06/06", "2022/04/01", "2022/03/19")),
  death_date = as.Date(c(NA, NA, "2022/10/07", NA))
)
knitr::kable(df_nas)

## ----example_data_frame_with_NAs_autodb---------------------------------------
show(autodb(df_nas))

## ----example_data_frame_with_NAs_nullably_normalised--------------------------
ideal_db <- decompose(
  df_nas,
  database_schema(
    relation_schema(
      list(
        patient = list(c("patient", "trial_entry_date"), list("patient")),
        patient_death = list(c("patient", "death_date"), list("patient"))
      ),
      names(df_nas)
    ),
    list(list("patient_death", "patient", "patient", "patient"))
  )
)
records(ideal_db)$patient_death <- subset(records(ideal_db)$patient_death, !is.na(death_date))
show(ideal_db)

## ----factor_example_datasets--------------------------------------------------
df_badmerge_int <- cbind(
  expand.grid(
    a = c(NA, 0L, 1L),
    b = c(NA, FALSE, TRUE)
  ),
  row = 1:9
)
df_badmerge_factor <- df_badmerge_int
df_badmerge_factor$a <- as.factor(df_badmerge_factor$a)
knitr::kable(df_badmerge_int)
df_badmerge_logical <- df_badmerge_int
df_badmerge_logical$a <- as.logical(df_badmerge_logical$a)
names(df_badmerge_logical)[[3]] <- "row2"
knitr::kable(df_badmerge_logical)

## ----factor_example_int_single------------------------------------------------
knitr::kable(merge(
  df_badmerge_int[, c("a", "row")],
  df_badmerge_logical[, c("a", "row2")]
))

## ----factor_example_int_single_symmetric--------------------------------------
knitr::kable(merge(
  df_badmerge_logical[, c("a", "row2")],
  df_badmerge_int[, c("a", "row")]
))

## ----factor_example_single----------------------------------------------------
knitr::kable(merge(
  df_badmerge_factor[, c("a", "row")],
  df_badmerge_logical[, c("a", "row2")]
))

## ----factor_example_single_symmetric------------------------------------------
knitr::kable(merge(
  df_badmerge_logical[, c("a", "row2")],
  df_badmerge_factor[, c("a", "row")]
))

## ----factor_example-----------------------------------------------------------
knitr::kable(merge(
  df_badmerge_factor,
  df_badmerge_logical
))

## ----factor_example_asymmetric------------------------------------------------
knitr::kable(merge(
  df_badmerge_logical,
  df_badmerge_factor
))

## ----redundant_keys_example---------------------------------------------------
fds_redkey <- functional_dependency(
  list(
    list("a", "b"),
    list("d", "c"),
    list(c("b", "d"), "a"),
    list("a", "c"),
    list(c("b", "c"), "d")
  ),
  letters[1:4]
)
fds_redkey

## ----redundant_keys_schema----------------------------------------------------
schema_redkey <- normalise(fds_redkey, remove_avoidable = TRUE)
show(schema_redkey)

## ----redundant_keys_fix-------------------------------------------------------
fds_redkey_fix <- functional_dependency(
  list(
    list("a", "b"),
    list("d", "c"),
    list(c("b", "c"), "a"),
    list("a", "d")
  ),
  letters[1:4]
)
fds_redkey_fix
schema_redkey_fix <- normalise(fds_redkey_fix, remove_avoidable = TRUE)
show(schema_redkey_fix)

## ----dup_example--------------------------------------------------------------
dup_db <- autodb(ChickWeight)
show(dup_db)

## ----dup_example_dup----------------------------------------------------------
show(dup_db[c(1, 1, 2, 2, 2)])

