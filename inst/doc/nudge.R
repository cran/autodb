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

## ----nudge_classes------------------------------------------------------------
knitr::kable(data.frame(
  attribute = names(nudge),
  class = vapply(nudge, \(x) class(x)[[1]], character(1)),
  NAs = vapply(nudge, \(x) sum(is.na(x)), integer(1)),
  row.names = NULL
))

## ----nudge_database_big-------------------------------------------------------
nudge_deps_big <- discover(nudge)
nudge_schema_big <- normalise(nudge_deps_big, remove_avoidable = TRUE)
nudge_db_big <- decompose(nudge, nudge_schema_big)

## ----nudge_database_big_length------------------------------------------------
length(nudge_schema_big)

## ----nudge_fds_big_length-----------------------------------------------------
length(nudge_deps_big)

## ----nudge_reduce_big---------------------------------------------------------
nudge_reduced_big <- reduce(nudge_db_big)

## ----nudge_reduce_big_length--------------------------------------------------
length(nudge_reduced_big)

## ----nudge_fds_big_size-------------------------------------------------------
table(lengths(detset(nudge_deps_big)))

## ----nudge_fds_big_freq-------------------------------------------------------
sort(table(unlist(detset(nudge_deps_big))), decreasing = TRUE)

## ----nudge_fds_big_freq_by_size-----------------------------------------------
sort_by_rowSums <- function(x, ...) x[order(rowSums(x), ...), , drop = FALSE]
level_table <- function(x, levels) table(factor(x, levels))
attrs_table <- function(x) level_table(unlist(x), names(nudge))
by_lengths <- function(x, f) do.call(cbind, tapply(x, lengths(x), f))

by_lengths(detset(nudge_deps_big), attrs_table) |>
  sort_by_rowSums(decreasing = TRUE)

## ----nudge_fds_big_class_freq_by_size-----------------------------------------
attr_classes <- vapply(nudge, \(x) class(x)[[1]], character(1))
class_table <- function(x) {
  level_table(attr_classes[unlist(x)], sort(unique(attr_classes)))
}

by_lengths(detset(nudge_deps_big), class_table) |>
  sort_by_rowSums(decreasing = TRUE)

## ----nudge_fds_big_class_filter-----------------------------------------------
det_nofloat <- vapply(
  detset(nudge_deps_big),
  \(x) all(attr_classes[x] != "numeric"),
  logical(1)
)
summary(det_nofloat)

## ----nudge_schema_big_filtered------------------------------------------------
length(normalise(nudge_deps_big[det_nofloat]))

## ----nudge_db_big_filtered----------------------------------------------------
nudge_schema_filtered <- normalise(nudge_deps_big[det_nofloat])
nudge_db_filtered <- reduce(decompose(nudge, nudge_schema_filtered))

## ----nudge_gv_big_filtered----------------------------------------------------
show(nudge_db_filtered)

## ----nudge_db_big_filtered_sample1--------------------------------------------
subsample <- c(
  "es_id",
  "study_id",
  "intervention_technique_n_comparison_n_control",
  "intervention_technique",
  "n_comparison_n_control"
)

## -----------------------------------------------------------------------------
show(nudge_db_filtered[subsample])

## ----nudge_sample_sizes-------------------------------------------------------
knitr::kable(
  subset(
    nudge,
    n_comparison != n_control + n_intervention,
    c(reference, study_id, es_id, n_study, n_comparison, n_control, n_intervention)
  ),
  row.names = FALSE
)

## -----------------------------------------------------------------------------
show(nudge_db_filtered[setdiff(names(nudge_db_filtered), subsample)])

## -----------------------------------------------------------------------------
show(nudge_db_filtered[c("title", "reference")])

## ----dupfun-------------------------------------------------------------------
duplicates <- function(x) unique(x[duplicated(x)])
subset_duplicates <- function(x, attr) {
  x[x[[attr]] %in% duplicates(x[[attr]]), , drop = FALSE]
}

## ----nudge_duplicates_check---------------------------------------------------
nudge_title_relation <- records(nudge_db_filtered)$title
knitr::kable(subset_duplicates(nudge_title_relation, "publication_id"))
knitr::kable(subset_duplicates(nudge_title_relation, "reference"))

## -----------------------------------------------------------------------------
nudge_fixed <- within(nudge, {
  publication_id[publication_id == 95 & year == 2015] <- max(publication_id) + 1L
  reference[publication_id == 19] <- "BETA (2018a)"
})

## -----------------------------------------------------------------------------
db_fixed <- autodb(
  nudge_fixed,
  exclude_class = "numeric"
)
length(db_fixed)

## -----------------------------------------------------------------------------
show(reduce(db_fixed))

## -----------------------------------------------------------------------------
db_final <- autodb(
  nudge_fixed,
  exclude_class = "numeric",
  detset_limit = 2
)
length(db_final)

## -----------------------------------------------------------------------------
show(db_final)

## -----------------------------------------------------------------------------
show(reduce(db_final))

## -----------------------------------------------------------------------------
show(reduce(autodb(
  nudge_fixed,
  exclude = c("type_experiment", "n_study"),
  exclude_class = "numeric",
  detset_limit = 2
)))

## ----nudge_gv_big_filtered----------------------------------------------------
show(nudge_db_filtered)

## ----nudge_publication_badfilter----------------------------------------------
nudge_schema_relfiltered <- nudge_schema_filtered[
  vapply(
    keys(nudge_schema_filtered),
    \(ks) all(lengths(ks) <= 2) &&
      sum(c("publication_id", "reference") %in% ks[[1]]) != 1,
    logical(1)
  )
]

## -----------------------------------------------------------------------------
show(nudge_schema_relfiltered)

## -----------------------------------------------------------------------------
show(autoref(nudge_schema_relfiltered))

## ----hierarchical_levels------------------------------------------------------
hlev <- c(
  publication_id = 3,
  study_id = 2,
  es_id = 1,
  reference = 3,
  title = 3,
  year = 3,
  location = 2,
  domain = 2,
  intervention_category = 2,
  intervention_technique = 2,
  type_experiment = 2,
  population = 2,
  n_study = 2,
  n_comparison = 1,
  n_control = 1,
  n_intervention = 1,
  binary_outcome = 1,
  mean_control = 1,
  sd_control = 1,
  mean_intervention = 1,
  sd_intervention = 1,
  cohens_d = 1,
  variance_d = 1,
  approximation = 1,
  wansink = 2
)
hfilter <- function(fds, hlev) {
  fds[mapply(
    \(det, dep) all(hlev[det] <= hlev[[dep]]),
    detset(fds),
    dependant(fds)
  )]
}

## -----------------------------------------------------------------------------
nudge |>
  discover(exclude_class = "numeric", detset_limit = 2) |>
  hfilter(hlev) |>
  normalise(remove_avoidable = TRUE) |>
  decompose(df = nudge) |>
  reduce() |>
  show()

## -----------------------------------------------------------------------------
nudge_fixed |>
  discover(exclude_class = "numeric", detset_limit = 2) |>
  hfilter(hlev) |>
  normalise(remove_avoidable = TRUE) |>
  decompose(df = nudge_fixed) |>
  show()

