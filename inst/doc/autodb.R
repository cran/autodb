## ----include=FALSE------------------------------------------------------------
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
deps <- discover(ChickWeight, progress = TRUE)
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
titanic_deps_freqonly <- discover(as.data.frame(Titanic), exclude = "Freq")
titanic_deps_freqonly

## -----------------------------------------------------------------------------
identical(titanic_deps_freqonly, discover(as.data.frame(Titanic), exclude_class = "numeric"))

## -----------------------------------------------------------------------------
show(autodb(as.data.frame(Titanic), exclude = "Freq"))

## -----------------------------------------------------------------------------
titanic_deps <- discover(as.data.frame(Titanic))
titanic_deps

## -----------------------------------------------------------------------------
titanic_deps[dependant(titanic_deps) == "Freq"]

## -----------------------------------------------------------------------------
linked_schema <- autoref(schema)
linked_schema

## -----------------------------------------------------------------------------
normalise(deps)

## -----------------------------------------------------------------------------
show(linked_schema)

## ----chickWeight_db2_plot-----------------------------------------------------
db2 <- decompose(ChickWeight, linked_schema)
show(db2)

## ----chickWeights_rejoin------------------------------------------------------
rejoined <- rejoin(db)
summary(rejoined)
identical(rejoined, ChickWeight)
df_equiv(rejoined, ChickWeight)

## ----nudge_classes------------------------------------------------------------
show(nudge)

## ----nudge_database-----------------------------------------------------------
nudge_deps <- discover(
  nudge,
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
nudge_deps_size <- discover(nudge[, startsWith(names(nudge), "n_")])
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
show(normalise(discover(
  nudge,
  accuracy = 1 - 2/nrow(nudge),
  method = "DFD",
  exclude = c("n_study", "n_comparison", "n_control", "n_intervention"),
  exclude_class = "numeric"
)))

## ----nudge_approximate--------------------------------------------------------
nudge_approx_database_schema <- discover(
  nudge,
  accuracy = 0.99,
  method = "DFD",
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

## ----nudge, include=FALSE-----------------------------------------------------
autodb(
  nudge,
  remove_avoidable = TRUE,
  exclude_class = "numeric",
  exclude = "n_study"
) |>
  reduce(main = "study_id") |>
  show()

## ----nonconnected, include=FALSE----------------------------------------------
fds <- functional_dependency(
  list(
    list("a", "b"),
    list("a", "c"),
    list("b", "d"),
    list("c", "e"),
    list(c("d", "e"), "f")
  ),
  letters[1:6]
)
fds

## ----nonconnected2, include=FALSE---------------------------------------------
schema <- normalise(fds)
show(schema)

## -----------------------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz("digraph {
  rankdir = \"LR\"
  node [shape=plaintext];

  \"a\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">a</TD></TR>
    <TR><TD PORT=\"TO_a\">a</TD><TD PORT=\"FROM_a\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_b\">b</TD><TD PORT=\"FROM_b\"></TD></TR>
    <TR><TD PORT=\"TO_c\">c</TD><TD PORT=\"FROM_c\"></TD></TR>
    </TABLE>>];
  \"b\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">b</TD></TR>
    <TR><TD PORT=\"TO_b\">b</TD><TD PORT=\"FROM_b\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_d\">d</TD><TD PORT=\"FROM_d\"></TD></TR>
    </TABLE>>];
  \"c\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">c</TD></TR>
    <TR><TD PORT=\"TO_c\">c</TD><TD PORT=\"FROM_c\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_e\">e</TD><TD PORT=\"FROM_e\"></TD></TR>
    </TABLE>>];
  \"d_e\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">d_e</TD></TR>
    <TR><TD PORT=\"TO_d\">d</TD><TD PORT=\"FROM_d\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_e\">e</TD><TD PORT=\"FROM_e\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_f\">f</TD><TD PORT=\"FROM_f\"></TD></TR>
    </TABLE>>];
  \"view\" [label = <
    <TABLE BGCOLOR=\"lightgrey\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"1\">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    <TR><TD>f</TD></TR>
    </TABLE>>];

  \"a\":FROM_b -> \"b\":TO_b;
  \"a\":FROM_c -> \"c\":TO_c;

  \"view\":TITLE -> \"a\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view\":TITLE -> \"d_e\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
}")

## -----------------------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz("digraph {
  rankdir = \"LR\"
  node [shape=plaintext];

  \"a\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">a</TD></TR>
    <TR><TD PORT=\"TO_a\">a</TD><TD PORT=\"FROM_a\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_b\">b</TD><TD PORT=\"FROM_b\"></TD></TR>
    <TR><TD PORT=\"TO_c\">c</TD><TD PORT=\"FROM_c\"></TD></TR>
    </TABLE>>];
  \"b\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">b</TD></TR>
    <TR><TD PORT=\"TO_b\">b</TD><TD PORT=\"FROM_b\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_d\">d</TD><TD PORT=\"FROM_d\"></TD></TR>
    </TABLE>>];
  \"c\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">c</TD></TR>
    <TR><TD PORT=\"TO_c\">c</TD><TD PORT=\"FROM_c\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_e\">e</TD><TD PORT=\"FROM_e\"></TD></TR>
    </TABLE>>];
  \"d_e\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">d_e</TD></TR>
    <TR><TD PORT=\"TO_d\">d</TD><TD PORT=\"FROM_d\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_e\">e</TD><TD PORT=\"FROM_e\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_f\">f</TD><TD PORT=\"FROM_f\"></TD></TR>
    </TABLE>>];
  \"view\" [label = <
    <TABLE BGCOLOR=\"lightgrey\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"1\">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    <TR><TD>f</TD></TR>
    </TABLE>>];
  \"view2\" [label = <
    <TABLE BGCOLOR=\"lightgrey\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"1\">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    </TABLE>>];
  \"view3\" [label = <
    <TABLE BGCOLOR=\"lightgrey\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"1\">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    </TABLE>>];

  \"a\":FROM_b -> \"b\":TO_b;
  \"a\":FROM_c -> \"c\":TO_c;

  \"view\":TITLE -> \"view2\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view\":TITLE -> \"d_e\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view2\":TITLE -> \"view3\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view2\":TITLE -> \"c\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view3\":TITLE -> \"a\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view3\":TITLE -> \"b\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
}")

## ----bcnf, include=FALSE------------------------------------------------------
fds <- functional_dependency(
  list(list(c("a", "b"), "c"), list("c", "a")),
  letters[1:3]
)
fds
schema <- normalise(fds)
show(schema)

## ----bcnf2--------------------------------------------------------------------
db <- schema |>
  create() |>
  insert(data.frame(c = 1:2, a = 1:2), relations = "c") |>
  insert(data.frame(a = 1:2, b = 1L, c = 2:1), relations = "a_b")
knitr::kable(records(db)$a_b)
knitr::kable(records(db)$c)

## ----bcnf3, echo=FALSE--------------------------------------------------------
schema2 <- schema
keys(schema2)$a_b <- list(c("b", "c"))
attrs(schema2)$a_b <- c("b", "c")
names(schema2)[names(schema2) == "a_b"] <- "b_c"
show(schema2)

## -----------------------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz("digraph {
  rankdir = \"LR\"
  node [shape=plaintext];

  \"c\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">c</TD></TR>
    <TR><TD PORT=\"TO_c\">c</TD><TD PORT=\"FROM_c\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_a\">a</TD><TD PORT=\"FROM_a\"></TD></TR>
    </TABLE>>];
  \"b_c\" [label = <
    <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">b_c</TD></TR>
    <TR><TD PORT=\"TO_b\">b</TD><TD PORT=\"FROM_b\" BGCOLOR=\"black\"></TD></TR>
    <TR><TD PORT=\"TO_c\">c</TD><TD PORT=\"FROM_c\" BGCOLOR=\"black\"></TD></TR>
    </TABLE>>];
  \"view\" [label = <
    <TABLE BGCOLOR=\"lightgrey\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">
    <TR><TD PORT=\"TITLE\" COLSPAN=\"2\">view</TD></TR>
    <TR><TD>a</TD><TD BGCOLOR=\"black\"></TD></TR>
    <TR><TD>b</TD><TD BGCOLOR=\"black\"></TD></TR>
    <TR><TD>c</TD><TD></TD></TR>
    </TABLE>>];

  \"b_c\":FROM_c -> \"c\":TO_c;

  \"view\":TITLE -> \"b_c\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
  \"view\":TITLE -> \"c\":TITLE [arrowhead=\"empty\" style=\"dashed\"];
}")

## ----example_data_frame_with_NAs----------------------------------------------
df_nas <- data.frame(
  patient = c(1L, 2L, 3L, 4L),
  trial_entry_date = as.Date(c("2022/05/02", "2022/06/06", "2022/04/01", "2022/03/19")),
  trial_exit_date = as.Date(c(NA, NA, "2022/10/07", NA))
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
        patient_exit = list(c("patient", "trial_exit_date"), list("patient"))
      ),
      names(df_nas)
    ),
    list(list("patient_exit", "patient", "patient", "patient"))
  )
)
records(ideal_db)$patient_exit <- subset(records(ideal_db)$patient_exit, !is.na(trial_exit_date))
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
show(db_options)

## ----example_data_frame_with_interval_option_nulls----------------------------
df_options_presence <- df_options[vapply(df_options, anyNA, logical(1))]
df_options_presence[] <- lapply(df_options_presence, Negate(is.na))
names(df_options_presence) <- paste0(names(df_options_presence), "_present")
df_options_with_presence <- cbind(df_options, df_options_presence)

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

## ----example_data_frame_with_interval_option_p1absent_constants---------------
knitr::kable(records(db_options_with_presence_p1absent)$constants)

## ----example_data_frame_with_interval_option_p1present_db---------------------
show(autodb(subset(
  df_options_with_presence,
  param1_present
)))

## -----------------------------------------------------------------------------
show(database_schema(
  relation_schema(
    list(
      citation = list(c("citer_id", "citee_id"), list(c("citer_id", "citee_id"))),
      citer = list(c("citer_id", "citer_title", "citer_author", "citer_year"), list("citer_id")),
      citee = list(c("citee_id", "citee_title", "citee_author", "citee_year"), list("citee_id"))
    ),
    c(
      "citer_id", "citer_title", "citer_author", "citer_year",
      "citee_id", "citee_title", "citee_author", "citee_year"
    )
  ),
  list(
    list("citation", "citer_id", "citer", "citer_id"),
    list("citation", "citee_id", "citee", "citee_id")
  )
))

## -----------------------------------------------------------------------------
show(database_schema(
  relation_schema(
    list(
      citation = list(c("citer", "citee"), list(c("citer", "citee"))),
      publication = list(c("id", "title", "author", "year"), list("id"))
    ),
    c("citer", "citee", "id", "title", "author", "year")
  ),
  list(
    list("citation", "citer", "publication", "id"),
    list("citation", "citee", "publication", "id")
  )
))

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

