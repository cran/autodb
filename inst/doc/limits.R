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

## -----------------------------------------------------------------------------
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
) |>
  database_schema(
    list(
      list("citation", "citer_id", "citer", "citer_id"),
      list("citation", "citee_id", "citee", "citee_id")
    )
  ) |>
  show()

## -----------------------------------------------------------------------------
database_schema(
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
) |>
  show()

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
normalise(fds_redkey, remove_avoidable = TRUE)

## -----------------------------------------------------------------------------
show(normalise(fds_redkey, remove_avoidable = TRUE))

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

## -----------------------------------------------------------------------------
show(schema_redkey_fix)

## ----dup_example--------------------------------------------------------------
dup_db <- autodb(ChickWeight)

## -----------------------------------------------------------------------------
show(dup_db)

## -----------------------------------------------------------------------------
show(dup_db[c(1, 1, 2, 2, 2)])

