## ----setup--------------------------------------------------------------------
library(autodb)

## -----------------------------------------------------------------------------
summary(ChickWeight)

## -----------------------------------------------------------------------------
measurement <- unique(subset(ChickWeight, , -Diet))
chick <- unique(subset(ChickWeight, , c(Chick, Diet)))
summary(measurement)
summary(chick)
stopifnot(
  identical(
    merge(measurement, chick, sort = FALSE)[names(ChickWeight)],
    data.frame(ChickWeight)
  ),
  setequal(measurement$Chick, chick$Chick)
)

## -----------------------------------------------------------------------------
chick_db <- autodb(ChickWeight)
chick_db

## -----------------------------------------------------------------------------
cat(gv(chick_db))

## ----check_diagrammer---------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  show <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
  maybe_plot <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
}else{
  show <- print
  maybe_plot <- function(x) invisible(NULL)
}

## -----------------------------------------------------------------------------
show(chick_db)

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
show(schema)

## -----------------------------------------------------------------------------
linked_schema <- autoref(schema)
linked_schema

## -----------------------------------------------------------------------------
normalise(deps)

## -----------------------------------------------------------------------------
show(linked_schema)

## ----chickWeight_db2_plot-----------------------------------------------------
db <- decompose(ChickWeight, linked_schema)

## -----------------------------------------------------------------------------
show(db)

## ----chickWeights_rejoin------------------------------------------------------
rejoined <- rejoin(db)
summary(rejoined)
identical(rejoined, ChickWeight)
df_equiv(rejoined, ChickWeight)

## -----------------------------------------------------------------------------
knitr::kable(as.data.frame(Titanic))

## -----------------------------------------------------------------------------
show(autodb(as.data.frame(Titanic)))

## -----------------------------------------------------------------------------
titanic_deps_freqonly <- discover(as.data.frame(Titanic), exclude = "Freq")
titanic_deps_freqonly

## -----------------------------------------------------------------------------
stopifnot(setequal(
  titanic_deps_freqonly,
  discover(as.data.frame(Titanic), exclude_class = "numeric")
))

## -----------------------------------------------------------------------------
show(autodb(as.data.frame(Titanic), exclude = "Freq"))

## -----------------------------------------------------------------------------
titanic_deps <- discover(as.data.frame(Titanic))
titanic_deps

## -----------------------------------------------------------------------------
titanic_deps[dependant(titanic_deps) == "Age"]

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
normalise(avoid_deps)

## -----------------------------------------------------------------------------
show(normalise(avoid_deps))

## -----------------------------------------------------------------------------
normalise(
  avoid_deps,
  remove_avoidable = TRUE
) |>
  show()

