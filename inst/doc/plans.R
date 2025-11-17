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

## ----bcnf---------------------------------------------------------------------
fds <- functional_dependency(
  list(list(c("a", "b"), "c"), list("c", "a")),
  letters[1:3]
)
fds
schema <- normalise(fds)

## -----------------------------------------------------------------------------
show(schema)

## ----bcnf2--------------------------------------------------------------------
db <- schema |>
  create() |>
  insert(data.frame(c = 1:2, a = 1:2), relations = "c") |>
  insert(data.frame(a = 1:2, b = 1L, c = 2:1), relations = "a_b")
rab <- records(db)$a_b
rc <- records(db)$c
knitr::kable(rab)
knitr::kable(rc)
knitr::kable(merge(rab, rc, by = "c", suffixes = c(".a_b", ".c")))

## ----bcnf3, echo=FALSE--------------------------------------------------------
schema2 <- schema
keys(schema2)$a_b <- list(c("b", "c"))
attrs(schema2)$a_b <- c("b", "c")
names(schema2)[names(schema2) == "a_b"] <- "b_c"

## -----------------------------------------------------------------------------
show(schema2)

## ----nonconnected-------------------------------------------------------------
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

## -----------------------------------------------------------------------------
show(normalise(fds))

## ----echo=FALSE---------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz(
'digraph {
  rankdir = "LR"
  node [shape=plaintext];

  "c" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">c</TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_a">a</TD><TD PORT="FROM_a"></TD></TR>
    </TABLE>>];
  "b_c" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">b_c</TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c" BGCOLOR="black"></TD></TR>
    </TABLE>>];
  "view" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">view</TD></TR>
    <TR><TD>a</TD><TD BGCOLOR="black"></TD></TR>
    <TR><TD>b</TD><TD BGCOLOR="black"></TD></TR>
    <TR><TD>c</TD><TD></TD></TR>
    </TABLE>>];

  "b_c":FROM_c -> "c":TO_c;

  "view":TITLE -> "b_c":TITLE [arrowhead="empty" style="dashed"];
}
')

## ----echo=FALSE---------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz(
'digraph {
  rankdir = "LR"
  node [shape=plaintext];

  "a" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">a</TD></TR>
    <TR><TD PORT="TO_a">a</TD><TD PORT="FROM_a" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b"></TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c"></TD></TR>
    </TABLE>>];
  "b" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">b</TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d"></TD></TR>
    </TABLE>>];
  "c" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">c</TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e"></TD></TR>
    </TABLE>>];
  "d_e" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">d_e</TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_f">f</TD><TD PORT="FROM_f"></TD></TR>
    </TABLE>>];
  "view" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="1">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    <TR><TD>f</TD></TR>
    </TABLE>>];

  "a":FROM_b -> "b":TO_b;
  "a":FROM_c -> "c":TO_c;

  "view":TITLE -> "a":TITLE [arrowhead="empty" style="dashed"];
  "view":TITLE -> "d_e":TITLE [arrowhead="empty" style="dashed"];
}
')

## ----echo=FALSE---------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz(
'digraph {
  rankdir = "LR"
  node [shape=plaintext];

  "a" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">a</TD></TR>
    <TR><TD PORT="TO_a">a</TD><TD PORT="FROM_a" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b"></TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c"></TD></TR>
    </TABLE>>];
  "b" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">b</TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d"></TD></TR>
    </TABLE>>];
  "c" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">c</TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e"></TD></TR>
    </TABLE>>];
  "d_e" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">d_e</TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_f">f</TD><TD PORT="FROM_f"></TD></TR>
    </TABLE>>];
  "view" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="1">view</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    <TR><TD>f</TD></TR>
    </TABLE>>];

  "a":FROM_b -> "b":TO_b;
  "a":FROM_c -> "c":TO_c;

  "view":TITLE -> "b":TITLE [arrowhead="empty" style="dashed"];
  "view":TITLE -> "c":TITLE [arrowhead="empty" style="dashed"];
  "view":TITLE -> "d_e":TITLE [arrowhead="empty" style="dashed"];
}
')

## ----echo=FALSE---------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz(
'digraph {
  rankdir = "LR"
  node [shape=plaintext];

  "a" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">a</TD></TR>
    <TR><TD PORT="TO_a">a</TD><TD PORT="FROM_a" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b"></TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c"></TD></TR>
    </TABLE>>];
  "b" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">b</TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d"></TD></TR>
    </TABLE>>];
  "c" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">c</TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e"></TD></TR>
    </TABLE>>];
  "d_e" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">d_e</TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_f">f</TD><TD PORT="FROM_f"></TD></TR>
    </TABLE>>];
  "view" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="1">view</TD></TR>
    <TR><TD PORT="TO_b">b</TD></TR>
    <TR><TD PORT="TO_c">c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    <TR><TD>f</TD></TR>
    </TABLE>>];

  "a":FROM_b -> "view":TO_b;
  "a":FROM_c -> "view":TO_c;

  "view":TITLE -> "b":TITLE [arrowhead="empty" style="dashed"];
  "view":TITLE -> "c":TITLE [arrowhead="empty" style="dashed"];
  "view":TITLE -> "d_e":TITLE [arrowhead="empty" style="dashed"];
}
')

## ----echo=FALSE---------------------------------------------------------------
if (requireNamespace("DiagrammeR", quietly = TRUE)) DiagrammeR::grViz(
'digraph {
  rankdir = "LR"
  node [shape=plaintext];

  "a" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">a</TD></TR>
    <TR><TD PORT="TO_a">a</TD><TD PORT="FROM_a" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b"></TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c"></TD></TR>
    </TABLE>>];
  "b" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">b</TD></TR>
    <TR><TD PORT="TO_b">b</TD><TD PORT="FROM_b" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d"></TD></TR>
    </TABLE>>];
  "c" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">c</TD></TR>
    <TR><TD PORT="TO_c">c</TD><TD PORT="FROM_c" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e"></TD></TR>
    </TABLE>>];
  "d_e" [label = <
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="2">d_e</TD></TR>
    <TR><TD PORT="TO_d">d</TD><TD PORT="FROM_d" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_e">e</TD><TD PORT="FROM_e" BGCOLOR="black"></TD></TR>
    <TR><TD PORT="TO_f">f</TD><TD PORT="FROM_f"></TD></TR>
    </TABLE>>];
  "view" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="1">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    <TR><TD>f</TD></TR>
    </TABLE>>];
  "view2" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="1">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    <TR><TD>e</TD></TR>
    </TABLE>>];
  "view3" [label = <
    <TABLE BGCOLOR="lightgrey" BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    <TR><TD PORT="TITLE" COLSPAN="1">view</TD></TR>
    <TR><TD>a</TD></TR>
    <TR><TD>b</TD></TR>
    <TR><TD>c</TD></TR>
    <TR><TD>d</TD></TR>
    </TABLE>>];

  "a":FROM_b -> "b":TO_b;
  "a":FROM_c -> "c":TO_c;

  "view":TITLE -> "view2":TITLE [arrowhead="empty" style="dashed"];
  "view":TITLE -> "d_e":TITLE [arrowhead="empty" style="dashed"];
  "view2":TITLE -> "view3":TITLE [arrowhead="empty" style="dashed"];
  "view2":TITLE -> "c":TITLE [arrowhead="empty" style="dashed"];
  "view3":TITLE -> "a":TITLE [arrowhead="empty" style="dashed"];
  "view3":TITLE -> "b":TITLE [arrowhead="empty" style="dashed"];
}
')

