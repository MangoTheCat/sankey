
context("Sankey plot")

## We cannot really test this properly until we'll have
## a robust image fingerprinting package on CRAN.

test_that("sankey plots nicely", {

  ne <- nodes_edges()
  x <- make_sankey(ne$nodes, ne$edges, break_edges = FALSE)
  png(tmp <- tempfile())
  sankey(x)
  dev.off()
})
