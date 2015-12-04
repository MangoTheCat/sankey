
context("Making sankey plots")

test_that("make_sankey", {

  ne <- nodes_edges()
  result <- make_sankey(ne$nodes, ne$edges)

  ## TODO
})
