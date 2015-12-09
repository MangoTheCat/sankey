
context("Making sankey plots")

test_that("make_sankey", {

  ne <- nodes_edges()
  result <- make_sankey(ne$nodes, ne$edges, y = "simple")

  expect_true(is.data.frame(result$nodes))
  expect_true(is.data.frame(result$edges))
  expect_true(all(
    c("name", "col", "size", "x", "shape", "lty", "srt", "textcol",
      "label", "adjx", "boxw", "cex", "bottom", "top", "center", "pos",
      "textx", "texty") %in% names(result$nodes)
  ))
  expect_true(all(
    c("from", "to", "colorstyle", "curvestyle",
      "col", "weight") %in% names(result$edges)
  ))

})

test_that("make_sankey handles nodes = NULL", {

  ne <- nodes_edges()
  ne$nodes <- data.frame(
    stringsAsFactors = FALSE,
    id = sort(ne$nodes[,1])
  )

  result1 <- make_sankey(ne$nodes, ne$edges, y = "simple")
  result2 <- make_sankey(edges = ne$edges, y = "simple")

  expect_equal(result1, result2)
})
