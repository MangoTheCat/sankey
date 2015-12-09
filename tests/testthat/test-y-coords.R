
context("Automatic y coords")

test_that("optimize_y", {

  ne <- nodes_edges()
  ne$nodes$x    <- optimize_x    (ne$nodes, ne$edges)
  ne$nodes$size <- optimize_sizes(ne$nodes, ne$edges)

  result <- optimize_y_simple(ne$nodes, ne$edges)

  expect_equal(result[,1:3], ne$nodes)
  expect_equal(
    result$bottom,
    c(-72.6, -66.8, -61, -55.2, -49.4, -41.6, -35.8, 0, 0, -30, -23.2,
      -17.4, -11.6, -5.8, 0, -30, -24.2, -17.4, -11.6, -5.8, 0)
  )
  expect_equal(
    result$top,
    c(-73.6, -67.8, -62, -56.2, -50.4, -44.6, -36.8, -1, -15, -31,
      -25.2, -18.4, -12.6, -6.8, -1, -31, -25.2, -19.4, -12.6, -6.8, -1)
  )
  expect_equal(
    result$center,
    c(-73.1, -67.3, -61.5, -55.7, -49.9, -43.1, -36.3, -0.5, -7.5,
      -30.5, -24.2, -17.9, -12.1, -6.3, -0.5, -30.5, -24.7, -18.4,
      -12.1, -6.3, -0.5)
  )
})
