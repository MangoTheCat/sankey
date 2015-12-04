
context("Automatic y coords")

test_that("optimize_y", {

  ne <- nodes_edges()
  ne$nodes$x    <- optimize_x    (ne$nodes, ne$edges)
  ne$nodes$size <- optimize_sizes(ne$nodes, ne$edges)

  result <- optimize_y(ne$nodes, ne$edges)

  expect_equal(result[,1:3], ne$nodes)
  expect_equal(
    result$bottom,
    c(-34.2, -31.6, -29, -26.4, -23.8, -19.2, -16.6, 0, 0, -14, -10.4,
      -7.8, -5.2, -2.6, 0, -14, -11.4, -7.8, -5.2, -2.6, 0)
  )
  expect_equal(
    result$top,
    c(-35.2, -32.6, -30, -27.4, -24.8, -22.2, -17.6, -1, -15, -15,
      -12.4, -8.8, -6.2, -3.6, -1, -15, -12.4, -9.8, -6.2, -3.6, -1)
  )
  expect_equal(
    result$center,
    c(-34.7, -32.1, -29.5, -26.9, -24.3, -20.7, -17.1, -0.5, -7.5,
      -14.5, -11.4, -8.3, -5.7, -3.1, -0.5, -14.5, -11.9, -8.8, -5.7,
      -3.1, -0.5)
  )
})
