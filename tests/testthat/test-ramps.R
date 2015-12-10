
context("Color ramps")

test_that("color_ramp_alpha is ok", {

  r1 <- colorRamp(c("red", "green"))( (0:4)/4 )
  r2 <- color_ramp_alpha(c("red", "green"))( (0:4)/4 )

  expect_equal(r1, r2[,1:3])
  expect_equal(r2[,4], rep(255, 5))

  r3 <- colorRamp(c("red", "green"), interpolate = "spline")( (0:4)/4 )
  r4 <- color_ramp_alpha(c("red", "green"), interpolate = "spline")( (0:4)/4 )

  expect_equal(r1, r2[,1:3])
  expect_equal(r2[,4], rep(255, 5))
})

test_that("color_ramp_palette_alpha is ok", {

  p1 <- colorRampPalette(c("blue", "red"))(4)
  p2 <- color_ramp_palette_alpha(c("blue", "red"))(4)

  expect_equal(paste0(p1, "FF"), p2)
})
