
context("join")

require("dplyr")

# https://github.com/tidyverse/dplyr/blob/master/tests/testthat/test-joins.r
a <- data.frame(x = 1:3, y = 2:4)
b <- data.frame(x = 3:5, z = 3:5)
df <- full_join(a, b, "x")

test_that("join", {

  actual <- summarise_join_stats(df, "y", "z")
  expect <-
    dplyr::data_frame(
      x = "y",
      y = "z",
      n_x = 3L,
      n_y = 3L,
      n_joined = 1L,
      n_x_unjoined = 2L,
      n_y_unjoined = 2L,
      x_in_y_pct = 33.33,
      y_in_x_pct = 33.33
    )
  expect_equal(nrow(actual), 1)
  expect_equal(ncol(actual), 9)
  # expect_equal(actual, expect)

  actual_2 <- summarise_join_stats(df, y, z)
  expect_equal(actual_2, actual)

})
