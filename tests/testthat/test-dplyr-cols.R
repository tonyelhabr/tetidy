
context("dplyr-cols")
require("datasets")
require("dplyr")
require("tibble")
# testthat::test_file("tests/testthat/test-dplyr-cols.R")

test_that("`add_rnk_col()` works as expected", {

  actual <- add_rnk_col(mtcars, "mpg", arrange = TRUE, pretty = TRUE)
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    mutate(rnk = row_number(desc(mpg))) %>%
    arrange(rnk)
  expect_equal(actual, expect)

  # Test NSE version.
  actual <- add_rnk_col(mtcars, mpg)
  expect_equal(actual, expect)

  # Test `pretty = FALSE"`.
  actual <- add_rnk_col(mtcars, mpg, arrange = TRUE, pretty = FALSE)
  expect <- mtcars
  expect <- with(expect, expect[order(-mpg),])
  expect$rnk <- seq(1:nrow(expect))
  expect <- expect[, c(names(mtcars), "rnk")]
  rownames(expect) <- NULL
  expect_equal(actual, expect)

  # Test `arrange = FALSE`.
  actual <- add_rnk_col(mtcars, "mpg", arrange = FALSE, pretty = TRUE)
  actual$n_row  <- seq(1:nrow(actual))
  expect <- mtcars
  expect$n_row <- seq(1:nrow(expect))
  rownames(expect) <- NULL
  joined <- merge(actual, expect, by = "n_row")
  expect_false(any(actual$rnk[1:2] == actual$n_row[1:2]))
  expect_equal(nrow(actual), nrow(joined))
  expect_equal(nrow(expect), nrow(joined))

  # Test `col_out != "rnk"`.
  actual <- add_rnk_col(mtcars, "mpg", col_out = "dummy", arrange = TRUE, pretty = TRUE)
  expect <- mtcars
  expect <- with(expect, expect[order(-mpg),])
  expect$dummy <- seq(1:nrow(expect))
  expect <- expect[, c("dummy", names(mtcars))]
  rownames(expect) <- NULL
  expect_equal(actual, expect)

})
