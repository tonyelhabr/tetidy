
context("dplyr-cols")
require("datasets")
require("dplyr")
require("tibble")
# testthat::test_file("tests/testthat/test-dplyr-cols.R")

testthat::test_that("`add_rnk_col()` works as expected with default values", {

  # Test SE version. Also, explicitly set default `arrange` and `pretty` to default values.
  actual <- add_rnk_col(mtcars, "mpg", arrange = TRUE, pretty = TRUE)
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::mutate(rnk = dplyr::row_number(dplyr::desc(mpg))) %>%
    dplyr::arrange(rnk) %>%
    dplyr::select(rnk, dplyr::everything())
  testthat::expect_equal(actual, expect)

  # Test NSE version.
  actual <- add_rnk_col(mtcars, mpg, arrange = TRUE, pretty = TRUE)
  testthat::expect_equal(actual, expect)

})

testthat::test_that("`add_rnk_col()` works as expected with non-default values", {
  # Test `pretty = FALSE"`.
  actual <- add_rnk_col(mtcars, mpg, arrange = TRUE, pretty = FALSE)
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::mutate(rnk = dplyr::row_number(dplyr::desc(mpg))) %>%
    dplyr::arrange(rnk)
  testthat::expect_equal(actual, expect)

  # Test `arrange = FALSE`.
  actual <- add_rnk_col(mtcars, "mpg", arrange = FALSE, pretty = TRUE)
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::mutate(rnk = dplyr::row_number(dplyr::desc(mpg))) %>%
    dplyr::select(rnk, dplyr::everything())
  testthat::expect_equal(actual, expect)

  # Test `col_out != "rnk"`.
  actual <- add_rnk_col(mtcars, "mpg", col_out = "dummy", arrange = TRUE, pretty = TRUE)
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dummy = dplyr::row_number(dplyr::desc(mpg))) %>%
    dplyr::select(dummy, dplyr::everything()) %>%
    dplyr::arrange(dummy)
  testthat::expect_equal(actual, expect)

})
