test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("describe_na_value works", {
  no_na_tbl <- tibble::tibble(col1 = c(1, 2),
                      col2 = c(0.5, 0.4),
                      col3 = c("a", "b"))

  numerical_na_tbl <- tibble::tibble(col1 = c(1, 2),
                             col2 = c(NaN, 0.4),
                             col3 = c("a", "b"))

  cat_na_tbl <- tibble::tibble(col1 = c(1, 2),
                        col2 = c(0.5, 0.4),
                        col3 = c(NA_character_, "b"))

  expect_true(dplyr::all_equal(edar::describe_na_values(no_na_tbl), tibble::tibble(col1 = c(1, 1),
                                                                    col2 = c(1, 1),
                                                                    col3 = c(1, 1))))

  expect_true(dplyr::all_equal(edar::describe_na_values(numerical_na_tbl), tibble::tibble(col1 = c(1, 1),
                                                                    col2 = c(0, 1),
                                                                    col3 = c(1, 1))))

  expect_true(dplyr::all_equal(edar::describe_na_values(cat_na_tbl), tibble::tibble(col1 = c(1, 1),
                                                                    col2 = c(1, 1),
                                                                    col3 = c(0, 1))))


})
