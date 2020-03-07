test_that("describe_na_value works", {
  no_na_tbl <- tibble::tibble(
    col1 = c(1, 2),
    col2 = c(0.5, 0.4),
    col3 = c("a", "b")
  )

  numerical_na_tbl <- tibble::tibble(
    col1 = c(1, 2),
    col2 = c(NaN, 0.4),
    col3 = c("a", "b")
  )

  cat_na_tbl <- tibble::tibble(
    col1 = c(1, 2),
    col2 = c(0.5, 0.4),
    col3 = c(NA_character_, "b")
  )

  not_a_tbl <- list(col1 = c(1, 2),
                    col2 = c(0.5, 0.4),
                    col3 = c("a", "b"))


  expect_error(describe_na_values(not_a_tbl),
               regexp = "The value of the argument 'dataframe' should be of type  'data.frame' or 'tibble'.")

  expect_true(dplyr::all_equal(
    edar::describe_na_values(no_na_tbl),
    tibble::tibble(
      col1 = c(1, 1),
      col2 = c(1, 1),
      col3 = c(1, 1)
    )
  ))

  expect_true(dplyr::all_equal(
    edar::describe_na_values(numerical_na_tbl),
    tibble::tibble(
      col1 = c(1, 1),
      col2 = c(0, 1),
      col3 = c(1, 1)
    )
  ))

  expect_true(dplyr::all_equal(
    edar::describe_na_values(cat_na_tbl),
    tibble::tibble(
      col1 = c(1, 1),
      col2 = c(1, 1),
      col3 = c(0, 1)
    )
  ))


})

#' Tests the correlation function to make sure output is rendered correctly
#' or the function will fail with an error message and problem.
#'
#' @return None. the function will not throw an error
#' if the tests fail.
#'
#' @examples
#' test_cal_cor()
test_cal_cor <- function(){
  data <- helper_create_data(200)
  num_var <- c('num1', 'num2', 'num3')
  p <- calc_cor(data, num_var)

  test_that("The returned plot should be a ggplot object.", {
    expect_true(ggplot2::is.ggplot(p))
  })

  test_that('Plot should use geomtile and geomrect.', {
    expect_true("GeomTile" %in% c(class(p$layers[[1]]$geom)))
    expect_true("GeomRect" %in% c(class(p$layers[[1]]$geom)))
    expect_true("Layer" %in% c(class(rlang::get_expr(p$layers[[1]]))))
  })

  test_that('Overlapping plot shoud use geomtext.', {
    expect_true("GeomText" %in% c(class(rlang::get_expr(p$layers[[2]]$geom))))
  })

  test_that('Plot should map Var1 to x-axis, and Var2 to y-axis.', {
    expect_true("Var1"  == rlang::get_expr(p$mapping$x))
    expect_true("Var2" == rlang::get_expr(p$mapping$y))
  })

  test_that('Variables for layer and base should be the same', {
    expect_true(rlang::get_expr(p$layers[[2]]$mapping$x) == rlang::get_expr(p$mapping$x))
    expect_true(rlang::get_expr(p$layers[[2]]$mapping$y) == rlang::get_expr(p$mapping$y))
  })

  test_that('All values should be between -1 and 1', {
    expect_true(all(p[[1]][3]) <= 1 & all(p[[1]][3] >= -1))
  })

  test_that('Var1 should not equal Var2', {
    expect_true(all(p[[1]][[1]] != p[[1]][[2]]))
  })

  test_that("Corresponding error message should be expected if the dataframe argument is not a dataframe.", {
    expect_error(calc_cor("abc", num_var),
                 regexp = "Input 'df' should be a dataframe.")
  })

  test_that("Corresponding error message should be expected if the column values are not numeric.", {
    expect_error(calc_cor(data, c("cat1")),
                 regexp = "Columns do not all contain numeric values.")
  })
}
test_cal_cor()

#' Tests the describe_num_var function to make sure outputs are correct
#' or the function will fail with the correct error message.
#'
#' @return None. the function will not throw an error
#' if the tests fail.
#'
#' @examples
#' test_describe_num_var()

test_describe_num_var <- function() {
  # Generate test data from the helper function.
  test_data <- helper_create_data(500)
  num_var <- c('num1', 'num2', 'num3')

  # Test the results when the input is correct.
  result <- describe_num_var(test_data, num_var)
  
  # Test if the statistical summary is correctly calculated.
  test_that("The statistical summary should be correctly calculated", {
    mean_num1 <- mean(test_data$num1, na.rm = TRUE)
    median_num1 <- median(test_data$num1, na.rm = TRUE)
    sd_num1 <- sd(test_data$num1, na.rm = TRUE)
    max_num1 <- max(test_data$num1, na.rm = TRUE)
    min_num1 <- min(test_data$num1, na.rm = TRUE)
    q25_num1 <- quantile(test_data$num1, 0.25, na.rm = TRUE)
    q75_num1 <- quantile(test_data$num1, 0.75, na.rm = TRUE)
    expect_equivalent(as.numeric(result$summary$num1[1]), round(q25_num1), 3)
    expect_equivalent(as.numeric(result$summary$num1[2]), round(q75_num1), 3)
    expect_equivalent(as.numeric(result$summary$num1[3]), round(min_num1), 3)
    expect_equivalent(as.numeric(result$summary$num1[4]), round(max_num1), 3)
    expect_equivalent(as.numeric(result$summary$num1[5]), round(median_num1), 3)
    expect_equivalent(as.numeric(result$summary$num1[6]), round(mean_num1), 3)
    expect_equivalent(as.numeric(result$summary$num1[7]), round(sd_num1), 3)
  })

  # Test the plot type is correct.
  test_that("The returned plot should be a ggplot object.", {
    expect_true(ggplot2::is.ggplot(result$plot))
  })

  # Test the plot type is correct.
  test_that("The plot should be a bar chart and without y mapping.", {
    expect_true("GeomBar" %in% c(class(result$plot$layers[[1]]$geom)))
    expect_true(is.null(rlang::get_expr(result$plot$mapping$y)))
  })

  # Test the error message is correct when the type of `dataframe` argument is wrong.
  test_that(
    "Corresponding error message should be expected if the dataframe argument is not a dataframe.",
    {
      expect_error(describe_num_var("abc", num_var),
                   regexp = "The value of the argument 'dataframe' should be of type  'data.frame' or 'tibble'.")
    }
  )

  # Test the error message is correct when the type of `num_vars` argument is wrong.
  test_that(
    "Corresponding error message should be expected if the num_vars argument is not a vector",
    {
      expect_error(describe_num_var(test_data, test_data),
                   regexp = "The value of the argument 'num_vars' should be a vector of characters.")
    }
  )

  # Test the error message is correct when the type of `num_vars` argument is wrong.
  test_that(
    "Corresponding error message should be expected if the num_vars argument is not a vector of charactors",
    {
      expect_error(describe_num_var(test_data, c(1, 2)),
                   regexp = "The value of the argument 'num_vars' should be a vector of characters.")
    }
  )

  # Test the error message is correct when the type of `num_vars` argument is not a subset of the column names of the dataframe.
  test_that(
    "Corresponding error message should be expected if the num_vars argument contains element that is not a column name",
    {
      expect_error(describe_num_var(test_data, c("num1", "abc")),
                   regexp = "The argument 'num_vars' should be a subset of the column names of the dataframe.")
    }
  )

  # Test the error message is correct when `num_vars` argument contains categorical columns of the dataframe.
  test_that(
    "Corresponding error message should be expected if the selected columns contains categorical variables.",
    {
      expect_error(describe_num_var(test_data, c("num1", "cat1")),
                   regexp = "Only numeric columns expected, please check the input.")
    }
  )
}

#' Tests the describe_cat_var function to make sure outputs are correct
#' or the function will fail with the correct error message.
#'
#' @return None. the function will not throw an error
#' if the tests fail.
#'
#' @examples
#' test_describe_cat_var()
test_describe_cat_var <- function() {
  test_data <- helper_create_data(500)
  cat_var <- c('cat1', 'cat2', 'cat3')
  result <- describe_cat_var(test_data, cat_var)


  test_that("The returned plot should be a ggplot object.", {
    expect_true(is.ggplot(result))
  })

  test_that("The plot should be a bar chart and without y mapping.", {
    expect_true("GeomBar" %in% c(class(result$layers[[1]]$geom)))
    expect_true(is.null(rlang::get_expr(result$mapping$y)))
  })

  test_that("Corresponding error message should be expected if the dataframe argument is not a dataframe.", {
    expect_error(describe_cat_var("abc", cat_var),
                 regexp = "The value of the argument 'dataframe' should be of type 'data.frame' or 'tibble'.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument is not a vector", {
    expect_error(describe_cat_var(test_data, test_data),
                 regexp = "The value of the argument 'cat_vars' should be a vector of characters.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument is not a vector of charactors", {
    expect_error(describe_cat_var(test_data, c(1, 2)),
                 regexp = "The value of the argument 'cat_vars' should be a vector of characters.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument contains element that is not a column name", {
    expect_error(describe_cat_var(test_data, c("num1", "abc")),
                 regexp = "The argument 'cat_vars' should be a subset of the column names of the dataframe.")
  })

}

