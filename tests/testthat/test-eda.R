#' Tests the `describe_num_var` function.

test_describe_num_var <- function() {
  test_data <- helper_create_data(500)
  num_var <- c('num1', 'num2', 'num3')
  result <- describe_num_var(test_data, num_var)

  test_that("The statistical summary should be correctly calculated", {
    mean_num1 <- mean(test_data$num1, na.rm = TRUE)
    median_num1 <- median(test_data$num1, na.rm = TRUE)
    sd_num1 <- sd(test_data$num1, na.rm = TRUE)
    max_num1 <- max(test_data$num1, na.rm = TRUE)
    min_num1 <- min(test_data$num1, na.rm = TRUE)
    q25_num1 <- quantile(test_data$num1, 0.25, na.rm = TRUE)
    q75_num1 <- quantile(test_data$num1, 0.75, na.rm = TRUE)
    expect_equivalent(as.numeric(result$summary$num1[1]), round(q25_num1),3)
    expect_equivalent(as.numeric(result$summary$num1[2]), round(q75_num1),3)
    expect_equivalent(as.numeric(result$summary$num1[3]), round(min_num1),3)
    expect_equivalent(as.numeric(result$summary$num1[4]), round(max_num1),3)
    expect_equivalent(as.numeric(result$summary$num1[5]), round(median_num1),3)
    expect_equivalent(as.numeric(result$summary$num1[6]), round(mean_num1),3)
    expect_equivalent(as.numeric(result$summary$num1[7]), round(sd_num1),3)
  })

  test_that("The returned plot should be a ggplot object.", {
    expect_true(is.ggplot(result$plot))
  })

  test_that("The plot should be a bar chart and without y mapping.", {
    expect_true("GeomBar" %in% c(class(result$plot$layers[[1]]$geom)))
    expect_true(is.null(rlang::get_expr(result$plot$mapping$y)))
  })

  test_that("Corresponding error message should be expected if the dataframe argument is not a dataframe.", {
    expect_error(describe_num_var("abc", num_var),
    regexp = "The value of the argument 'dataframe' should be of type  'data.frame' or 'tibble'.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument is not a vector", {
    expect_error(describe_num_var(test_data, test_data),
    regexp = "The value of the argument 'num_vars' should be a vector of characters.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument is not a vector of charactors", {
    expect_error(describe_num_var(test_data, c(1, 2)),
    regexp = "The value of the argument 'num_vars' should be a vector of characters.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument contains element that is not a column name", {
    expect_error(describe_num_var(test_data, c("num1", "abc")),
    regexp = "The argument 'num_vars' should be a subset of the column names of the dataframe.")
  })

  test_that("Corresponding error message should be expected if the selected columns contains categorical variables.", {
    expect_error(describe_num_var(test_data, c("num1", "cat1")),
    regexp = "Only numeric columns expected, please check the input.")
  })
}


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
                 regexp = "The value of the argument 'dataframe' should be of type  'data.frame' or 'tibble'.")
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
