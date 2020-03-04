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
    expect_equivalent(as.numeric(result$summary$num1[1]), q25_num1)
    expect_equivalent(as.numeric(result$summary$num1[2]), q75_num1)
    expect_equivalent(as.numeric(result$summary$num1[3]), min_num1)
    expect_equivalent(as.numeric(result$summary$num1[4]), max_num1)
    expect_equivalent(as.numeric(result$summary$num1[5]), median_num1)
    expect_equivalent(as.numeric(result$summary$num1[6]), mean_num1)
    expect_equivalent(as.numeric(result$summary$num1[7]), sd_num1)
  })

  test_that("The returned plot should be a ggplot object.", {
    expect_true(is.ggplot(result$plot))
  })

  test_that("The plot should be a bar chart and without y mapping.", {
    expect_true("GeomBar" %in% c(class(result$plot$layers[[1]]$geom)))
    expect_true(is.null(rlang::get_expr(result$plot$mapping$y)))
  })

  test_that("Corresponding error message should be expected if the dataframe argument is not a dataframe.", {
    expect_error(describe_num_var("abc", num_var), regexp = "The first argument should be a data frame.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument is not a vector", {
    expect_error(describe_num_var(test_data, test_data), regexp = "The second argument should be a vector of characters.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument is not a vector of charactors", {
    expect_error(describe_num_var(test_data, c(1, 2)), regexp = "The second argument should be a vector of characters.")
  })

  test_that("Corresponding error message should be expected if the num_vars argument contains element that is not a column name", {
    expect_error(describe_num_var(test_data, c("num1", "abc")), regexp = "The second argument should be a subset of the cloumn names of the dataframe.")
  })

  test_that("Corresponding error message should be expected if the selected columns contains categorical variables.", {
    expect_error(describe_num_var(test_data, c("num1", "cat1")), regexp = "Not only the numeric variables are selected, please check the input.")
  })
}
