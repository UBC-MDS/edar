#' Tests the correlation function to make sure output is rendered correctly
#' or the function will fail with an error message and problem.
#'
#' @return None. the function will not throw an error
#' if the tests fail.
#'
#' @export
#'
#' @examples
#' test_cal_cor()
test_cal_cor <- function(){
  data <- helper_create_data(200)
  num_var <- c('num1', 'num2', 'num3')
  p <- calc_cor(data, num_var)

  test_that("The returned plot should be a ggplot object.", {
    expect_true(is.ggplot(p))
  })

  test_that('Plot should use geomtile and geomrect.', {
    expect_true("GeomTile" %in% c(class(p$layers[[1]]$geom)))
    expect_true("GeomRect" %in% c(class(p$layers[[1]]$geom)))
    expect_true("Layer" %in% c(class(rlang::get_expr(corr_plot$layers[[1]]))))
  })

  test_that('Overlapping plot shoud use geomtext.', {
    expect_true("GeomText" %in% c(class(rlang::get_expr(corr_plot$layers[[2]]$geom))))
  })

  test_that('Plot should map Var1 to x-axis, and Var2 to y-axis.', {
    expect_true("Var1"  == rlang::get_expr(p$mapping$x))
    expect_true("Var2" == rlang::get_expr(p$mapping$y))
  })

  test_that('Variables for layer and base should be the same', {
    expect_true(rlang::get_expr(corr_plot$layers[[2]]$mapping$x) == rlang::get_expr(p$mapping$x))
    expect_true(rlang::get_expr(corr_plot$layers[[2]]$mapping$y) == rlang::get_expr(p$mapping$y))
  })

  test_that('All values should be between -1 and 1', {
    p[[1]][3] <= 1 & p[[1]][3] >= -1
  })

  test_that('Var1 should not equal Var2', {
    corr_plot[[1]][1] != corr_plot[[1]][2]
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
