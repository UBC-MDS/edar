test_corr_cal <- function(corr_function){
  data <- helper_create_data(200)
  p <- corr_data(data, num_var)

  test_that("Plot should be a ggplot object.", {
    expect_true(is.ggplot(p))
  })

  test_that('Plot should use GeomTile and GeomRect.', {
    expect_true("GeomTile" %in% c(class(p$layers[[1]]$geom)))
    expect_true("GeomRect" %in% c(class(p$layers[[1]]$geom)))
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
}
