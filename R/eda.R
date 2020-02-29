# author Group 4 Isaac Newton

library(dplyr)

#' Provides statistical summary of the numeric variables for a dataframe, such as the mean, median, maximum and minimum for the numeric variables.
#'
#' @param dataframe tbl. The dataframe to be inspected.
#' @param num_vars vector of character strings of the names of the numeric variables.
#' @param plot logical. If TRUE (the default) the distribution of the numeric variables will be plotted.
#'
#' @return tbl. A dataframe contains the statistical summary.
#'
#' @export
#'
#' @examples
#' X <- tibble(type = c('Car','Bus','Car'), height = c(10,20,30))
#' num_vars <- c('height')
#' describe_num_var(X, num_vars)
#'
describe_num_var <- function(dataframe, num_vars, plot=TRUE) {
  #TODO implement function
  print(num_vars)
}

#' Evaluates the correlation between the numeric columns of a given dataframe.
#'
#' @param dataframe The dataframe to be inspected.
#' @param num_vars A list of strings of column names containing numeric variables.
#'
#' @return ggplot object; a correlation matrix plot labelled
#' with the correlation coefficients of -1 to 1 between
#' each numeric column and other numeric columns in the dataframe.
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = (c(2,3,4)), y= c(1,10,3))
#' col_num <- list("x", "y")
#' calc_cor(df, col_num)
calc_cor <- function(dataframe, num_vars) {
  #TODO implement function
}

