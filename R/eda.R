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
