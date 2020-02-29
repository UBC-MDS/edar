# author Group 4 Isaac Newton

library(dplyr)

#' This function generates an EDA report by plotting graphs and tables for the
#' numeric variables, categorical variables, NA values and correlation in a dataframe
#'
#' @param dataframe tbl. The dataframe to be inspected.
#' @param cat_vars vector of character strings of the names of the categorical variables.
#' @param num_vars vector of character strings of the names of the numeric variables.
#'
#' @return It returns True on successful execution else returns False
#'
#' @export
#'
#' @examples
#' X <- tibble(type = c('Car','Bus','Car'), height = c(10,20,30))
#' cat_vars <- c('type')
#' num_vars <- c('height')
#' generate_report(X, cat_vars, num_vars)
#'
generate_report <- function(dataframe, cat_vars, num_vars) {
  #TODO implement function
}


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


#' This function will take data frame and categorical variable names and will
#  plot the histogram of each categorical variable.
#'
#' @param dataframe tbl. The dataframe to be inspected.
#' @param cat_vars vector of character strings of the names of the categorical variables.
#'
#' @return ggplot object to plot histogra of the categorical variables
#'
#' @export
#'
#' @examples
#' X <- tibble(type = c('Car','Bus','Car'), height = c(10,20,30))
#' cat_vars <- c('type')
#' describe_num_var(X, cat_vars)
#'
describe_cat_var <- function(dataframe, cat_vars) {
  #TODO implement function
  print(cat_vars)
}
