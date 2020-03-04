# author Group 4 Isaac Newton

library(dplyr)
library(ggcorrplot)
library(reshape2)

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
#' @return ggplot object to plot histogram of the categorical variables
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


#' Evaluates the correlation between the numeric columns of a given dataframe.
#'
#' @param df The dataframe to be inspected.
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
calc_cor <- function(df, num_vars) {
  # Test dataframe input to check if dataframe is a dataframe
  if (!is.data.frame(df))
    stop("Input 'df' should be a dataframe.")

  # Find numerical columns and remove NA
  df_num <- df %>%
    select(num_vars) %>%
    drop_na()

  # Test colums to check if columns provided are numeric
  if (!all(sapply(df_num, is.numeric)))
    stop("Columns do not all contain numeric values.")

  # Find the correlation
  df_cor <- round(cor(df_num),2)

  # Plot the correlation matrix
  corr_plot <- ggcorrplot(df_cor,
                          type = "lower",
                          outline.color = "white",
                          lab = TRUE,
                          title="Correlation Matrix",
                          colors = c("blue", "white", "red"),
                          lab_size = 8) +
    theme(title=element_text(size=20),
          axis.text=element_text(size=15, face='bold'),
          text=element_text(size=15))

  return(corr_plot)
}


#' evaluates a dataframe for NA values.
#'
#' @param dataframe the dataframe to be inspected.
#'
#' @return a list of vectors; each vector corresponds to a column and
#' each value inside the vector is 0 if the corresponding value is NA,
#' 1 otherwise.
#' TODO write meaningful examples as implementation goes on
describe_na_values <- function(dataframe) {
  #TODO implement function
  list(c(0))
}
