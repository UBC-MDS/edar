# author Group 4 Isaac Newton

#' This function generates an EDA report by plotting graphs and tables for the numeric variables, categorical variables, NA values and correlation in a dataframe
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
#' X <- dplyr::tibble(type = c('Car','Bus','Car'), height = c(10,20,30))
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
#'
#' @return list. A list containing a dataframe with a statistical summary and a ggplot object with histograms of numeric variables faceted by each variable.
#'
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom stats median quantile sd
#' @export
#'
#' @examples
#' X <- dplyr::tibble(type = c('Car', 'Bus', 'Car'), height = c(10, 20, 15), width = c(10, 15, 13))
#' num_vars <- c('height', 'width')
#' result <- describe_num_var(X, num_vars)
#' result$summary
#' result$plot
#'
describe_num_var <- function(dataframe, num_vars) {
  # Check the input dataframe is a dataframe
  if (!is.data.frame(dataframe)) {
    stop("The value of the argument 'dataframe' should be of type  'data.frame' or 'tibble'.")
  }

  # Transform the input into tibble
  dataframe <- as_tibble(dataframe)

  # Chech the input num_vars is a vector of characters
  if (!is.character(num_vars) | !is.vector(num_vars)) {
    stop("The value of the argument 'num_vars' should be a vector of characters.")
  }

  # Check the input num_vars is a vector of column names of dataframe
  if (!all(num_vars %in% colnames(dataframe))) {
    stop("The argument 'num_vars' should be a subset of the column names of the dataframe.")
  }

  # Select the numeric variables to work with
  df <- dataframe %>%
    select(num_vars)

  # Check if only the numeric variables are selected
  if (!all(sapply(df, is.numeric))) {
    stop("Only numeric columns expected, please check the input.")
  }

  # Calculate the statistical summaries for all columns
  stat_funs <- c(min, max, median, mean, sd)
  temp <- c()
  for (item in stat_funs) {
    data_stat <- sapply(df, item, na.rm = TRUE)
    temp <- rbind(temp, data_stat)
  }
  data_quantile <- sapply(df, quantile, na.rm = TRUE)

  # Get the 25% and 75% quantiles
  quantile_025 <- data_quantile[2, ]
  quantile_075 <- data_quantile[4, ]

  # Give the summaries meaningful names and make the result as a tibble
  summary <- c("25%", "75%", "min", "max", "median", "mean", "sd")
  summary_wo_name <- round(rbind(quantile_025, quantile_075, temp),3)
  result <- as_tibble(cbind(summary, summary_wo_name))

  # Plot the faceted histogram
  data_plot <- df %>%
    drop_na() %>%
    gather() %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, color = 'gray') +
    facet_wrap(~ key, scales = "free", ncol = 3)

  return(list(summary = result, plot = data_plot))
}


#' This function will take data frame and categorical variable names and will plot the histogram of each categorical variable.
#'
#' @param dataframe tbl. The dataframe to be inspected.
#' @param cat_vars vector of character strings of the names of the categorical variables.
#'
#' @return ggplot object to plot histogram of the categorical variables
#'
#' @export
#'
#' @examples
#' X <- dplyr::tibble(type = c('Car','Bus','Car'), height = c(10,20,30))
#' cat_vars <- c('type')
#' describe_cat_var(X, cat_vars)
#'
describe_cat_var <- function(dataframe, cat_vars) {
  # Check the input dataframe is a dataframe
  if (!is.data.frame(dataframe)) {
    stop("The value of the argument 'dataframe' should be of type 'data.frame' or 'tibble'.")
  }

  # Transform the input into tibble
  dataframe <- as_tibble(dataframe)

  # Chech the input cat_vars is a vector of characters
  if (!is.character(cat_vars) | !is.vector(cat_vars)) {
    stop("The value of the argument 'cat_vars' should be a vector of characters.")
  }

  # Check the input cat_vars is a vector of column names of dataframe
  if (!all(cat_vars %in% colnames(dataframe))) {
    stop("The argument 'cat_vars' should be a subset of the column names of the dataframe.")
  }

  # Select the categorical variables to work with
  df <- dataframe %>%
    select(cat_vars)

  # Plot the faceted histogram
  data_plot <- df %>%
    drop_na() %>%
    gather() %>%
    ggplot(aes(x = value)) +
    geom_bar(aes(y=..count..), color = 'gray',width = 0.6) +
    facet_wrap(~ key, scales = "free", ncol = 3)+
    ggtitle('Histogram of categorical variables')

  return(data_plot)
}



#' Evaluates the correlation between the numeric columns of a given dataframe.
#'
#' @param df The dataframe to be inspected.
#' @param num_vars A list of strings of column names containing numeric variables.
#'
#' @return ggplot object; a correlation matrix plot labelled with the correlation coefficients of -1 to 1 between each numeric column and other numeric columns in the dataframe.
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = (c(2,3,4)), y= c(1,10,3))
#' col_num <- c("x", "y")
#' calc_cor(df, col_num)
#'
calc_cor <- function(df, num_vars) {
  # Test dataframe input to check if dataframe is a dataframe
  if (!is.data.frame(df))
    stop("Input 'df' should be a dataframe.")

  # Find numerical columns and remove NA
  df_num <- df[,num_vars]
  df_num <- tidyr::drop_na(df_num)


  # Test colums to check if columns provided are numeric
  if (!all(sapply(df_num, is.numeric)))
    stop("Columns do not all contain numeric values.")

  # Find the correlation
  df_cor <- round(stats::cor(df_num), 2)

  # Plot the correlation matrix
  corr_plot <- ggcorrplot::ggcorrplot(df_cor,
                                      type = "lower",
                                      outline.color = "white",
                                      lab = TRUE,
                                      title="Correlation Matrix",
                                      colors = c("blue", "white", "red"),
                                      lab_size = 8) +
    ggplot2::theme(title=ggplot2::element_text(size=20),
                   axis.text=ggplot2::element_text(size=15, face='bold'),
                   text=ggplot2::element_text(size=15))

  return(corr_plot)
}


#' evaluates a dataframe for NA values.
#'
#' @param dataframe the dataframe to be inspected.
#'
#' @return a tibble; each column corresponds to the same column in dataframe and each value inside the columnr is 0 if the corresponding value is NA, 1 otherwise.

#' stops if the object passed in is not a data.frame or tibble.
#' @export
#'
#' @examples
#' df <- data.frame(x = (c(2,3,4)), y= c(1,10,3))
#' col_num <- describe_na_values(df)
#'
#' #> # A tibble: 2 x 3
#' #>       x     y
#' #>   <int> <int>
#' #> 1     1     1
#' #> 2     1     1
#' #> 3     1     1
#'
#' df <- data.frame(x = (c(2,NaN,4)), y= c(1,10,3))
#' col_num <- describe_na_values(df)
#'
#' #> # A tibble: 2 x 3
#' #>       x     y
#' #>   <int> <int>
#' #> 1     1     1
#' #> 2     0     1
#' #> 3     1     1
#'
describe_na_values <- function(dataframe) {
  if (!is.data.frame(dataframe)) {
    stop("The value of the argument 'dataframe' should be of type  'data.frame' or 'tibble'.")
  }
  ret_dataframe <-purrr::map(dataframe, function(x){as.numeric(!is.na(x))})
  ret_dataframe
}
