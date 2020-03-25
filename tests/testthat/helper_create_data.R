library(tibble)

#' Generate test dataframe for the edar package. The dataframe has three numeric and three categorical variables. Each type of variables has one column that contains 5% NA values.
#'
#' @param n int. Number of rows in the dataframe.
#'
#' @return tbl. The dateframe that can be used for test.
#'
#' @examples
#' test_data <- helper_create_data(500)
#'
helper_create_data <- function(n){
  # Generate two index to impute na values
  index1 <- sample(1:n, 0.05*n, replace=F)
  index2 <- sample(1:n, 0.05*n, replace=F)

  vals <- c('cat','dog','lion')

  t <- tibble(
    num1 = rexp(n, 3),
    num2 = rnorm(n, 2, 2),
    num3 = rnorm(n, 10, 3),
    cat1 = as.character(rbinom(n, 1, 0.7)),
    cat2 = as.character(rpois(n, 1)),
    cat3 = as.character(rbinom(n, 5, 0.4)),
    cat4 = sample(vals,n,replace=TRUE)
  )
  t$num1[index1] <- NA
  t$cat2[index2] <- NA
  return(t)
}
