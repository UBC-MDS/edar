# author Group 4 Isaac Newton


#' A silly function
#'
#' A silly function to generate the structure of the package.
#'
#' @param a int
#'
#' @return Null
#' @export
#' @examples
#' foo(2020)
#'
foo <- function(a) {
  print(a)
}



#' evaluates a dataframe for NA values.
#'
#' @param dataframe the dataframe to be inspected.
#'
#' @return a list of vectors; each vector corresponds to a column and
#' each value inside the vector is 0 if the corresponding value is NA,
#' 1 otherwise.
#'
#' @export
#'
#' @examples
#' TODO write meaningful examples as implementation goes on
describe_na_values <- function(dataframe) {
  #TODO implement function
  list(c(0))
}
