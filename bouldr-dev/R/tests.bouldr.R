#' Convenience function to pull out the table of test results
#'
#' @param rocbag A bouldr-class object
#'
#' @return A data frame containing the test information
#' @export
#'
tests <- function(rocbag) {
  #
  return(rocbag$tests)
}
