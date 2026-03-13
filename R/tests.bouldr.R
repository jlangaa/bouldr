#' Convenience function to pull out the table of test results
#'
#' @param rocbag A bouldr-class object
#'
#' @return A list with two data frames containing the test information: "compare" contains comparisons between curves and "chance" shows tests against change classification.
#' @export
#'
tests <- function(rocbag, compare = TRUE, versus_chance = TRUE) {
  #
  ret <- list()

  if (compare) {
    ret$compare <- rocbag$tests
  }

  if (versus_chance){

    ret$chance <- rocbag$null_tests
  }

  print(ret)
}
