#' Convenience function to pull out the table of test results
#'
#' @param rocbag A bouldr-class object
#'
#' @return A data frame containing the test information
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
