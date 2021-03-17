
#' A print function for the bouldr object
#'
#' Prints the original formula, then the AUCs then (optionally) the tests
#'
#' @param x A bouldr object to print
#' @param tests Should the tests be printed? Default is FALSE, since these can be long
#' @param ... Additional arguments for printing
#'
#' @return NULL
#' @export
print.bouldr <- function(x, tests = FALSE, ...) {
  lhs <- x$formula[[2]]
  rhs <- x$formula[[3]]

  cat("Outcome var:\t", lhs, "\n")
  cat("Predictor var:\t", rhs[[2]][[2]], "\n")

  if (length(rhs) > 1){
    cat("Grouping var:\t", rhs[[2]][[3]], "\n")

  }
  if (length(rhs) > 2){
    cat("Faceting var:\t", rhs[[3]], "\n")

  }

  cat("\n\nAUC table\n\n")

  print(aucs(x))

  if(!tests){
    cat("To show tests, add 'tests = TRUE' to print command")
  } else {
    cat("\n\nTests\n\n")

    print(tests(x))
  }
  cat("AUC table\n")

  print(aucs(x))

  cat("To show tests, add 'tests = TRUE' to print command")

}
