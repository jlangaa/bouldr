
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

  #To use a tab separator, use "\t", and use "\n" to create a line break.
  if (length(rhs) == 1){
  cat("Predictor var:\t", rhs, "\n")
  }

  if (length(rhs) > 1 && length(as.character(rhs[[2]])) == 1){
    cat("Predictor var:\t", rhs[[2]], "\n")
    cat("Grouping var:\t", rhs[[3]], "\n")
  }

  if (length(rhs) > 1 && length(as.character(rhs[[2]])) == 3){
    cat("Predictor var:\t", as.character(rhs[[2]])[[2]], "\n")
    cat("Grouping var:\t", as.character(rhs[[2]])[[3]], "\n")
    cat("Faceting var:\t", rhs[[3]], "\n")
  }

  cat("\n\nAUC table\nSee next page\n\n")

  print(aucs(x))

  if(!tests){
    cat("\nTo show tests, add 'tests = TRUE' to print command")
  } else {
    cat("\nTests\nSee the last page\n\n")
    print(tests(x))
  }


}
