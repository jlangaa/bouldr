
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
  # warning("this isn't working properly")
  lhs <- x$formula[[2]]
  rhs <- x$formula[[3]]

  cat("Outcome:\t", lhs, "\n")

  if (x$type == "single") {
    cat("Predictor:\t", rhs, "\n")
  }

  if (x$type == "grouped") {
    cat("Predictor:\t", rhs[[2]], "\n")
    cat("Grouping:\t", rhs[[3]], "\n")
  }

  if (x$type == "faceted") {
    cat("Predictor:\t", rhs[[2]][[2]], "\n")
    cat("Grouping:\t", rhs[[2]][[3]], "\n")
    cat("Faceting:\t", rhs[[3]], "\n")
  }

  cat("\nAUC table\n")
  aucs(x) |> print()

  if (!tests) {
    cat("\nTo show tests, add 'tests = TRUE' to print command")
  } else {
    cat("Tests\n")
    tests(x)
  }

  }
