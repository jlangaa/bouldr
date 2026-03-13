#' Compute optimal thresholds for ROCs
#'
#' @param x A bouldr-class object
#'
#' @return A data frame containing the coordinates for each roc
#' @export
#' @examples
#' dd <- generate_data(10,Diagnosis = c("Depression","Anxiety","ADHD"),
#'         Measure = c("A","B"), Informant = c("Self","Parent", "Teacher"))
#' single <- bouldr(dat = dd,
#' f = Outcome ~ Score,
#' test = 'delong',
#' levels = c('no','yes'),
#' direction = "<")
#'
#' optimal(single)
#'
optimal.bouldr <- function(x, type = "best", ...) {
  if (!inherits(x, "bouldr")) {
    stop("input must be of type 'bouldr'")
  }

  best.method <- c("youden")

  dots <- list(...)
  if (type == "best" & !is.null(dots$best.method)) {
    best.method <- dots$best.method
  }

  if (x$type == "faceted") {
    ret <- purrr::map(x$rocs, \(g) {
      purrr::map(g, \(r)
      pROC::coords(roc = r, x = type, best.method = best.method)) %>%
        bind_rows(.id = "group")
    }) %>%
      bind_rows(.id = "facet")
  } else if (x$type == "grouped") {
    ret <- purrr::map(x$rocs, \(r) {
      pROC::coords(roc = r, x = type, best.method = best.method)
    }) %>%
      bind_rows(.id = "group")
  } else if (x$type == "single") {
    ret <- pROC::coords(roc = x$rocs, x = type, best.method=best.method)
  }

  return(ret)
}
