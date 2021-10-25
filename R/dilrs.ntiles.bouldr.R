#' A Diagnostic Likelihood Ratio (DiLR) calculator for n-tiles
#'
#' Computes Diagnostic Likelihood Ratios (DiLRS) for a specified quantiles of
#' the continuous variable. Also outputs additional
#' classification statistics (true/false positives/negatives).
#'
#' @param n (numeric) the number of cut points to include
#' @param ... Additional arguments passed to `dilrs()`
#'
#' @return a data frame containing the classification statistics for each cut point
#' @export
dlrs.ntiles <- function(n, ...) {
  # This function computes DiLRs etc. for a specified number of quantiles.
  #
  # n := number of cut-scores desired
  #
  # Returns a list with a row for each cut point

  dots <- list(...)

  ## Determine score cutoffs for n cut-scores
  # the quantile() function always includes 0% and 100$ so the "n+1" here is to
  # omit those and have `n` map to the number of cut scores desired.
  ntile_scores <- stats::quantile(x = dots$data[,get(dots$scores)], seq(0,1,1/(n+1)))[2:(n+1)]

  ## using the map() function, run dlrs() for each cut score defined above

  ret <-
    purrr::map_df(
      ntile_scores,
      ~ unlist(dlrs(
        cut = .x,
        scores = dots$scores,
        outcomes = dots$outcomes,
        positive = dots$positive,
        data = dots$data,
        ...
      )), .id = "Percentile"
    )

  return(ret)
}
