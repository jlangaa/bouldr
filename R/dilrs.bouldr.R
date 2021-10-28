#' A Diagnostic Likelihood Ratio (DiLR) calculator
#'
#' Computes Diagnostic Likelihood Ratios (DiLRS) for a specified cut-point
#' of the continuous variable. Also outputs additional
#' classification statistics (true/false positives/negatives).
#'
#' @param x a data frame containing the relevant variables
#' @param scores (character) the name of the continuous variable
#' @param outcomes (character) the name of the classification variable
#' @param positive (string or numeric) the positive case in `outcomes`
#' @param ... Additional arguments (not implemented yet)
#'
#' @return a numeric vector containing the classification statistics
#' @export
#' @importFrom rlang .data
dlrs <- function(x, scores, outcomes, positive, ...) {
  # Computes Diagnostic Likelihood ratios (and a bunch of other stuff)
  #
  # cut := the cut score (numeric)
  # n.cuts := the number of quantiles desired
  # scores := the name of the column containing the scores
  # outcomes := the name of the column containing the binary outcomes
  # positive := the outcome value of the positive case (pay attention to type!)
  # x := the x frame containing at least 2 columns: one containing
  # continuous scores `scores` and another containing binary outcomes `outcomes`
  #
  # returns a tibble with one row

  dots <- list(...)
  ## make sure x is data.table object
  x <- tidyr::as_tibble(x)
  if (!is.null(dots$cut)) {
    ## DiLR formula:
    ## LR+ = P(Score > Cut | Yes Dx) / P(Score > Cut | No Dx)
    ## Alternatively:
    ## LR+ = sensitivity / (1 - specificity)

    cut <- dots$cut

    ret <- x %>%
      dplyr::summarise(
        cut = cut,
        true_pos = sum(.data[[scores]] >= cut &
                         .data[[outcomes]] == positive),
        false_pos = sum(.data[[scores]] >= cut &
                          .data[[outcomes]] != positive),
        true_neg = sum(.data[[scores]] < cut &
                         .data[[outcomes]] != positive),
        false_neg = sum(.data[[scores]] < cut &
                          .data[[outcomes]] == positive),
        sensitivity = .data$true_pos / (.data$true_pos + .data$false_neg),
        specificity = .data$true_neg / (.data$false_pos + .data$true_neg),
        LR_pos = (.data$true_pos / (.data$true_pos + .data$false_neg)) / (.data$false_pos / (.data$false_pos + .data$true_neg)),
        LR_neg = (.data$false_neg / (.data$true_pos + .data$false_neg)) / (.data$true_neg / (.data$false_pos + .data$true_neg))
      )

    ## return as a tibble
    return(ret)
  }
  else if (!is.null(dots$n.cuts)) {
    n <- dots$n.cuts

    tot.pos <- sum(x[[outcomes]] == positive)
    tot.neg <- sum(x[[outcomes]] != positive)

    quantiles <- stats::quantile(x[[scores]], seq(0, 1, 1 / n))

    ret <- x %>%
      dplyr::mutate(score.band = cut(get(scores), breaks = quantiles, ordered_result = TRUE)) %>%
      dplyr::group_by(.data$score.band) %>%
      dplyr::summarize(
        positive.cases = sum(.data[[outcomes]] == positive),
        negative.cases = dplyr::n() - .data$positive.cases,
        percent.positive = .data$positive.cases / tot.pos,
        percent.negative = .data$negative.cases / tot.neg,
        LR = .data$percent.positive / .data$percent.negative,
        total.positive = tot.pos,
        total.negative = tot.neg
      )
    return(ret)
  } else {
    stop("You must supply either 'cut' or 'n.cuts' argument")
  }
}
