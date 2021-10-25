#' A Diagnostic Likelihood Ratio (DiLR) calculator
#'
#' Computes Diagnostic Likelihood Ratios (DiLRS) for a specified cut-point
#' of the continuous variable. Also outputs additional
#' classification statistics (true/false positives/negatives).
#'
#' @param cut (numeric) the cut score at which to compute the DiLR
#' @param scores (character) the name of the continuous variable
#' @param outcomes (character) the name of the classification variable
#' @param positive (string or numeric) the positive case in `outcomes`
#' @param data a data frame containing the relevant variables
#' @param ... Additional arguments (not implemented yet)
#'
#' @return a numeric vector containing the classification statistics
#' @export
dlrs <- function(cut, scores, outcomes, positive, data, ...) {
  # Computes Diagnostic Likelihood ratios (and a bunch of other stuff)
  #
  # cut := the cut score (numeric)
  # scores := the name of the column containing the scores
  # outcomes := the name of the column containing the binary outcomes
  # positive := the outcome value of the positive case (pay attention to type!)
  # data := the data frame containing at least 2 columns: one containing
  # continuous scores `scores` and another containing binary outcomes `outcomes`
  #
  # returns a vector of data

  dots <- list(...)

  ## make sure data is data.table object
  if (!("data.table" %in% class(data)))
    data <- data.table::as.data.table(data)

  ## DiLR formula:
  ## LR+ = P(Score > Cut | Yes Dx) / P(Score > Cut | No Dx)
  ## Alternatively:
  ## LR+ = sensitivity / (1 - specificity)


  ## Calculate relevant statistics using syntax from data.table package
  ## note `.N` gives the number of rows defined by the logical statement
  true_pos  = data[get(scores) >= cut & get(outcomes) == positive, data.table::.N]
  false_pos = data[get(scores) >= cut & get(outcomes) != positive, data.table::.N]
  false_neg = data[get(scores) < cut & get(outcomes) == positive, data.table::.N]
  true_neg  = data[get(scores) < cut & get(outcomes) != positive, data.table::.N]

  ## save this in a list
  ret <- list(
    cut = cut,
    true_pos  = true_pos,
    false_pos = false_pos,
    false_neg = false_neg,
    true_neg  = true_neg,
    LRPos = (true_pos / (true_pos + false_neg)) / (false_pos / (false_pos + true_neg)),
    LRNeg = (true_neg / (true_pos + false_neg)) / (false_neg / (false_pos + true_neg))
  )

  ## return as a vector
  return(unlist(ret))
  ## it seems strange to list() then unlist(), but this creates a convenient
  ## and extendable output format.
}

