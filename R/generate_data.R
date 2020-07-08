#' Generate a test data frame with 6 nested variables
#'
#' ID: participant ID
#' Diagnosis: the type of diagnosis
#' Measure: the form used to measure symptoms
#' Informant: the person taking the form
#' Outcome: whether the diagnosis is present
#' Score: the score on the measure
#'
#' @param obs.per Number of observations per cell
#' @param Diagnosis A character vector of names for this grouping variable
#' @param Measure  A character vector of names for this grouping variable
#' @param Informant A character vector of names for this grouping variable
#'
#' @return A data frame that can be used in bouldr
#' @export
generate_data <- function(obs.per,
                          Diagnosis = c("Depression","Anxiety"),
                          Measure = c("A","B","C","D","E"),
                          Informant = c("Parent","Teacher","Self")) {

  if (!is.numeric(obs.per))
    stop("Error: Please specify number of observations per condition (obs.per must be defined)")

  roc.data <- data.frame()

  for (D in Diagnosis) {
    for (M in Measure) {
      for (I in Informant) {
        separation <- stats::runif(1, min = 0, max = 8)
        sd.pooled <- stats::runif(1,1,5)

        g1 <- data.frame(Outcome = "no",
                         Score = stats::rnorm(obs.per,0,sd.pooled)
        )
        g2 <- data.frame(Outcome = "yes",
                         Score = stats::rnorm(obs.per,separation, sd.pooled))
        scores <- rbind(g1,g2)

        d <- data.frame(
          ID = 1:obs.per,
          Diagnosis = D,
          Measure = M,
          Informant = I
        )
        d <- cbind(d, scores)
        if (nrow(roc.data) == 0) {
          roc.data <- d
        } else {
          roc.data <- rbind(roc.data, d)
        }
      }
    }
  }
  return(roc.data)
}

