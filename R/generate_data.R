#' Title: Generate a test data frame for use with `bouldr`
#'
#' Description: Generates a data frame of the correct format to be used
#' with `bouldr`.
#'
#' By default, the output data frame frame contains the following variables:
#'
#' - ID: participant ID
#' - Diagnosis: the type of diagnosis. One or more -- default 2: "Depression" and "Anxiety"
#' - Measure: the form used to measure symptoms. One or more -- default 5: (A through E)
#' - Informant: the person taking the form. One or more -- default 3: "Parent", "Teacher" and "Self"
#' - Outcome: whether the diagnosis is present. Must be dichotomous -- default: "yes" and "no"
#' - Score: the score on the measure. A (continuous) numeric variable.
#'
#' Note that these variable names are arbitrary; they have been
#' chosen here to reflect common use-cases.
#'
#' @param obs.per Number of observations per cell. Total _N_ would be this number times the number of options in the other three variables.
#' @param Diagnosis A character vector of names for this grouping variable
#' @param Measure  A character vector of names for this grouping variable
#' @param Informant A character vector of names for this grouping variable
#'
#' @return A data frame that can be used in `bouldr`
#' @export
#' @examples
#' generate_data(10)
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

