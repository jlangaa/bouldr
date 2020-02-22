# Generate test data
# Josh Langfus
# Feb 19 2020

## Generate a test data frame with 6 nested variables
##
## Usage:
## gen.test.data(50)
## outputs a test data frame with 50 observations per condition
##
## Output:
## ID: participant ID
## Diagnosis: the type of diagnosis
## Measure: the form used to measure symptoms
## Informant: the person taking the form
## Outcome: whether the diagnosis is present
## Score: the score on the measure
gen.test.data <- function(obs.per,
                          Diagnosis = c("Depression","Anxiety"),
                          Measure = c("A","B","C","D","E"),
                          Informant = c("Parent","Teacher","Self")) {
  
  if (!is.numeric(obs.per))
    stop("Error: Please specify number of observations per condition (obs.per must be defined)")
  
  roc.data <- data.frame()
  
  for (D in Diagnosis) {
    for (M in Measure) {
      for (I in Informant) {
        separation <- runif(1)
        sd.pooled <- runif(1,1,5)
        
        g1 <- data.frame(Outcome = "no",
                         Score = rnorm(obs.per,0,sd.pooled)
        )
        g2 <- data.frame(Outcome = "yes",
                         Score = rnorm(obs.per,separation, sd.pooled))
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

