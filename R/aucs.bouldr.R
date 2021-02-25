#' Build a table of AUCs for each ROC
#'
#' @param rocbag A `bouldr`-class object

#'
#' @return A data frame with the AUCs
#' @export
aucs <- function(rocbag) {
  # Create a table of AUC for each roc
  aucs <- NULL

  if (rocbag$type == 'single') {
    r <- rocbag$rocs

    aucs <- tidyr::tibble(
      Comparison = paste(r$levels[1], r$direction, r$levels[2]),
      AUC = r$auc
    )
  }

  if (rocbag$type == 'grouped') {
    aucs <- list()
    rocs <- rocbag$rocs

    for (group in names(rocs)) {
      r <- rocs[[group]]

      aucs[[group]] <- tidyr::tibble(
        Group = group,
        Comparison = paste(r$levels[1], r$direction, r$levels[2]),
        AUC = unclass(r$auc)[1]
      )
    }
    aucs <- dplyr::bind_rows(aucs)
  }

  if (rocbag$type == 'faceted') {
    aucs <- list()
    rocs <- rocbag$rocs

    for (facet in names(rocs)) {
      a <- list()

      for (group in names(rocs[[facet]])) {
        r <- rocs[[facet]][[group]]

        a[[group]] <- tidyr::tibble(
          Facet = facet,
          Group = group,
          Comparison = paste(r$levels[1], r$direction, r$levels[2]),
          AUC = unclass(r$auc)[1]
        )

      }
      aucs[[facet]] <- dplyr::bind_rows(a)
    }
    aucs <- dplyr::bind_rows(aucs)
  }
  return(aucs)
}
