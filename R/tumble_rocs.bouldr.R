#' An internal function for extracing data from bouldr objects for plotting
#'
#' @param rocbag The bouldr object to manipulate
#'
#' @return A data frame with plottable data
#'
tumble_rocs <- function(rocbag) {
  ret <- list()
  rocs <- rocbag$rocs
  #Pulls out the data from the rocs
  if (rocbag$type == "single")  {
    ret$data <- data.frame(
      sens = rocs$sensitivities,
      spec = rocs$specificities,
      Predictor = "",
      Group = "",
      stringsAsFactors = FALSE
    )
  }

  if(rocbag$type == "grouped") {
    for (g in names(rocs)){
      ret[[g]] <- data.frame(
        Group = g,
        Facet = "",
        sens = rocs[[g]]$sensitivities,
        spec = rocs[[g]]$specificities,
        stringsAsFactors = FALSE
      )
    }
  }

  if(rocbag$type == "faceted") {
    for (f in names(rocs)) {
      for (g in names(rocs[[f]])) {
        ret[[paste0(g,f)]] <- data.frame(
          Group = g,
          Facet = f,
          sens = rocs[[f]][[g]]$sensitivities,
          spec = rocs[[f]][[g]]$specificities,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  return(dplyr::bind_rows(ret))
}
