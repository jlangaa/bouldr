## Plotting -------------------------------------------------------------------
#
# Purpose: plot the ROC objects in a pretty way, with faceting when appropriate
# 
require(ggplot2)

tumble_rocs <- function(rocbag) {
  ret <- list()
  rocs <- rocbag$rocs
  #Pulls out the data from the rocs
  if (rocbag$type == "single")  {
    ret <- data.frame(
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
  return(bind_rows(ret))
}

rocplot <- function(rocbag, ...) {
  # Plots the rocs, give output from main
  if (class(rocbag) != 'bouldr') {
    stop("input must be of type 'bouldr'")
  }
  roc.data <- tumble_rocs(rocbag)
  
  p <- ggplot(roc.data, aes(x = 1 - sens, y = spec, color = Group))+
    geom_point(size = .5)+
    geom_line()+
    geom_abline(slope = 1, intercept = 0, color = 'grey40')+
    labs(x = "1 - Sensitivity", y = "Specificity") +
    theme_classic()
  
  if (rocbag$type == "faceted") {
    p <- p + facet_wrap(~Facet)
  }
  
  return(p)
}
