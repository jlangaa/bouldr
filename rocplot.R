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
  
  if(rocbag$type == "multiple") {
    for (r in names(rocs)){
      ret[[r]] <- data.frame(
        Predictor = r,
        Group = "",
        sens = rocs[[r]]$sensitivities,
        spec = rocs[[r]]$specificities,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if(rocbag$type == "nested") {
    for (g in names(rocs)) {
      for (r in names(rocs[[g]])) {
        ret[[paste0(r,g)]] <- data.frame(
          Predictor = r,
          Group = g,
          sens = rocs[[g]][[r]]$sensitivities,
          spec = rocs[[g]][[r]]$specificities,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  return(bind_rows(ret))
}

rocplot <- function(rocbag,plot=TRUE, ...) {
  # Plots the rocs, give output from main
  
  roc.data <- tumble_rocs(rocbag)
  
  p <- ggplot(roc.data, aes(x = 1 - sens, y = spec, color = Predictor))+
    geom_point()+
    geom_line()+
    geom_abline(slope = 1, intercept = 0, color = 'grey40')+
    labs(x = "1 - Sensitivity", y = "Specificity") +
    theme_classic()
  
  if (rocbag$type == "nested") {
    p <- p + facet_wrap(~Group)
  }
  
  return(p)
}
