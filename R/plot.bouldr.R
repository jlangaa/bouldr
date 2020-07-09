#' Plot the ROC objects in a pretty way, with faceting when appropriate
#'
#'
#' @param x An object of class `bouldr`
#' @param ... Additional arguments
#'
#' @return A ggplot2 object with relevant plot(s)
#' @export
plot.bouldr <- function(x, ...) {
  # Plots the rocs, give output from main
  if (class(x) != 'bouldr') {
    stop("input must be of type 'bouldr'")
  }
  roc.data <- tumble_rocs(x)

  roc.data$FPR <- with(roc.data, 1 - sens)

  p <- ggplot2::ggplot(roc.data, ggplot2::aes_string(x = "FPR", y = "spec", color = "Group"))+
    ggplot2::geom_point(size = .5)+
    ggplot2::geom_line()+
    ggplot2::geom_abline(slope = 1, intercept = 0, color = 'grey40')+
    ggplot2::labs(x = "1 - Sensitivity", y = "Specificity") +
    ggplot2::theme_classic()

  if (x$type == "faceted") {
    p <- p + ggplot2::facet_wrap(~Facet)
  }

  return(p)
}
