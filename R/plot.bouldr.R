#' Plot the ROC objects in a pretty way, with faceting when appropriate
#'
#'
#' @param x An object of class `bouldr`
#' @param point_size Sets the size of the  points; default is 0 (no points)
#' @param line_size Sets the size of the line; default is 1
#' @param bw logical; print the plot in black and white with shapes & linetypes?
#' @param shapes logical; print shapes and linetypes (on color plots)
#' @param ncol numeric; for faceted plots, specifies number of columns
#' @param nrow numeric; for faceted plots, specifies number of rows
#' @param facet_order string; for faceted plots, specifies the order of the facets
#' @param ... Additional arguments
#'
#' @return A ggplot2 object with relevant plot(s)
#' @export
plot.bouldr <- function(x, point_size = 0, line_size = 1, bw = FALSE, shapes = TRUE, ncol = NULL, nrow = NULL, facet_order = NULL, ...) {
  # Plots the rocs, give output from main
  #facet_order should be a named

  if (!inherits(x, 'bouldr')) {
    stop("input must be of type 'bouldr'")
  }
  roc.data <- tumble_rocs(x)

  if (!is.null(facet_order)) {
    if(!all(unique(facet_order) %in% roc.data$Facet)) {
      stop("not all values in facet_order represented in data")
    }
    roc.data$Facet <- factor(roc.data$Facet, levels=facet_order)
  }

  roc.data$FPR <- with(roc.data, 1 - sens)

  if (bw) {
    mapping <- ggplot2::aes_string(x = "FPR", y = "spec", shape = "Group", linetype = "Group")
  }
  if (!bw && shapes) {
    mapping <- ggplot2::aes_string(x = "FPR", y = "spec", color = "Group", shape = "Group", linetype = "Group")
  }
  if (!bw && !shapes) {
    mapping <- ggplot2::aes_string(x = "FPR", y = "spec", color = "Group")
  }
  if (bw && !shapes) {
    warning("how are you going to tell the lines apart??")
  }

  p <- ggplot2::ggplot(roc.data, mapping)+
    ggplot2::geom_point(size = point_size)+
    ggplot2::geom_line(size = line_size)+
    ggplot2::geom_abline(slope = 1, intercept = 0, color = 'grey40')+
    ggplot2::labs(x = "1 - Sensitivity", y = "Specificity") +
    ggplot2::theme_classic()

  if (x$type == "faceted") {
    p <- p + ggplot2::facet_wrap(~Facet, nrow = nrow, ncol = ncol)
  }

  return(p)
}
