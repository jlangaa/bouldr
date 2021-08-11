#' Run ROCs and pairwise comparisons using desired test
#'
#' This function takes a data frame and a formula and runs nested ROCs
#' as appropriate. Also required are the levels of the response variable
#' and the direction of the cases (i.e., which level is the positive case).
#' Providing an argument for test will change the test used to compare
#' ROC curves.
#'
#' @param data A data frame containing the response and predictor vars
#' @param formula A specification of the variables of form: response ~ predictor + (group1) + (group2)
#' @param levels The levels of the response variable as a character vector (e.g., C("yes","no"))
#' @param direction Either "<" or ">", indicating which level is the positive case
#' @param test Which test should be used to compare ROCs (optional). Default is delong
#' @param ... Additional arguments
#'
#' @return An object of class 'bouldr' containing ROCs and tests.
#' @export
#' @examples
#' dd <- generate_data(10,Diagnosis = c("Depression","Anxiety","ADHD"),
#'         Measure = c("A","B"), Informant = c("Self","Parent", "Teacher"))
#' single <- bouldr(dat = dd,
#' f = Outcome ~ Score,
#' test = 'delong',
#' levels = c('no','yes'),
#' direction = "<")
#' aucs(single)



bouldr <- function(formula, data, levels, direction = "auto", test = "delong", ...) {
# 3/17/21 - Issue 5 - Remove clunky required arguments from bouldr function call https://github.com/jlangaa/bouldr/issues/5
#   Added 'auto' to direction parameter per pROC settings: automatically define in which group the median is higher and take the good direction to have an AUC >= 0.5


  ### Cast data as data.frame
  data <- as.data.frame(data)

  ### Useful definitions
  ret <- list()
  roclist <- list()
  testlist <- list()

  ### Parse formula and extract variable names
  allvars <- all.vars(formula)

  if (!(all(allvars %in% colnames(data)))) {
    stop("Missing variables in data")
  }

  nvars <- length(allvars)

  out <- allvars[1]
  pred <- allvars[2]

  if (nvars > 2) {
    grouping.vars <- allvars[3:nvars]
  }

  ### Remove rows where response var is NA
  n.orig <- nrow(data)

  data <- data[!is.na(data[,out]),]

  n.no.na <- nrow(data)

  ### Do ROC tests

  if (nvars == 2) {
    ### If there are no additional grouping variables, there is only
    ### one curve. Check that it is significantly different from AUC = .5
    if(length(unique(data[,out])) == 1) {
      stop("Skipping ROC due to no case differentiation in response group", call. = FALSE)
    } else {
      real.roc <- pROC::roc_(data = data, response = out, predictor = pred ,
                             levels = levels,
                             direction = direction)

    ## Generate a roc with random guessing for predictor (same sample size)
    # random.roc <- roc(predictor = data[,pred],
    #                   response = sample(unique(data[,out]),
    #                                     length(data[,out]),replace = TRUE),
    #                   levels = levels,
    #                   direction = direction)

    roclist <-  real.roc
    # testlist <- tidy(roc.test(real.roc, random.roc, method = test))

    # Do a Mann-Whitney U test to see if AUC > .5
    testlist <- broom::tidy(stats::wilcox.test(data[,pred] ~ data[,out], conf.int = TRUE))
    }
  }

  if (nvars == 3) {
    ### If there is one grouping variable, run a ROC for each level
    ### and compare every level to every other
    grp.var <- grouping.vars[1]

    ## Run the ROCs
    for (g in unique(data[,grp.var])){
      d <- dplyr::filter(data, get(grp.var) == g)
      if(length(unique(d[,out])) == 1) {
        warning("Skipping ROC due to no case differentiation in group: ", g, call. = FALSE)
      } else {
        roclist[[g]] <- pROC::roc_(data = d, response = out, predictor = pred,
                                   levels = levels,
                                   direction = direction)
      }
    }
    ## Pairwise comparisons
    comboList <- RcppAlgos::comboGeneral(names(roclist), m = 2)

    testlist <- apply(comboList, 1, function(x) { pROC::roc.test(roclist[[x[1]]], roclist[[x[2]]], method = test) } )
    testlist <- dplyr::bind_rows(lapply(testlist, broom::tidy))
    testlist[paste0(grp.var,"2")] <- comboList[,2]
    testlist[paste0(grp.var,"1")] <- comboList[,1]

    nc <- ncol(testlist)
    testlist <- testlist[,c(nc,nc-1,2:nc-2)]
  }

  if (nvars == 4) {
    grp.var <- grouping.vars[1]
    facet.var <- grouping.vars[2]

    for (fv in unique(data[,facet.var])) {
      ## Run the ROCs
      roc.facet <- list()
      test.facet <- list()

      for (g in unique(data[,grp.var])){
        d <- dplyr::filter(data, get(grp.var) == g, get(facet.var) == fv)

        if(length(unique(d[,out])) == 1) {
          warning("Skipping ROC due to no case differentiation in facet: ", fv,
                  ", group: ",g, call. = FALSE)
        } else {
          roc.facet[[g]] <- pROC::roc_(data = d, response = out, predictor = pred,
                                       levels = levels,
                                       direction = direction)
        }
      }

      #remove NA rocs (due to case uniformity)
      roc.facet <- roc.facet[!is.na(roc.facet)]

      ## Pairwise comparisons
      comboList <- RcppAlgos::comboGeneral(names(roc.facet), m = 2)

      test.facet <- apply(comboList, 1, function(x) { pROC::roc.test(roc.facet[[x[1]]], roc.facet[[x[2]]], method = test) } )
      # names(test.facet) <- apply(comboList, 1, function(x) { paste(x[1],x[2],sep="_") })

      test.facet <- dplyr::bind_rows(lapply(test.facet, broom::tidy))
      test.facet[facet.var] <- fv
      test.facet[paste0(grp.var,"2")] <- comboList[,2]
      test.facet[paste0(grp.var,"1")] <- comboList[,1]

      nc <- ncol(test.facet)
      test.facet <- test.facet[,c(nc,nc-1, nc-2, 3:nc-3)]

      roclist[[fv]] <- roc.facet
      testlist[[fv]] <- test.facet
    }
    testlist <- dplyr::bind_rows(testlist)

  }
  if (nvars > 4) {
    stop("Too many grouping variables! Don't get greedy!")
  }

  ret[["n.input"]] <- n.orig
  ret[["n.used"]] <- n.no.na
  ret[["rocs"]] <- roclist
  ret[["tests"]] <- testlist
  ret[["stat"]] <- test
  ret[["formula"]] <- formula
  ## Indicate how much nesting exists
  ret[["type"]] <- dplyr::case_when(
    nvars == 2 ~ "single",
    nvars == 3 ~ "grouped",
    nvars == 4 ~ "faceted",
    TRUE ~ "invalid"
  )

  class(ret) <- 'bouldr'
  return(ret)
}
