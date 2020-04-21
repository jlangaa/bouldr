# Main method ----------------------------------------------------------------
#
# Purpose: Run ROCs and pairwise comparisons using desired test
#
# Input:
#   dat   a data frame containing the response and predictor vars
#   f     a formula of form [response] ~ [predictor] + [group1] + [group2]
#     note: group1 and group2 are optional; all test will be within group1 only
#   test  a string indicating which test is desired; default is delong
#     Options are: "delong", "bootstrap","venkatraman", "sensitivity", "specificity"
# Output:
#   a list with two top-level objects
#     rocs  the roc objects
#     tests the output of the desired tests
#     Class: bouldr
#     
#     
## Load dependencies
require(tidyr)
require(dplyr)
require(magrittr)
require(pROC)
require(RcppAlgos)
require(broom)

bouldr <- function(dat, f, levels, direction, test = "delong", ...) {
  
  ### Useful definitions
  ret <- list()
  roclist <- list()
  testlist <- list()
  
  ### Parse formula and extract variable names
  allvars <- all.vars(f)
  
  if (!(all(allvars %in% colnames(dat)))) {
    stop("Missing variables in data")
  }
  
  nvars <- length(allvars)
  
  out <- allvars[1]
  pred <- allvars[2]
  
  if (nvars > 2) {
    grouping.vars <- allvars[3:nvars] 
  }
  
  ### Do ROC tests
  
  if (nvars == 2) {
    ### If there are no additional grouping variables, there is only
    ### one curve. Check that it is significantly different from AUC = .5
    
    real.roc <- roc_(data = dat, response = out, predictor = pred ,
                     levels = levels,
                     direction = direction)
    
    ## Generate a roc with random guessing for predictor (same sample size)
    random.roc <- roc(predictor = dat[,pred],
                      response = sample(unique(dat[,out]),
                                        length(dat[,out]),replace = TRUE),
                      levels = levels,
                      direction = direction)
    
    roclist <-  real.roc
    testlist <- tidy(roc.test(real.roc, random.roc, method = test))
  }
  
  if (nvars == 3) {
    ### If there is one grouping variable, run a ROC for each level
    ### and compare every level to every other
    grp.var <- grouping.vars[1]
    
    ## Run the ROCs
    for (g in unique(dat[,grp.var])){
      d <- filter(dat, get(grp.var) == g)
      roclist[[g]] <- roc_(data = d, response = out, predictor = pred,
                           levels = levels,
                           direction = direction)
    }
    
    ## Pairwise comparisons
    comboList <- comboGeneral(names(roclist), m = 2)
    
    testlist <- apply(comboList, 1, function(x) { roc.test(roclist[[x[1]]], roclist[[x[2]]], method = test) } )
    testlist <- bind_rows(lapply(testlist, tidy))
    testlist[paste0(grp.var,"2")] <- comboList[,2]
    testlist[paste0(grp.var,"1")] <- comboList[,1]
    
    nc <- ncol(testlist)
    testlist <- testlist[,c(nc,nc-1,2:nc-2)]
  }
  
  if (nvars == 4) {
    grp.var <- grouping.vars[1]
    facet.var <- grouping.vars[2]
    
    for (fv in unique(dat[,facet.var])) {
      ## Run the ROCs
      roc.facet <- list()
      test.facet <- list()
      
      for (g in unique(dat[,grp.var])){
        d <- filter(dat, get(grp.var) == g, get(facet.var) == fv)
        roc.facet[[g]] <- roc_(data = d, response = out, predictor = pred,
                               levels = levels,
                               direction = direction)
      }
      
      ## Pairwise comparisons
      comboList <- comboGeneral(names(roc.facet), m = 2)
      
      test.facet <- apply(comboList, 1, function(x) { roc.test(roc.facet[[x[1]]], roc.facet[[x[2]]], method = test) } )
      # names(test.facet) <- apply(comboList, 1, function(x) { paste(x[1],x[2],sep="_") })
      
      test.facet <- bind_rows(lapply(test.facet, tidy))
      test.facet[facet.var] <- fv
      test.facet[paste0(grp.var,"2")] <- comboList[,2]
      test.facet[paste0(grp.var,"1")] <- comboList[,1]
      
      nc <- ncol(test.facet)
      test.facet <- test.facet[,c(nc,nc-1, nc-2, 3:nc-3)]
      
      roclist[[fv]] <- roc.facet
      testlist[[fv]] <- test.facet
    }
    testlist <- bind_rows(testlist)
    
  }
  if (nvars > 4) {
    stop("Too many grouping variables! Don't get greedy!")
  }
  
  ret[["rocs"]] <- roclist
  ret[["tests"]] <- testlist
  ret[["stat"]] <- test
  ## Indicate how much nesting exists
  ret[["type"]] <- case_when(
    nvars == 2 ~ "single",
    nvars == 3 ~ "grouped",
    nvars == 4 ~ "faceted",
    TRUE ~ "invalid"
  )
  
  class(ret) <- 'bouldr'
  return(ret)
}

tests <- function(rocbag) {
  # Convenience function to pull out the table of test results
  return(rocbag$tests)
}

aucs <- function(rocbag) {
  # Create a table of AUC for each roc
  aucs <- NULL
  
  if (rocbag$type == 'single') {
    r <- rocbag$rocs
    
    aucs <- tibble(
      Comparison = paste(r$levels[1], r$direction, r$levels[2]),
      AUC = r$auc
    )
  }
  
  if (rocbag$type == 'grouped') {
    aucs <- list()
    rocs <- rocbag$rocs
    
    for (group in names(rocs)) {
      r <- rocs[[group]]
      
      aucs[[group]] <- tibble(
        Group = group,
        Comparison = paste(r$levels[1], r$direction, r$levels[2]),
        AUC = unclass(r$auc)[1]
      )
    }
    aucs <- bind_rows(aucs)
  }
  
  if (rocbag$type == 'faceted') {
    aucs <- list()
    rocs <- rocbag$rocs
    
    for (facet in names(rocs)) {
      a <- list()
      
      for (group in names(rocs[[facet]])) {
        r <- rocs[[facet]][[group]]
        
        a[[group]] <- tibble(
          Facet = facet,
          Group = group,
          Comparison = paste(r$levels[1], r$direction, r$levels[2]),
          AUC = unclass(r$auc)[1]
        )
        
      }
    aucs[[facet]] <- bind_rows(a)
    }
    aucs <- bind_rows(aucs)
  }
  return(aucs)
}

rocs <- function(rocbag) {
  # Convenience function to pull out the list of roc objects
  return(rocbag$rocs)
}