# Main method ----------------------------------------------------------------

## Load dependencies
require(tidyr)
require(magrittr)
require(pROC)
require(RcppAlgos)

main <- function(dat, f, test = "delong", ...) {
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
    grouping.vars <-allvars[3:nvars] 
  }
  
  ### Do ROC tests
  
  if (nvars == 2) {
    ### If there are no additional grouping variables, there is only
    ### one curve. Check that it is significantly different from AUC = .5
    
    real.roc <- roc_(data = dat, response = out, predictor = pred)
    ## Generate a roc with random guessing for predictor (same sample size)
    random.roc <- roc(predictor = dat[,pred], response = sample(unique(dat[,out]),length(dat[,out]),replace=T))
    
    roclist <-  real.roc
    testlist <- roc.test(real.roc, random.roc, method = test)
    
    ## TODO: will probably want to simulate this many times
    ## or use venkatramen test.
  }
  
  if (nvars == 3) {
    ### If there is one grouping variable, run a ROC for each level
    ### and compare every level to every other
    grp.var <- grouping.vars[1]
    
    ## Run the ROCs
    for (g in unique(dat[,grp.var])){
      d <- filter(dat, get(grp.var) == g)
      roclist[[g]] <- roc_(data = d, response = out, predictor = pred)
    }
    
    ## Pairwise comparisons
    comboList <- comboGeneral(names(roclist), m=2)
    
    testlist <- apply(comboList, 1, function(x) { roc.test(roclist[[x[1]]], roclist[[x[2]]], method = test) } )
    names(testlist) <- apply(comboList, 1, function(x) { paste(x[1],x[2],sep="_") })
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
        roc.facet[[g]] <- roc_(data = d, response = out, predictor = pred)
      }
      
      ## Pairwise comparisons
      comboList <- comboGeneral(names(roc.facet), m = 2)
      
      test.facet <- apply(comboList, 1, function(x) { roc.test(roc.facet[[x[1]]], roc.facet[[x[2]]], method = test) } )
      names(test.facet) <- apply(comboList, 1, function(x) { paste(x[1],x[2],sep="_") })
      
      roclist[[fv]] <- roc.facet
      testlist[[fv]] <- test.facet
    }
  }
  if (nvars > 4) {
    stop("Too many grouping variables! Don't get greedy!")
  }
  
  ret[["rocs"]] <- roclist
  ret[["tests"]] <- testlist
  return(ret)
}

#TODO:
# use tidy to make ret$tests a data frame instead of a list
