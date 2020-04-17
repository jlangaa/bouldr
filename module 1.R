#-------------------------------------------------------------------------------------------------------------
#MODULE 1: createGroupings
#-------------------------------------------------------------------------------------------------------------
#
#
# PURPOSE: 
# Code creates ROC groupings by diagnosis and 2 nesting variables and returns
# list object that contains the following
#           (a) postion and name of each grouping created
#           (b) ROCDATA which contains ROC results for each
#               unique grouping
#         This object is used to call statistical tests to 
#         compare ROC curves and AUC in Module 2
#
# REQUIRES: pROC and its dependencies
#
# THIS FUNCTION IS CALLED AS FOLLOWS:
#
# createGroupings(datasetName = , 
#                trueDX= , 
#                measureName=, 
#                informant=, 
#                outcomeVar=, 
#                predVar= )
# where 
#   datasetName is the user's input dataset in long form
#   trueDX is patient diagnosis
#   measureName is column containing form
#   informant is column containg indicator of respondant to the measure (e.g. self, parent)
#   outcomeVar is column containing whether the person was diagnosed by the measure (e.g. "yes", "no" )
#   predVar is variable predicting outcome (e.g. score on measure)
#
# EXAMPLE USING TESTDATA:
#  <note: column names must be entered in " ">
#  fittedGroupedROCs <-createGroupings(testData, "Diagnosis", "Measure", "Informant", "Outcome", "Score") 
#  
#
# SOME OUTSTANDING TO DOS:
#
# (1) make sure function allows for nondefault pROC options
# (2) produce message that tells people # of unique pairings that were created so they can datacheck
#     (perhaps not needed)?
# (3) add code to check for and install needed packages?
#
#---------------------------------------------------------------------------------------------------------
#### STEP 0: LOAD DEPENDENCIES
require(pROC)

#### STEP 1: SUBSET DATA
createGroupings <- function(datasetName, trueDX, measureName, informant, outcomeVar, predVar ) {
  
  #create unique pairings of grouping factors
  getGroupings <- split(datasetName, list(datasetName[,trueDX], datasetName[,measureName], datasetName[,informant]))
  
#### STEP 2: GENERATE AND STORE ROC DATA
  
  #create list that stores ROC datasets for all pairings
  getROCData <- list()
  
  #produce ROC results
  for(i in 1:length(getGroupings)){
    getROCData[[i]] <-roc(getGroupings[[i]][,outcomeVar], getGroupings[[i]][,predVar], plot=TRUE)
  }
  
#### STEP 3: RETURN RESULTS TO USER 
  
  #get grouping names to return to user
  groupingNames <- names(getGroupings)
  
  resultsM1 <- list("groupNames"= groupingNames,"ROCData"=getROCData) 
  return(resultsM1)
}