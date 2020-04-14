#-------------------------------------------------------------------------------------------------------------
#MODULE 2: getROC test statistics
#-------------------------------------------------------------------------------------------------------------
#
#
# PURPOSE: get all unique combinations of ROCs produced by module 1
#          by outcome variable name and within each outcome pass the ROCs to roc.test to get
#          the following ROC comparison statstics for every unique combination of 2x2 ROCs within the outcome 
#          group.  The following compairison statistics are available:
#             (a) delong test
#             (b) bootstrap method 
#             (c) venkatraman test
#             (d) senstivity
#              (e) specificity
#            See pROC documentation and associated publications for details
# 
# OUTPUT: Code creates object with all ROC infromation from module 1 and the requested comparison statistic 
#          for each unique 2x2 comparison within outcomesreturns
# 
# REQUIRES: (1)pROC 
#           (2)RcppAlgos
# 
# 
# THIS FUNCTION IS CALLED AS FOLLOWS:
#
# compStatsROC(m1Data= , 
#              compareSTAT= , 
#              cutpoint=   )
# where 
#   m1Data is the name user gave to output from m1
#   compareSTAT is the type of ROC compairison test statistic user wants 
#   cutpoint is only used if the user requests sensitivity or
#   specificity statististics.  It should be entered as a proportion (see example below)
#
# EXAMPLE USING TESTDATA:
#  <note: name for type of statistical test must be entered in " ">
#  
#  getDelongStat <- compStatsROC(fitGroupedROCs,"delong")
#  
#  getSpecificity <- compStatsROC(fitGroupedROCs,"specificity", 0.9)
#
# SOME OUTSTANDING TO DOS:
#
# (1) figure out how to deal with the warnings for improper test-- can we identify and issue a message
#     before user runs the data or automatically pull those compairisons from the data? 
# (2) if user wants multiple methods they have to run this for each method and the ROC curve info
#     will produce a large list for each one.  Could we supress some of this?
# (3) add code to check for and install needed packages?
#
#---------------------------------------------------------------------------------------------------------

  
#STEP 1: GET UNIQUE LIST OF MEASURES AND INFORMANTS BY OUTCOME

compStatsROC <- function(m1data, compSTAT, cutpoint) {
  
#declare needed hash and list objects
groupNameHash<- new.env(hash=TRUE)
groupNameHashCount<- new.env(hash=TRUE)
groupNameHashIndex<- new.env(hash=TRUE) 
groupNameSplit<- list()

#get ROC grouping combination information by diagnosis
#note--> this will be used later to pass into roc.test from pROC in order 
#        to get statistical tests for all unique combinations of ROC curves within diagnosis 
#        by the grouping variables.  The ROC curves come from running in module 1

for(i in 1:length(m1data[["groupNames"]])){
  #separate ROCs for each outcome from module 1 into the measure and grouping information components for use in determining combination pairings later
  groupNameSplit[[i]]<- unlist(strsplit(fitGroupedROCs[["groupNames"]][[i]], "[.]"))
  #create object with outcome as key to use for hash objects
  outcome <- groupNameSplit[[i]][[1]]
  #get unique pairings for groupings of interst (concatonate group 1 and group 2) and store combined name to groupNameSplit list in position 4
  groupNameSplit[[i]][[4]]<- paste(groupNameSplit[[i]][[2]], groupNameSplit[[i]][[3]], sep=".")
  #save index on split in postion 5 of groupNameSplit list
  groupNameSplit[[i]][[5]]<- i
  #create list that contains unique pairings for grouping variables by each outcome using hash object
  if (!exists(outcome,groupNameHash)){
    groupNameHash[[outcome]] <- list()
    groupNameHashIndex[[outcome]] <- list()
    groupNameHashCount[[outcome]] <- 1
  }
  
  Count<-groupNameHashCount[[outcome]] 
  groupNameHash[[outcome]][[Count]] <- groupNameSplit[[i]][[4]]
  #adds index to list of indexes
  groupNameHashIndex[[outcome]][[Count]] <- i
  groupNameHashCount[[outcome]] <- Count + 1
}

#STEP 2: GET ALL UNIQUE COMBINATION PAIRS FROM groupNameHashIndex BY OUTCOME 
#goal is to use key to get position by each outcome and store it in a list

#declare needed objects 
resultsM2 <-list()
resultsM2Index <-1 
comboGroupVars<- new.env(hash=TRUE) #-->object used to store unique combinations of pairings for ROC test

#Step 1: get unique combinations by outcome
#get outcome names so they can be passed to has object
outcomeNames<- strsplit(ls(groupNameHashIndex), " ")

#create unique combinations to pass to roc.test
for (outcome in outcomeNames){
  #create unique combinations using package comboGeneral (note:comboGeneral takes only vectors for 1st argument)
  comboGroupVars[[outcome]]<- comboGeneral(unlist(groupNameHashIndex[[outcome]]), m = 2)
  #genrate roc1 and roc2 etc...syntax for passing to roc.test
  for (i in 1:length(comboGroupVars[[outcome]][,1])){
    rocleft <- fitGroupedROCs[["ROCData"]][[comboGroupVars[[outcome]][[i,1]]]]
    rocright <- fitGroupedROCs[["ROCData"]][[comboGroupVars[[outcome]][[i,2]]]]
#Step 2: Call roc.test    
    #call roc.test
    rocOutput <- roc.test(rocleft,rocright, method= compSTAT, cutpoint)
#Step 3: output results to user
    #output results of roc.test and information about outcome, grouping variable names used for each test to a list
    resultsM2[[resultsM2Index]] <- list(outcome=outcome, "1st Grouped ROC(roc1)"=groupNameSplit[[comboGroupVars[[outcome]][[i,1]]]][[4]], "2nd Grouped ROC(roc2)"=groupNameSplit[[comboGroupVars[[outcome]][[i,2]]]][[4]], ROCTestResults=rocOutput)
    resultsM2Index <- resultsM2Index + 1
  }
}
return(resultsM2)
}

            