######################################################################
# Script to create datasets for practice effect adjustments in MCI   #
# pipeline. This script should be run before calculatinf practice    #
# effects.                                                           #
#                                                                    #
# Inputs:                                                            #
# --------------                                                     #
#   dataV1: VETSA 1 dataset including variables needed for MCI Dx    #
#   dataV2: VETSA 2 dataset including variables needed for MCI Dx    #
#   dataInfo: Dataset with subject info needed to adjustment. This   #
#         includes: vetsaid, case, NAS201TRAN, VETSAGRP              #
#   dataExclude: Dataset specifying subjects who should be excluded  #
#         from MCI Dx (e.g., stroke or large tumor). This            #
#         should include vetsaid and a variable called               #
#         AnyMCIExclude with values 0=include and 1=exclude          #
#                                                                    #
# Output:                                                            #
#----------------                                                    #
#   V1V2_RawScores.csv: Unadjusted VETSA 1/2 scores combined into    #
#         one dataset                                                #
#   V1V2_NAS201TRAN_Adj.csv: VETSA 1/2 scores that have been         #
#         adjusted for age 20 AFQT. All scores will have the a       #
#         suffix of "_adj"                                           #
#                                                                    #  
#                                                                    #
#                                                                    #
#                                                                    #
# The script adjusts V1 and V2 test scores by regressing out age 20  # 
# AFQT (NAS201TRAN). Mixed effects models are used to account for    #
# twin pairs, and the intercept is added back in to retain mean      #
# level differences.                                                 #
######################################################################

# Import libraries
library(dplyr)
library(lme4)

# Create list of raw variable names to adjust
rawVarsV1 = c("VRCTOTSS","MTXT","DSPSS","SSPSS","LNSC","TRL1TSC","TRL4TSC",
              "STRIT","LFCORSC","CFCORSC","CSSACCSC","MR1CORZ","HFTOTCORZ",
              "CVLT","LM","VR","TRL","STR")
rawVarsV2 = paste0(rawVarsV1, "_V2")
rawVarsV3 = paste0(rawVarsV1, "_V3")

# Print variable names and verify these are correct
rawVarsV1
rawVarsV2
rawVarsV3

# Load raw test scores and demographics data
dataV1 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect MCI/VETSA3/Data/V1MCI_Components.csv",
                   stringsAsFactors = FALSE)
dataV1 = dataV1 %>% select(vetsaid, rawVarsV1)
dataV2 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect MCI/VETSA3/Data/V2MCI_PrePracticeEffectAdj.csv",
                  stringsAsFactors = FALSE)
dataV2 = dataV2 %>% select(vetsaid, rawVarsV2)
dataV3 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect MCI/VETSA3/Data/V3MCI_PrePracticeEffectAdj_TestingOnly.csv",
                  stringsAsFactors = FALSE)
dataV3 = dataV3 %>% select(vetsaid, rawVarsV3)

dataInfo = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect MCI/VETSA3/Data/SubjectInfo_TestingOnly.csv")
dataExclude = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect MCI/VETSA3/Data/V1V2MCI_Exclude.csv", stringsAsFactors = F)

allData = dataV1 %>% full_join(dataV2, by="vetsaid")
allData = allData %>% full_join(dataV3, by="vetsaid")
allData = dataInfo %>%  
  dplyr::select(vetsaid, case, NAS201TRAN, VETSAGRP) %>%
  right_join(allData, by="vetsaid") %>%
  left_join(dataExclude, by="vetsaid") %>%
  filter(AnyMCIExclude!=1 & !is.na(NAS201TRAN)) %>%
  dplyr::select(-V1MCIExclude, -V2MCIExclude, -AnyMCIExclude)

names(allData) = toupper(names(allData))
str(allData)

write.csv(allData, "/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect MCI/VETSA3/Data/V1V2V3_RawScores_TestingOnly.csv", row.names=FALSE)



#----------------------------------------------------------------------------#
#                     Define functions                                       #
#----------------------------------------------------------------------------#

adjustDataset = function(regVars,adjVars,nDemoVars=7,data){
  #######################################################################
  # Adjust dataset for specified set of variables.Regresses passed      #
  # variables from each measure using linear regression. The intercept  #
  # is added back in to retain mean level information.                  #
  # Input:                                                              #
  # regVars = List of variables to regress out                          #
  # adjVars = List of variables to be adjusted                          #
  # nDemoVars = Number of demographic variables included in dataframe.  #
  #             These should be the first 1:nDemoVars columns of        #
  #             the dataframe                                           #
  #######################################################################
  
  # Read variable names from data and store in a vector
  allNames <- names(data)
  
  #*** Check variables are correct
  nVars <- length(adjVars)	
  
  ### Creating Storate Data Frame ###
  
  # Set number of individuals 
  n <- dim(data)[1]
  tot <- dim(data)[2]
  
  # Create Data Frame
  data <- cbind(data,matrix(NA,nrow=n,ncol=nVars))
  names(data) <- c(allNames,paste(adjVars,"_adj",sep=""))
  
  ### Running Loop Using lapply ###
  
  # fitting models
  models <- lapply(adjVars, function(x) {
    fmla = as.formula(paste0(x," ~ ",regVars," + (1|CASE)"))
    lmer(formula=fmla, data = data, na.action=na.exclude)
  })
  
  # storing residuals from each model into data frame
  for(v in 1:nVars){
    data[,tot+v] <- residuals(models[[v]]) + fixef(models[[v]])[[1]]
  }
  
  #dataR is now your residualized parameters
  dataR <- data[,c(1:nDemoVars,(tot+1):(tot+nVars))]
  dataR
}


########################################
### Begin creating adjusted datasets ###
########################################

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for NAS201TRAN (Age 20 AFQT)                              #
#                                                                                   #
# Adjustment consists of regressing out nuisance variable from raw variables.       # 
# Intercept is added back in to avoid mean centering.                               #
#-----------------------------------------------------------------------------------#

# Adjust raw scores from VETSA 1 and VETSA 2
adjVars = c(rawVarsV1, rawVarsV2)

# Set number of demographic variables included in dataframe (these won't be adjusted)
nDemoVars = 4

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$NAS201TRAN))

# Specify NAS201TRAN (Age 20 AFQT as variable to regress out)
regVars = paste("scale(NAS201TRAN)", sep=" + ")

# Regress NAS201TRAN out of dataset
nasAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Save out dataset with Age 20 AFQT regressed out
write.csv(nasAdjRawScoresData, "/home/jelman/netshare/K/Projects/PracEffects_MCI/data/V1V2_NAS201TRAN_Adj.csv",
          row.names=F)
