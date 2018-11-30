######################################################################
# Script to create datasets for practice effect analyses.            #
# This script will create four datasets:                             #
#   - Cognitive data with nas201tran (age 20 AFQT) regressed out.    #      
#     Scores are not standardized.                                   #
#   - Cognitive data with above adjustment for nas201tran. Scores    #
#     are standardized (z-scored) based on VETSA1 means and sd.      #
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
# The script adjusts V1, V2, and V3 test scores by regressing out    #
# age 20 AFQT (NAS201TRAN). Mixed effects models are used to account #
# for twin pairs, and the intercept is added back in to retain mean  #
# level differences.                                                 #
######################################################################

library(dplyr)
library(haven)
library(readxl)


#---------------------------------------#
#           LOAD AND MERGE DATA         #
#---------------------------------------#

### Define variable names of interest and load data ###
# Create list of raw variable names to adjust
# rawVarsV1 = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
#               "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
#               "AFQTBXPCTTRAN","DSFRAW","DSBRAW","DSFMAX","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
#               "STRWRAW","STRCRAW","STRCWRAW","STRIT","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","RSATOT",
#               "SRTLMEANLOG","SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG","CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG",
#               "CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG")
rawVarsV1 = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
              "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
              "AFQTBXPCTTRAN","DSFRAW","DSBRAW","DSFMAX","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
              "STRWRAW","STRCRAW","STRCWRAW","STRIT","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","RSATOT")
rawVarsV2 = paste0(rawVarsV1, "_V2")
rawVarsV3 = paste0(rawVarsV1, "_V3")

# Print variable names and verify these are correct
rawVarsV1
rawVarsV2
rawVarsV3

# Load raw test scores and demographics data. Rename all columns to upper
dataV1 = read_sas("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_VETSA 1 & 2 DATA_MOST UP TO DATE 7_2_2015/VETSA 1 Aging Most up to date July 2 2015/vetsa1merged_22dec2016_nomiss.sas7bdat")
names(dataV1) = toupper(names(dataV1))

dataV2 = read_sas("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_VETSA 1 & 2 DATA_MOST UP TO DATE 7_2_2015/VETSA 2 Aging Most up to date July 2 2015/vetsa2merged_23dec2016_nomiss.sas7bdat")
names(dataV2) = toupper(names(dataV2))
rt_dataV2 = read.csv("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/Reaction Time Data V2/vetsa2_reactiontime_merge.csv")
names(rt_dataV2) = toupper(names(rt_dataV2))
dataV2 = dataV2 %>% left_join(rt_dataV2, by="VETSAID")

dataV3 = read.csv("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/vetsa3merged.csv")
names(dataV3) = toupper(names(dataV3))
names(dataV3)[7:length(names(dataV3))] = paste0(names(dataV3)[7:length(names(dataV3))], "_V3")

dataInfo = read.csv("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/SubjectInfo_TestingOnly.csv")
names(dataInfo) = toupper(names(dataInfo))


### Log transform timing data ### 
# Get names of variables to transform
# timeVarsV1 = c("TRL1T","TRL2T","TRL3T","TRL4T","TRL5T","SRTLMEAN","SRTLSTD","SRTRMEAN",
#                "SRTRSTD","SRTGMEAN","SRTGSTD","CHRTLMEAN","CHRTRMEAN","CHRTLSTD",
#                "CHRTRSTD","CHRTGMEAN","CHRTGSTD")
timeVarsV1 = c("TRL1T","TRL2T","TRL3T","TRL4T","TRL5T")
timeVarsLogV1 = paste0(timeVarsV1, "LOG")
timeVarsV2 = paste0(timeVarsV1, "_V2")
timeVarsLogV2 = paste0(timeVarsLogV1, "_V2")
timeVarsV3 = paste0(timeVarsV1, "_V3")
timeVarsLogV3 = paste0(timeVarsLogV1, "_V3")

# Transform
dataV1[timeVarsLogV1] = log(dataV1[timeVarsV1])                
dataV1 = dplyr::select(dataV1, -one_of(timeVarsV1))
dataV2[timeVarsLogV2] = log(dataV2[timeVarsV2])                
dataV2 = dplyr::select(dataV2, -one_of(timeVarsV2))
dataV3[timeVarsLogV3] = log(dataV3[timeVarsV3])                
dataV3 = dplyr::select(dataV3, -one_of(timeVarsV3))

# Select only variables to be adjusted
dataV1 = dataV1 %>% select(VETSAID, rawVarsV1)
dataV2 = dataV2 %>% select(VETSAID, rawVarsV2)
dataV3 = dataV3 %>% select(VETSAID, rawVarsV3)

### Merge all data and apply excludes ###
# Join all data
allData = dataV1 %>% full_join(dataV2, by="VETSAID")
allData = allData %>% full_join(dataV3, by="VETSAID")
allData = dataInfo %>%  
  dplyr::select(VETSAID, CASE, NAS201TRAN, VETSAGRP) %>%
  right_join(allData, by="VETSAID") 

# Exclude indidivuals missing age 20 AFQT data
allData = allData %>%
  filter(!is.na(NAS201TRAN)) 

### Save out unadjusted scores on raw score scale ###
write.csv(allData, 
          "~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1V2V3_CogData_Unadj.csv",
          row.names = FALSE)


#----------------------------------------------------------------------------#
#                     Define functions                                       #
#----------------------------------------------------------------------------#

adjustDataset = function(regVars,adjVars,nDemoVars=7,suffix="adj",data){
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
  names(data) <- c(allNames,paste(adjVars,suffix,sep="_"))
  
  ### Running Loop Using lapply ###
  
  # fitting models
  models <- lapply(adjVars, function(x) {
    fmla = as.formula(paste0(x," ~ ",regVars))
    lm(formula=fmla, data = data, na.action=na.exclude)
  })
  
  # storing residuals from each model into data frame
  for(v in 1:nVars){
    data[,tot+v] <- residuals(models[[v]]) + coef(models[[v]])[[1]]
  }
  
  #dataR is now your residualized parameters
  dataR <- data[,c(1:nDemoVars,(tot+1):(tot+nVars))]
  dataR
}


addScaleVals = function(df,varname, x) {
  ###########################################################
  # Save mean and SD of all variables into a dataframe      #
  # Input:                                                  #
  # df = Initialized dataframe to hold results              #
  # varname = String name of variable                       #
  # x = Scaled vector of data                               #
  ###########################################################
  meanVal = attr(x, which="scaled:center")
  sdVal = attr(x, which="scaled:scale")
  rbind(df, data.frame(Variable=varname, Mean=meanVal, SD=sdVal))
}



########################################
### Begin creating adjusted datasets ###
########################################

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for nas201tran (Age 20 AFQT)                              #
#                                                                                   #
# Adjustment consists of regressing out nuisance variable from raw variables.       # 
# Intercept is added back in to avoid mean centering.                               #
#-----------------------------------------------------------------------------------#

# Adjust raw scores from VETSA 1 and VETSA 2
adjVars = c(rawVarsV1, rawVarsV2, rawVarsV3)

### Set number of demographic variables included in dataframe (these won't be adjusted) ###
nDemoVars = 7

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$NAS201TRAN))

# Specify nas201tran (Age 20 AFQT as variable to regress out)
regVars = paste("scale(NAS201TRAN)", sep=" + ")

# Regress nas201tran out of dataset
nasAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, "nas", data)

# Save out dataset with Age 20 AFQT regressed out
write.csv(nasAdjRawScoresData, "/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1V2V3_CogData_NASAdj.csv",
          row.names=F)

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for nas201tran (Age 20 AFQT) and standardized.            #
#                                                                                   #
# Dataset with NAS201TRAN (age 20 AFQT) regressed out is standardized (z-scored)    #
# based on VETSA 2 means and sd.                                                    #
#-----------------------------------------------------------------------------------#

# Initialize dataframe to hold means and SDs
scaleValues = data.frame()

nasAdjZscoresData = nasAdjRawScoresData

# Scale VETSA 1 variables that have been adjusted for nas201tran
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset
for(i in rawVarsV1){
  varname = paste0(i, "_nas")
  zvarname = gsub("_nas","_znas",varname)
  nasAdjZscoresData[[zvarname]] = scale(nasAdjZscoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, nasAdjZscoresData[[zvarname]])
  nasAdjZscoresData[[varname]] = NULL
}

# Scale VETSA 2 variables that have been adjusted for nas201tran using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
for(i in rawVarsV2){
  varnameV2 = paste0(i, "_nas")
  zvarname = gsub("_nas","_znas",varnameV2)
  varnameV1 = gsub("_V2","",varnameV2)
  nasAdjZscoresData[[zvarname]] = scale(nasAdjZscoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  nasAdjZscoresData[[varnameV2]] = NULL
}

# Scale VETSA 3 variables that have been adjusted for nas201tran using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
for(i in rawVarsV3){
  varnameV3 = paste0(i, "_nas")
  zvarname = gsub("_nas","_znas",varnameV3)
  varnameV1 = gsub("_V3","",varnameV3)
  nasAdjZscoresData[[zvarname]] = scale(nasAdjZscoresData[[varnameV3]],
                                        center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                        scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  nasAdjZscoresData[[varnameV3]] = NULL
}

# Save out adjusted and z-scored dataset
write.csv(nasAdjZscoresData, 
          "/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1V2V3_CogData_NASAdj_Z.csv",
          row.names = FALSE)

# Save out means and standard deviations used to standardize scores
write.csv(scaleValues, "/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1_NASAdj_Means_SDs.csv",
          row.names = FALSE)

