######################################################################
# Script to create datasets for practice effect analyses.            #
# All datasets will include composite cognitive domain scores as     #
# well as individual tests comprising them. AFQT and subtests        #
# are also included.                                                 #
#                                                                    #
# In addition to unadjusted dataset, the script currently creates    #
# datasets adjusted for:                                             #
# nas201tran (Age 20 AFQT)                                           #
#                                                                    #
######################################################################

# Import libraries
library(dplyr)
library(lme4)

# Load raw test scores and demographics data
allData = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/V1V2_PracticeEffect_Raw.csv",
                   stringsAsFactors = FALSE)

# Convert all variable names to upper case
names(allData) = toupper(names(allData))

# Log transform timing data
timeVarsV1 = c("TRL1T","TRL2T","TRL3T","TRL4T","TRL5T","SRTLMEAN","SRTLSTD","SRTRMEAN",
                "SRTRSTD","SRTGMEAN","SRTGSTD","CHRTLMEAN","CHRTRMEAN","CHRTLSTD",
                "CHRTRSTD","CHRTGMEAN","CHRTGSTD")
timeVarsLogV1 = paste0(timeVarsV1, "LOG")
timeVarsV2 = paste0(timeVarsV1, "_V2")
timeVarsLogV2 = paste0(timeVarsLogV1, "_V2")


allData[timeVarsLogV1] = log(allData[timeVarsV1])                
allData = dplyr::select(allData, -one_of(timeVarsV1))
allData[timeVarsLogV2] = log(allData[timeVarsV2])                
allData = dplyr::select(allData, -one_of(timeVarsV2))

# Create list of raw variable names to adjust
rawVarsV1 = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
              "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
              "AFQTBXPCTTRAN","DSFRAW","DSBRAW","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
              "STRWRAW","STRCRAW","STRCWRAW","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","SRTLMEANLOG",
              "SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG","CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG",
              "CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG","RSATOT")
rawVarsV2 = paste0(rawVarsV1, "_V2")

# Create lists of z-scored variable names to create calculate practice effect with
zVarsV1 = paste0("z",rawVarsV1)
zVarsV2 = paste0("z",rawVarsV2)

# Print variable names and verify these are correct
rawVarsV1
rawVarsV2
zVarsV1
zVarsV2


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
# Residuals of V1 and V2 variables are then scaled (z-scored) by V1 mean and SD.    # 
# Intercept is not included to avoid mean centering.                                #
# Composite domain scores are created by averaging together these adjusted scores.  #
#-----------------------------------------------------------------------------------#

# Adjust raw scores from VETSA 1 and VETSA 2
adjVars = c(rawVarsV1, rawVarsV2)

# Set number of demographic variables included in dataframe (these won't be adjusted)
nDemoVars = 7

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$NAS201TRAN))

# Specify nas201tran (Age 20 AFQT as variable to regress out)
regVars = paste("scale(NAS201TRAN)", sep=" + ")

# Regress nas201tran out of dataset
nasAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Save out dataset with Age 20 AFQT regressed out
write.csv(nasAdjRawScoresData, "/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_Adj.csv",
          row.names=F)

# Initialize dataframe to hold means and SDs
scaleValues = data.frame()

nasAdjZscoresData = nasAdjRawScoresData

# Scale VETSA 1 variables that have been adjusted for nas201tran
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset
for(i in rawVarsV1){
  varname = paste0(i, "_adj")
  zvarname = paste0("z", varname)
  nasAdjZscoresData[[zvarname]] = scale(nasAdjZscoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, nasAdjZscoresData[[zvarname]])
  nasAdjZscoresData[[varname]] = NULL
}

# Scale VETSA 2 variables that have been adjusted for nas201tran using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
for(i in rawVarsV2){
  varnameV2 = paste0(i, "_adj")
  zvarname = paste0("z", varnameV2)
  varnameV1 = gsub("_V2","",varnameV2)
  nasAdjZscoresData[[zvarname]] = scale(nasAdjZscoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  nasAdjZscoresData[[varnameV2]] = NULL
}

# Save out adjusted and z-scored dataset
write.csv(nasAdjZscoresData, 
          "/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_Adj_Zscored.csv",
          row.names = FALSE)

# Save out means and standard deviations used to standardize scores
write.csv(scaleValues, "/home/jelman/netshare/K/Projects/PracticeEffects/data/V1_NAS201TRAN_Adj_Means_SDs.csv",
          row.names = FALSE)

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for TEDALL (Education)                                    #
#                                                                                   #
# Adjustment consists of regressing out nuisance variable from raw variables.       # 
# Residuals of V1 and V2 variables are then scaled (z-scored) by V1 mean and SD.    # 
# Intercept is not included to avoid mean centering.                                #
# Composite domain scores are created by averaging together these adjusted scores.  #
#-----------------------------------------------------------------------------------#

# Adjust raw scores from VETSA 1 and VETSA 2
adjVars = c(rawVarsV1, rawVarsV2)

# Set number of demographic variables included in dataframe (these won't be adjusted)
nDemoVars = 7

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$TEDALL))

# Specify TEDALL (Education as variable to regress out)
regVars = paste("scale(TEDALL)", sep=" + ")

# Regress TEDALL out of dataset
tedAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Save out dataset with Education regressed out
write.csv(tedAdjRawScoresData, "/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_TEDALL_Adj.csv",
          row.names=F)

# Initialize dataframe to hold means and SDs
scaleValues = data.frame()

tedAdjZScoresData = tedAdjRawScoresData

# Scale VETSA 1 variables that have been adjusted for TEDALL
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset
for(i in rawVarsV1){
  varname = paste0(i, "_adj")
  zvarname = paste0("z", varname)
  tedAdjZScoresData[[zvarname]] = scale(tedAdjZScoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, tedAdjZScoresData[[zvarname]])
  tedAdjZScoresData[[varname]] = NULL
}

# Scale VETSA 2 variables that have been adjusted for TEDALL using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
for(i in rawVarsV2){
  varnameV2 = paste0(i, "_adj")
  zvarname = paste0("z", varnameV2)
  varnameV1 = gsub("_V2","",varnameV2)
  tedAdjZScoresData[[zvarname]] = scale(tedAdjZScoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  tedAdjZScoresData[[varnameV2]] = NULL
}

# Save out adjusted and z-scored dataset
write.csv(tedAdjZScoresData, 
          "/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_TEDALL_Adj_Zscored.csv",
          row.names = FALSE)

# Save out means and standard deviations used to standardize scores
write.csv(scaleValues, "/home/jelman/netshare/K/Projects/PracticeEffects/data/V1_TEDALL_Adj_Means_SDs.csv",
          row.names = FALSE)
