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
allData = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/Practice Effect Cognition/data/V1V2_PracticeEffect_Raw.csv",
                   stringsAsFactors = FALSE)

# Convert all variable names to upper case
names(allData) = toupper(names(allData))

# Select subjects from groups of interest
allData = allData %>%
  filter(VETSAGRP=="V1V2" | VETSAGRP=="V1" | VETSAGRP=="V2AR")


# Create list of raw variable names to adjust
rawVarsV1 = c("MR1COR","TRL1T","TRL2T","TRL3T","TRL4T","TRL5T","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR","AFQTPCT",
              "AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
              "AFQTBXPCTTRAN","DSFRAW","DSBRAW","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
              "STRWRAW","STRCRAW","STRCWRAW","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","SRTLMEAN",
              "SRTLSTD","SRTRMEAN","SRTRSTD","SRTGMEN","SRTGSTD","CHRTLMEAN","CHRTRMEAN","CHRTLSTD","CHRTRSTD","CHRTGMEAN",
              "CHRTGSTD","RSATOT")
rawVarsV2 = paste0(rawVarsV1, "_V2")

# Create lists of z-scored variable names to create calculate practice effect with
zVarsV1 = paste0("z",rawVarsV1)
zVarsV2 = paste0("z",rawVarsV2)

# Print variable names and verify these are correct
rawVarsV1
rawVarsV2
zVarsV1
zVarsV2

#---------------------------#
# Create unadjusted dataset #
#---------------------------#

## Load demographic data ##
demoData = read.csv("/home/jelman/netshare/K/data/VETSA_demo_vars.csv", stringsAsFactors = F)

## Load VETSA 1 Cognitive Domain data ##
vetsa1CogDomains = read.csv("/home/jelman/netshare/K/Projects/Cognitive Domains/data/V1_CognitiveDomains_All.csv", stringsAsFactors = F)

## Load VETSA 2 Cognitive Domain data ##
vetsa2CogDomains = read.csv("/home/jelman/netshare/K/Projects/Cognitive Domains/data/V2_CognitiveDomains_All.csv", stringsAsFactors = F)


## Make sure variable names match up between V1 and V2 datasets ##
# Make sure suffix has the same case for all variables
names(vetsa2CogDomains) = gsub("_V2","_v2",names(vetsa2CogDomains))
# Rename zrsatotrevtran variable in V1 to match variable name in V2
vetsa1CogDomains = dplyr::rename(vetsa1CogDomains, zrsatottran=zrsatotrevtran)
# Rename RSATOTrevtran variable in V1 to match variable name in V2
vetsa1CogDomains = dplyr::rename(vetsa1CogDomains, RSATOTTRAN=RSATOTrevtran)
# Rename strit variable in V1 to match variable name in V2
vetsa1CogDomains = dplyr::rename(vetsa1CogDomains, strit=STRIT)
# Remove zVerbal and zvoctran. These were not collected at time 2
vetsa1CogDomains = vetsa1CogDomains %>% dplyr::select(-one_of(c("zVerbal","zvoctran")))

## Load VETSA 1 AFQT data ##
vetsa1afqt = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/V1_AFQTscores_All.csv",
                      stringsAsFactors = F)
## Load VETSA 2 AFQT data ##
vetsa2afqt = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/V2_AFQTscores_All.csv",
                      stringsAsFactors = F)

# Join demographic data with VETSA1 & VETSA2 datasets
allData = demoData %>%
  left_join(vetsa1CogDomains, by="vetsaid") %>% 
  left_join(vetsa2CogDomains, by="vetsaid") %>%
  left_join(vetsa1afqt, by="vetsaid") %>%
  left_join(vetsa2afqt, by="vetsaid")

# Select subjects from groups of interest
allData = allData %>%
  filter(VETSAGRP=="V1V2" | VETSAGRP=="V1" | VETSAGRP=="V2AR")

# Write out unadjusted dataset
write.csv(allData,"/home/jelman/netshare/K/Projects/PracticeEffects/data/PracEffectData_Unadj.csv", 
          row.names=F)

#----------------------------------------------------------------------------#
#                     Define functions                                       #
#----------------------------------------------------------------------------#

adjustDataset = function(regVars,adjVars,nDemoVars=16,data){
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
    fmla = as.formula(paste0(x," ~ ",regVars," + (1|case)"))
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


renameVars = function(df){
  ###############################################################
  # Rename variables to be consistent with other unadjusted and #
  # unadjusted datasets.                                        #
  # Input:                                                      #
  # df = Dataframe containing variables to be renamed           #
  ###############################################################
  dfAdjusted = df %>%
    dplyr::rename(zrsatottran_adj=zRSATOTTRAN_adj,
                  zrsatottran_v2_adj=zRSATOTTRAN_v2_adj,
                  zcvsdfr_adj=zCVSDFR_adj,
                  zcvsdfr_v2_adj=zCVSDFR_v2_adj,
                  zcvldfr_adj=zCVLDFR_adj,
                  zcvldfr_v2_adj=zCVLDFR_v2_adj,
                  zlfcor_adj=zLFCOR_adj,
                  zlfcor_v2_adj=zLFCOR_v2_adj,
                  zcfcor_adj=zCFCOR_adj,
                  zcfcor_v2_adj=zCFCOR_v2_adj,
                  ztrl2tran_adj=zTRL2TRAN_adj,
                  ztrl2tran_v2_adj=zTRL2TRAN_v2_adj,
                  ztrl3tran_adj=zTRL3TRAN_adj,
                  ztrl3tran_v2_adj=zTRL3TRAN_v2_adj,
                  ztrl4adjtran_adj=zTRL4ADJTRAN_adj,
                  ztrl4adjtran_v2_adj=zTRL4ADJTRAN_v2_adj)
  dfAdjusted
}


createV1CogDomains = function(df){
  #########################################################################
  # Creates cognitive domains for V1 data. Composite scores are means of  #
  # tests comprising each domain. Note: mean of z-scores does not         #
  # produce a z-score (SD will likely be <1).                             #
  # Input:                                                                #
  # df = Dataframe containing variables to be renamed                     #
  #########################################################################
  
  df$VisSpat_adj = rowMeans(df[c("zMR1COR_adj","zHFTOTCOR_adj")],na.rm=T)
  df$AbsReason_adj = df$zMTXTRAN_adj
  df$STWKMem_adj = rowMeans(df[,c("zdsfraw_adj","zdsbraw_adj","zlntot_adj",
                                   "zsspfraw_adj","zsspbraw_adj","zrsatottran_adj")], 
                             na.rm=T)
  df$EpsMem_adj = rowMeans(df[,c("zcvatot_adj","zcvsdfr_adj","zcvldfr_adj",
                                  "zlmitot_adj","zlmdtot_adj","zvritot_adj","zvrdtot_adj")], 
                            na.rm=T)
  df$VerbFlu_adj = rowMeans(df[,c("zlfcor_adj","zcfcor_adj")], na.rm=T)
  df$ProcSpeed_adj = rowMeans(df[,c("zstrwraw_adj","zstrcraw_adj","ztrl2tran_adj",
                                     "ztrl3tran_adj")],na.rm=T)
  df$ExecTrailsSwitch_adj = df$ztrl4adjtran_adj 
  df$ExecCategorySwitch_adj = df$zCSSACCADJ_adj
  df$ExecInhibit_adj = df$zstrit_adj
  df
}


createV2CogDomains = function(df){
  #########################################################################
  # Creates cognitive domains for V2 data. Composite scores are means of  #
  # tests comprising each domain. Note: mean of z-scores does not         #
  # produce a z-score (SD will likely be <1).                             #
  # Input:                                                                #
  # df = Dataframe containing variables to be renamed                     #
  #########################################################################
  
  df$VisSpat_v2_adj = rowMeans(df[c("zMR1COR_v2_adj","zHFTOTCOR_v2_adj")],na.rm=T)
  df$AbsReason_v2_adj = df$zMTXTRAN_v2_adj
  df$STWKMem_v2_adj = rowMeans(df[,c("zdsfraw_v2_adj", "zdsbraw_v2_adj",
                                      "zlntot_v2_adj","zsspfraw_v2_adj",
                                      "zsspbraw_v2_adj","zrsatottran_v2_adj")], na.rm=T)
  df$EpsMem_v2_adj = rowMeans(df[,c("zcvatot_v2_adj","zcvsdfr_v2_adj",
                                     "zcvldfr_v2_adj","zlmitot_v2_adj",
                                     "zlmdtot_v2_adj","zvritot_v2_adj","zvrdtot_v2_adj")], 
                               na.rm=T)
  df$VerbFlu_v2_adj = rowMeans(df[,c("zlfcor_v2_adj","zcfcor_v2_adj")],na.rm=T)
  df$ProcSpeed_v2_adj = rowMeans(df[,c("zstrwraw_v2_adj","zstrcraw_v2_adj",
                                        "ztrl2tran_v2_adj","ztrl3tran_v2_adj")],na.rm=T)
  df$ExecTrailsSwitch_v2_adj = df$ztrl4adjtran_v2_adj 
  df$ExecCategorySwitch_v2_adj = df$zCSSACCADJ_v2_adj
  df$ExecInhibit_v2_adj = df$zstrit_v2_adj
  df
}
  

########################################
### Begin creating adjusted datasets ###
########################################

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for nas201tran (Age 20 AFQT)                              #
#                                                                                   #
# Adjustment consists of regressing out nuisance variable from z-scored variables.  #
# Intercept is not included to avoid mean centering.                                #
#-----------------------------------------------------------------------------------#

# Create list of variables to adjust
adjVars = c(zVarsV1, zVarsV2)

# Set number of demographic variables included in dataframe (these won't be adjusted)
nDemoVars = 16

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$nas201tran))

# Specify nas201tran (Age 20 AFQT as variable to regress out)
regVars = paste("scale(nas201tran)", sep=" + ")

# Regress nas201tran out of dataset
nasAdjZscoreData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Save out adjusted dataset
write.csv(nasAdjZscoreData, 
          "/home/jelman/netshare/K/Projects/PracticeEffects/data/PracEffectData_nas201tran_ZAdj.csv",
          row.names = FALSE)

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
nDemoVars = 16

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$nas201tran))

# Specify nas201tran (Age 20 AFQT as variable to regress out)
regVars = paste("scale(nas201tran)", sep=" + ")

# Regress nas201tran out of dataset
nasAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Initialize dataframe to hold means and SDs
scaleValues = data.frame()

# Scale VETSA 1 variables that have been adjusted for nas201tran
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset
for(i in rawVarsV1){
  varname = paste0(i, "_adj")
  zvarname = paste0("z", varname)
  nasAdjRawScoresData[[zvarname]] = scale(nasAdjRawScoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, nasAdjRawScoresData[[zvarname]])
  nasAdjRawScoresData[[varname]] = NULL
}

# Scale VETSA 2 variables that have been adjusted for nas201tran using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
for(i in rawVarsV2){
  varnameV2 = paste0(i, "_adj")
  zvarname = paste0("z", varnameV2)
  varnameV1 = gsub("_v2","",varnameV2)
  nasAdjRawScoresData[[zvarname]] = scale(nasAdjRawScoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  nasAdjRawScoresData[[varnameV2]] = NULL
}

# Rename variables to be consistent with other unadjusted and unadjusted datasets
nasAdjRawScoresData = renameVars(nasAdjRawScoresData)

# Create VETSA 1 cognitive domain scores
nasAdjRawScoresData = createV1CogDomains(nasAdjRawScoresData)

# Create VETSA 2 cognitive domain scores
nasAdjRawScoresData = createV2CogDomains(nasAdjRawScoresData)

# Scale VETSA 1 cognitive domain variables that have been adjusted for nas201tran
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset. 
# This is necessary to make composite scores actual z-scores
for(i in rawCogDomainsV1){
  varname = paste0(i, "_adj")
  zvarname = paste0("z", varname)
  nasAdjRawScoresData[[zvarname]] = scale(nasAdjRawScoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, nasAdjRawScoresData[[zvarname]])
  nasAdjRawScoresData[[varname]] = NULL
}

# Scale VETSA 2 cognitive domain variables that have been adjusted for nas201tran using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
# This is necessary to make composite scores actual z-scores
for(i in rawCogDomainsV2){
  varnameV2 = paste0(i, "_adj")
  zvarname = paste0("z", varnameV2)
  varnameV1 = gsub("_v2","",varnameV2)
  nasAdjRawScoresData[[zvarname]] = scale(nasAdjRawScoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  nasAdjRawScoresData[[varnameV2]] = NULL
}

# Save out adjusted dataset
write.csv(nasAdjRawScoresData, 
          "/home/jelman/netshare/K/Projects/PracticeEffects/data/PracEffectData_nas201tran_RawAdj.csv",
          row.names = FALSE)

# Save out dataset
write.csv(scaleValues, "/home/jelman/netshare/K/Projects/PracticeEffects/data/V1_nas201tranAdjustedRaw_Means_SDs.csv",
          row.names = FALSE)

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for TEDALL (Education)                              #
#                                                                                   #
# Adjustment consists of regressing out nuisance variable from raw variables.       # 
# Residuals of V1 and V2 variables are then scaled (z-scored) by V1 mean and SD.    # 
# Intercept is not included to avoid mean centering.                                #
# Composite domain scores are created by averaging together these adjusted scores.  #
#-----------------------------------------------------------------------------------#

# Adjust raw scores from VETSA 1 and VETSA 2
adjVars = c(rawVarsV1, rawVarsV2)

# Set number of demographic variables included in dataframe (these won't be adjusted)
nDemoVars = 16

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$TEDALL))

# Specify TEDALL (Education as variable to regress out)
regVars = paste("scale(TEDALL)", sep=" + ")

# Regress TEDALL out of dataset
tedAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Initialize dataframe to hold means and SDs
scaleValues = data.frame()

# Scale VETSA 1 variables that have been adjusted for TEDALL
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset
for(i in rawVarsV1){
  varname = paste0(i, "_adj")
  zvarname = paste0("z", varname)
  tedAdjRawScoresData[[zvarname]] = scale(tedAdjRawScoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, tedAdjRawScoresData[[zvarname]])
  tedAdjRawScoresData[[varname]] = NULL
}

# Scale VETSA 2 variables that have been adjusted for TEDALL using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
for(i in rawVarsV2){
  varnameV2 = paste0(i, "_adj")
  zvarname = paste0("z", varnameV2)
  varnameV1 = gsub("_v2","",varnameV2)
  tedAdjRawScoresData[[zvarname]] = scale(tedAdjRawScoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  tedAdjRawScoresData[[varnameV2]] = NULL
}

# Rename variables to be consistent with other unadjusted and unadjusted datasets
tedAdjRawScoresData = renameVars(tedAdjRawScoresData)

# Create VETSA 1 cognitive domain scores
tedAdjRawScoresData = createV1CogDomains(tedAdjRawScoresData)

# Create VETSA 2 cognitive domain scores
tedAdjRawScoresData = createV2CogDomains(tedAdjRawScoresData)

# Scale VETSA 1 cognitive domain variables that have been adjusted for TEDALL
# Adds mean and SD to dataframe and deletes adjusted raw variables from dataset. 
# This is necessary to make composite scores actual z-scores
for(i in rawCogDomainsV1){
  varname = paste0(i, "_adj")
  zvarname = paste0("z", varname)
  tedAdjRawScoresData[[zvarname]] = scale(tedAdjRawScoresData[[varname]])
  scaleValues = addScaleVals(scaleValues, varname, tedAdjRawScoresData[[zvarname]])
  tedAdjRawScoresData[[varname]] = NULL
}

# Scale VETSA 2 cognitive domain variables that have been adjusted for TEDALL using VETSA 1 mean and SD
# Delete adjusted raw variable from dataset
# This is necessary to make composite scores actual z-scores
for(i in rawCogDomainsV2){
  varnameV2 = paste0(i, "_adj")
  zvarname = paste0("z", varnameV2)
  varnameV1 = gsub("_v2","",varnameV2)
  tedAdjRawScoresData[[zvarname]] = scale(tedAdjRawScoresData[[varnameV2]],
                                          center=scaleValues$Mean[scaleValues$Variable==varnameV1],
                                          scale=scaleValues$SD[scaleValues$Variable==varnameV1])
  tedAdjRawScoresData[[varnameV2]] = NULL
}


# Save out adjusted dataset
write.csv(tedAdjRawScoresData, 
          "/home/jelman/netshare/K/Projects/PracticeEffects/data/PracEffectData_TEDALL_RawAdj.csv",
          row.names = FALSE)

# Save out dataset
write.csv(scaleValues, "/home/jelman/netshare/K/Projects/PracticeEffects/data/V1_TEDALLAdjustedRaw_Means_SDs.csv",
          row.names = FALSE)
