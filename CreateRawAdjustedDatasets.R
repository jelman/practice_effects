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


# Create vector of all variable names for which  practice effects are 
# calculated. These variables will be adjusted
zVarsV1 = c("zVisSpat","zMR1COR","zHFTOTCOR","zSTWKMem","zdsfraw","zdsbraw",
            "zlntot","zsspfraw","zsspbraw","zrsatottran","zEpsMem","zcvatot",
            "zcvsdfr","zcvldfr","zlmitot","zlmdtot","zvritot","zvrdtot",
            "zAbsReason","zMTXTRAN","zVerbFlu","zlfcor","zcfcor",
            "zExecTrailsSwitch","ztrl4adjtran","zProcSpeed","zstrwraw",
            "zstrcraw","ztrl2tran","ztrl3tran","zExecCategorySwitch",
            "zCSSACCADJ","zExecInhibit","zstrit","zafqtpcttran","zafqtvocpcttran",
            "zafqtarpcttran","zafqttlpcttran","zafqtbxpcttran")
zVarsV1
zVarsV2 = paste0(zVarsV1,"_v2")

rawVarsV1 = c("MR1COR","HFTOTCOR","MTXTRAN","dsfraw","dsbraw","lntot","sspfraw",
            "sspbraw","RSATOTTRAN","cvatot","CVSDFR","CVLDFR","lmitot","lmdtot",
            "vritot","vrdtot","LFCOR","CFCOR","strwraw","strcraw","TRL2TRAN","TRL3TRAN",
            "TRL4ADJTRAN","CSSACCADJ","strit","afqtpcttran","afqtvocpcttran",
            "afqtarpcttran","afqttlpcttran","afqtbxpcttran")
rawVarsV1
rawVarsV2 = paste0(rawVarsV1,"_v2")

#---------------------------#
# Create unadjusted dataset #
#---------------------------#

## Load demographic data ##
demoData = read.csv("K:/data/VETSA_demo_vars.csv", stringsAsFactors = F)

## Load VETSA 1 Cognitive Domain data ##
vetsa1CogDomains = read.csv("K:/Projects/Cognitive Domains/data/V1_CognitiveDomains_All.csv", stringsAsFactors = F)

## Load VETSA 2 Cognitive Domain data ##
vetsa2CogDomains = read.csv("K:/Projects/Cognitive Domains/data/V2_CognitiveDomains_All.csv", stringsAsFactors = F)

#--------------------------------------------------------------#
# Make sure variable names match up between V1 and V2 datasets #
#--------------------------------------------------------------#

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
#--------------------------------------------------------------#

## Load VETSA 1 AFQT data ##
vetsa1afqt = read.csv("K:/Projects/PracticeEffects/data/V1_AFQTscores_All.csv",
                      stringsAsFactors = F)
## Load VETSA 2 AFQT data ##
vetsa2afqt = read.csv("K:/Projects/PracticeEffects/data/V2_AFQTscores_All.csv",
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
write.csv(allData,"K:/Projects/PracticeEffects/data/PracEffectData_Unadj.csv", 
          row.names=F)

#-------------------------------------------------------#
# Create function to adjusted dataset by regressing out #
# specified variables                                   #
#-------------------------------------------------------#

adjustDataset = function(regVars,adjVars,nDemoVars=16,data){
  #######################################################################
  # Adjust dataset for specified set of variables.Regresses passed      #
  # variables from each measure using mixed effects models. Residuals   #
  # are scaled to 0 mean and unit standard deviation.                   #
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
    fmla = as.formula(paste0(x," ~ 0 + ",regVars," + (1|case)"))
    lmer(formula=fmla, data = data, na.action=na.exclude)
  })
  
  # storing residuals from each model into data frame
  for(v in 1:nVars){
    data[,tot+v] <- residuals(models[[v]], scale=TRUE)
  }
  
  #dataR is now your residualized parameters

  dataR <- data[,c(1:nDemoVars,(tot+1):(tot+nVars))]
  dataR
}



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
regVars = paste("nas201tran", sep=" + ")

# Regress nas201tran out of dataset
nasAdjZscoreData = adjustDataset(regVars, adjVars, nDemoVars, data)

# Save out adjusted dataset
write.csv(nasAdjZscoreData, 
          "K:/Projects/PracticeEffects/data/PracEffectData_nas201tran_ZAdj.csv",
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
regVars = paste("nas201tran", sep=" + ")

# Regress nas201tran out of dataset
nasAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, data)

## Create function to save mean and SD of all variables ##
addScaleVals = function(df,varname, x) {
  meanVal = attr(x, which="scaled:center")
  sdVal = attr(x, which="scaled:scale")
  rbind(df, data.frame(Variable=varname, Mean=meanVal, SD=sdVal))
}

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

write.csv(scaleValues, "K:/Projects/PracticeEffects/data/V1_nas201tranAdjustedRaw_Means_SDs.csv",
          row.names = FALSE)

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
nasAdjRawScoresData = nasAdjRawScoresData %>%
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

# Create VETSA 1 cognitive domain scores
nasAdjRawScoresData$zVisSpat_adj = rowMeans(nasAdjRawScoresData[c("zMR1COR_adj","zHFTOTCOR_adj")],
                                            na.rm=T)
nasAdjRawScoresData$zAbsReason_adj = nasAdjRawScoresData$zMTXTRAN_adj
nasAdjRawScoresData$zSTWKMem_adj = rowMeans(nasAdjRawScoresData[,c("zdsfraw_adj","zdsbraw_adj",
                                           "zlntot_adj","zsspfraw_adj","zsspbraw_adj",
                                           "zrsatottran_adj")], na.rm=T)
nasAdjRawScoresData$zEpsMem_adj = rowMeans(nasAdjRawScoresData[,c("zcvatot_adj","zcvsdfr_adj",
                                          "zcvldfr_adj","zlmitot_adj","zlmdtot_adj",
                                          "zvritot_adj","zvrdtot_adj")], na.rm=T)
nasAdjRawScoresData$zVerbFlu_adj = rowMeans(nasAdjRawScoresData[,c("zlfcor_adj","zcfcor_adj")], 
                                            na.rm=T)
nasAdjRawScoresData$zProcSpeed_adj = rowMeans(nasAdjRawScoresData[,c("zstrwraw_adj","zstrcraw_adj",
                                             "ztrl2tran_adj","ztrl3tran_adj")],na.rm=T)
nasAdjRawScoresData$zExecTrailsSwitch_adj = nasAdjRawScoresData$ztrl4adjtran_adj 
nasAdjRawScoresData$zExecCategorySwitch_adj = nasAdjRawScoresData$zCSSACCADJ_adj
nasAdjRawScoresData$zExecInhibit_adj = nasAdjRawScoresData$zstrit_adj

# Create VETSA 2 cognitive domain scores
nasAdjRawScoresData$zVisSpat_v2_adj = rowMeans(nasAdjRawScoresData[c("zMR1COR_v2_adj",
                                                                     "zHFTOTCOR_v2_adj")],na.rm=T)
nasAdjRawScoresData$zAbsReason_v2_adj = nasAdjRawScoresData$zMTXTRAN_v2_adj
nasAdjRawScoresData$zSTWKMem_v2_adj = rowMeans(nasAdjRawScoresData[,c("zdsfraw_v2_adj",
                                                                      "zdsbraw_v2_adj","zlntot_v2_adj",
                                                                   "zsspfraw_v2_adj","zsspbraw_v2_adj",
                                                                   "zrsatottran_v2_adj")], na.rm=T)
nasAdjRawScoresData$zEpsMem_v2_adj = rowMeans(nasAdjRawScoresData[,c("zcvatot_v2_adj","zcvsdfr_v2_adj",
                                                                  "zcvldfr_v2_adj","zlmitot_v2_adj",
                                                                  "zlmdtot_v2_adj","zvritot_v2_adj",
                                                                  "zvrdtot_v2_adj")], na.rm=T)
nasAdjRawScoresData$zVerbFlu_v2_adj = rowMeans(nasAdjRawScoresData[,c("zlfcor_v2_adj","zcfcor_v2_adj")],
                                                                    na.rm=T)
nasAdjRawScoresData$zProcSpeed_v2_adj = rowMeans(nasAdjRawScoresData[,c("zstrwraw_v2_adj",
                                                                        "zstrcraw_v2_adj",
                                                                     "ztrl2tran_v2_adj",
                                                                     "ztrl3tran_v2_adj")],na.rm=T)
nasAdjRawScoresData$zExecTrailsSwitch_v2_adj = nasAdjRawScoresData$ztrl4adjtran_v2_adj 
nasAdjRawScoresData$zExecCategorySwitch_v2_adj = nasAdjRawScoresData$zCSSACCADJ_v2_adj
nasAdjRawScoresData$zExecInhibit_v2_adj = nasAdjRawScoresData$zstrit_v2_adj

# Save out adjusted dataset
write.csv(nasAdjRawScoresData, 
          "K:/Projects/PracticeEffects/data/PracEffectData_nas201tran_RawAdj.csv",
          row.names = FALSE)
