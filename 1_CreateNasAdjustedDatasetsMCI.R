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
#   dataV3: VETSA 3 dataset including variables needed for MCI Dx    #
#   dataInfo: Dataset with subject info needed to adjustment. This   #
#         includes: vetsaid, case, NAS201TRAN, VETSAGRP              #
#   dataExclude: Dataset specifying subjects who should be excluded  #
#         from MCI Dx (e.g., stroke or large tumor). This            #
#         should include vetsaid and a variable called               #
#         AnyMCIExclude with values 0=include and 1=exclude          #
#                                                                    #
# Output:                                                            #
#----------------                                                    #
#   V1V2V3_CogData_Unadj.csv: Unadjusted VETSA 1/2/3 scores combined #
#         into one dataset. Practice effect adjustments will be      #
#         applied to this dataset after they are calculated. This    #
#         will allow for analyses where age 20 AFQT is a variables   #
#         of interest. Includes V1new subjects so that their scores  #
#         can be corrected as well.                                  # 
#   V1V2V3_CogData_NASAdj.csv: VETSA 1/2/3 scores on the raw score   #
#         scale that have been adjusted for age 20 AFQT. This        #
#         dataset will be used to calculate practice effects. It     #
#         does not include V1new subjects. All adjusted scores will  #
#         have the suffix of "_nas"                                  #
#   V1V2V3_CogData_NASAdj_Z.csv: VETSA 1/2/3 scores z-scored based   #
#         V1 means and SD that have been adjusted for age 20 AFQT.   #
#         This dataset can be used to calculate practice effects in  #
#         z-score units in the future. It does not include V1new     #
#         subjects. All adjusted scores will have the suffix of      #
#         "_znas"                                                    #  
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

# Get date for filenames
dstamp = Sys.Date()

#---------------------------------------#
#           LOAD AND MERGE DATA         #
#---------------------------------------#

### Define variable names of interest and load data ###
# Create list of raw variable names to adjust
rawVarsV1 = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
              "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
              "AFQTBXPCTTRAN","DSFRAW","DSBRAW","DSFMAX","DSTOT","SSPFRAW","SSPBRAW","SSPTOTP","LNTOT","LM1A","LM1B","LM2A","LM2B",
              "LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR","STRWRAW","STRCRAW","STRCWRAW","STRIT","LFFCOR","LFACOR","LFSCOR","LFCOR",
              "CFANCOR","CFBNCOR","CFCOR","CSCOR","RSATOT","SRTLMEANLOG","SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG",
              "CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG","CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG","AXHITRATE","AXFARATE","AXMISSRATE",
              "BXHITRATE","BXFARATE","BXMISSRATE")
rawVarsV2 = paste0(rawVarsV1, "_V2")
rawVarsV3 = paste0(rawVarsV1, "_V3")
# Remove AX-CPT variables from V3 data
rawVarsV3 = rawVarsV3[! rawVarsV3 %in% c("AXHITRATE_V3","AXFARATE_V3","AXMISSRATE_V3","BXHITRATE_V3","BXFARATE_V3","BXMISSRATE_V3")]
# Print variable names and verify these are correct
rawVarsV1
rawVarsV2
rawVarsV3

### Load raw test scores and demographics data. Rename all columns to upper

# VETSA 1
dataV1 = read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 1 Aging/vetsa1merged_22dec2016_nomiss.sas7bdat")
names(dataV1) = toupper(names(dataV1))
# Merge in Ax-CPT data
axcpt_v1 = read.csv("~/netshare/M/VETSA DATA FILES_852014/AX-CPT/data/AX-CPT_V1.csv")
names(axcpt_v1) = toupper(names(axcpt_v1))  
dataV1 = dataV1 %>% left_join(axcpt_v1[c("VETSAID","AXHITRATE","AXFARATE","AXMISSRATE","BXHITRATE","BXFARATE","BXMISSRATE")], by="VETSAID")

# VETSA 2
dataV2 = read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA 2 Aging/vetsa2merged_23dec2016_nomiss.sas7bdat")
names(dataV2) = toupper(names(dataV2))
rt_dataV2 = read.csv("~/netshare/M/VETSA DATA FILES_852014/Reaction Time Data V2/vetsa2_reactiontime_merge.csv")
names(rt_dataV2) = toupper(names(rt_dataV2))
dataV2 = dataV2 %>% left_join(rt_dataV2, by="VETSAID")
# Merge in Ax-CPT data
axcpt_v2 = read.csv("~/netshare/M/VETSA DATA FILES_852014/AX-CPT/data/AX-CPT_V2.csv")
names(axcpt_v2) = toupper(names(axcpt_v2))  
names(axcpt_v2)[2:length(names(axcpt_v2))] = paste0(names(axcpt_v2)[2:length(names(axcpt_v2))], "_V2")
dataV2 = dataV2 %>% left_join(axcpt_v2[c("VETSAID","AXHITRATE_V2","AXFARATE_V2","AXMISSRATE_V2","BXHITRATE_V2","BXFARATE_V2","BXMISSRATE_V2")], by="VETSAID")
  
# VETSA 3
dataV3 = read_sas("~/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/VETSA3 interim datasets/vetsa3_master_db_20200114.sas7bdat")
names(dataV3) = toupper(names(dataV3))
# Merge in RT data
rt_dataV3 = read.csv("/home/jelman/netshare/M/VETSA DATA FILES_852014/Reaction Time Data V3/vetsa3_rt_merge.csv")
names(rt_dataV3) = toupper(names(rt_dataV3))
dataV3 = dataV3 %>% left_join(rt_dataV3, by="VETSAID")
# Create RSATOT variable
dataV3 = dataV3 %>% 
  mutate(RSATOT_V3 = RSA21_V3+RSA22_V3+RSA23_V3+RSA24_V3+RSA25_V3+RSA31_V3+RSA32_V3+RSA33_V3+RSA34_V3+RSA35_V3+RSA41_V3+RSA42_V3+RSA43_V3+RSA44_V3+RSA45_V3)

# Get VETSA group and age 20 AFQT data
admin = read_sas("/home/jelman/netshare/M/NAS VETSA MASTER DATAFILES/Master Data/Admin/vetsa_admin_file_20191210.sas7bdat")
afqt = read.csv("/home/jelman/netshare/M/NAS VETSA MASTER DATAFILES/Other cognitive measures/AFQT--age 20 cannot be distributed outside VETSA/AllVETSA_NAS201.csv")

dataInfo = admin %>% full_join(afqt, by="vetsaid")
names(dataInfo) = toupper(names(dataInfo))
dataInfo = dataInfo %>% select(VETSAID, CASE, VETSAGRP=VGRP_PROCVAR, NAS201=AFQT20, NAS201TRAN) %>%
  mutate(VETSAGRP = gsub("AR|M","",VETSAGRP))

### Log transform timing data ### 
# Get names of variables to transform
timeVarsV1 = c("TRL1T","TRL2T","TRL3T","TRL4T","TRL5T","SRTLMEAN","SRTLSTD","SRTRMEAN",
               "SRTRSTD","SRTGMEAN","SRTGSTD","CHRTLMEAN","CHRTRMEAN","CHRTLSTD",
               "CHRTRSTD","CHRTGMEAN","CHRTGSTD")
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
dataV1 = dataV1 %>% select(VETSAID, all_of(rawVarsV1))
dataV2 = dataV2 %>% select(VETSAID, all_of(rawVarsV2))
dataV3 = dataV3 %>% select(VETSAID, all_of(rawVarsV3))

### Merge all data and apply excludes ###
# Join all data
allData = dataV1 %>% full_join(dataV2, by="VETSAID")
allData = allData %>% full_join(dataV3, by="VETSAID")
allData = dataInfo %>%  
  dplyr::select(VETSAID, CASE, NAS201TRAN, VETSAGRP) %>%
  right_join(allData, by="VETSAID") 


### Save out unadjusted scores on raw score scale ###
outname = paste0("~/netshare/M/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/data/V1V2V3_CogData_Unadj_",dstamp,".csv")
write.csv(allData, outname, row.names = FALSE)

# Exclude indidivuals missing age 20 AFQT data
allData = allData %>%
  filter(!is.na(NAS201TRAN)) 

outname = paste0("~/netshare/M/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/data/intermediate_files/V1V2V3_CogData_NoMissingNAS201TRAN_Unadj_",dstamp,".csv")
write.csv(allData, outname, row.names = FALSE)

#----------------------------------------------------------------------------#
#                     Define functions                                       #
#----------------------------------------------------------------------------#

adjustDataset = function(regVars,adjVars,nDemoVars=4,suffix="adj",data){
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
nDemoVars = 4

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na(allData$NAS201TRAN))

# Remove V1 new subjects before regressing out age 20 AFQT
data = subset(data, !grepl("V1NE", data$VETSAGRP))

# Specify nas201tran (Age 20 AFQT as variable to regress out)
regVars = paste("scale(NAS201TRAN)", sep=" + ")

# Regress nas201tran out of dataset
nasAdjRawScoresData = adjustDataset(regVars, adjVars, nDemoVars, "nas", data)

# Save out dataset with Age 20 AFQT regressed out
outname = paste0("/home/jelman/netshare/M/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/data/intermediate_files/V1V2V3_CogData_NASAdj_",dstamp,".csv")
write.csv(nasAdjRawScoresData, outname, row.names=F)

#-----------------------------------------------------------------------------------#
# Create dataset adjusted for nas201tran (Age 20 AFQT) and standardized.            #
#                                                                                   #
# Dataset with NAS201TRAN (age 20 AFQT) regressed out is standardized (z-scored)    #
# based on VETSA 1 means and sd.                                                    #
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
outname = paste0("/home/jelman/netshare/M/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/data/intermediate_files/V1V2V3_CogData_NASAdj_Z_",dstamp,".csv")
write.csv(nasAdjZscoresData, outname, row.names = FALSE)

# Save out means and standard deviations used to standardize scores
outname = paste0("/home/jelman/netshare/M/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/data/intermediate_files/V1_NASAdj_Means_SDs_",dstamp,".csv")
write.csv(scaleValues, outname, row.names = FALSE)
