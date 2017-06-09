#######################################################################################
# This script adjusts cognitive test scores for practice effects. Three datasets are  #
# adjusted:                                                                           #
#                                                                                     #
# 1.) Scores that have been *not* been adjusted for age 20 AFQT and remain on the     #
#     raw test score scale. These are adjusted by practice effects calculated from    #
#     scores that *have* been adjusted for AFQT, but remain on raw score scale.       #
#     Can be used when age 20 AFQT is a predictor of interest.                        #
#                                                                                     #
# 2.) Scores that have been adjusted for age 20 AFQT and z-scored based on VETSA1     #
#     means and standard deviations. These are adjusted by practice effects           #
#     calculated from scores that have been adjusted for AFQT and z-scored based      #
#     on VETSA1 means and standard deviations. These should not be used if age 20     #
#     is a predictor of interest.                                                     #
# 3.) Scores that have been adjusted for age 20 AFQT but remain on the raw score      #
#     scale. These are adjusted by practice effects calculated from scores that       #
#     *have* been adjusted for AFQT, but remain on raw score scale. These should not  #
#     be used if age 20 is a predictor of interest.                                   #
#                                                                                     #
# Note: Only time 2 scores of longitudinal (V1V2) subjects are adjusted               #
#                                                                                     #
#######################################################################################

library(dplyr)


###############################################
#     Define variables names and functions    #
###############################################

# Basenames of scores to adjust
varNames = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
             "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
             "AFQTBXPCTTRAN","DSFRAW","DSBRAW","DSFMAX","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
             "STRWRAW","STRCRAW","STRCWRAW","STRIT","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","SRTLMEANLOG",
             "SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG","CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG",
             "CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG","RSATOT","AXHITRATE","AXFARATE","AXMISSRATE","BXHITRATE","BXFARATE",
             "BXMISSRATE","CPTDPRIME")

# Function to replace negative numbers with zero
zeroFloor = function(x){
  x = (x + abs(x)) / 2
  x}

# Function to replace times over a given cutoff
timeCeiling = function(x, maxt){
  x[x>log(maxt) & !is.na(x)] = log(maxt)
  x
}
  


#-----------------------------------------------------------------#
#   1. Scores on raw score scale, not adjusted for age 20 AFQT    #
#-----------------------------------------------------------------#

# Load dataset to be adjusted: No age 20 afqt adjustment, raw score scale
unadj_df = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_Unadj.csv",
                        stringsAsFactors = F)

# Load practice effect values calculated based scores adjusted for AFQT but on raw score scale
pracEffects = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/results/PracEffectValues_NASAdj.csv",
                                row.names=1, stringsAsFactors = F)

## Apply practice effect adjustments ## 

unadj_df_PEadj = unadj_df
idxV1V2 = which(unadj_df_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2")
  newVarName_V2 = paste0(varName_V2,"p")
  peVal = pracEffects[varName,"PracticeEffect"]
  unadj_df_PEadj[, newVarName_V2] = unadj_df_PEadj[, varName_V2]
  unadj_df_PEadj[idxV1V2, newVarName_V2] = unadj_df_PEadj[idxV1V2, newVarName_V2] - peVal
  unadj_df_PEadj[[varName_V2]] = NULL
}

# Replace invalid negative numbers with 0
negVars = names(unadj_df_PEadj)[grepl("TRAN", names(unadj_df_PEadj)) | grepl("DPRIME", names(unadj_df_PEadj))]
posVars = names(unadj_df_PEadj)[(!names(unadj_df_PEadj) %in% negVars) & (!sapply(unadj_df_PEadj, is.character))]  
unadj_df_PEadj = unadj_df_PEadj %>% mutate_at(.cols=posVars, .funs=zeroFloor)

# Replace trails times over limit with the max value allowed
trl240vars = names(unadj_df_PEadj)[grepl("TRL4T", names(unadj_df_PEadj))]
unadj_df_PEadj = unadj_df_PEadj %>% mutate_at(.cols=trl240vars, .funs=timeCeiling, maxt=240)
trl150vars = names(unadj_df_PEadj)[grepl("TRL[1235]T", names(unadj_df_PEadj))]
unadj_df_PEadj = unadj_df_PEadj %>% mutate_at(.cols=trl150vars, .funs=timeCeiling, maxt=150)

# Save out dataset of scores not adjusted for AFQT, raw score scale, practice effect adjusted
write.csv(unadj_df_PEadj, '~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_PE.csv',
          row.names=FALSE)

#-----------------------------------------------------------------------#
#   2. Scores on z-score scale that have been adjusted for age 20 AFQT  #
#-----------------------------------------------------------------------#

# Load dataset to be adjusted: Age 20 afqt adjusted, z-score scale
nas201adjzscore_df = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_NASAdj_Z.csv",
                        stringsAsFactors = F)

# Load practice effect values calculated based scores adjusted for AFQT and z-scored based on VETSA 1
pracEffects_Zscored = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/results/PracEffectValues_NASAdj_Z.csv",
                               row.names=1, stringsAsFactors = F)

## Apply practice effect adjustments ## 

nas201adjzscore_PEadj = nas201adjzscore_df
idxV1V2 = which(nas201adjzscore_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2_znas")
  newVarName_V2 = paste0(varName_V2,"p")
  peVal = pracEffects_Zscored[varName,"PracticeEffect"]
  nas201adjzscore_PEadj[, newVarName_V2] = nas201adjzscore_PEadj[, varName_V2]
  nas201adjzscore_PEadj[idxV1V2, newVarName_V2] = nas201adjzscore_PEadj[idxV1V2, newVarName_V2] - peVal
  nas201adjzscore_PEadj[[varName_V2]] = NULL
}

# Save out dataset of scores adjusted for AFQT, z-scored to VETSA 1, practice effect adjusted
write.csv(nas201adjzscore_PEadj, '~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_NASAdj_Z_PE.csv',
          row.names=FALSE)


#--------------------------------------------------------------------------#
#   3. Scores on  raw score scale that have been adjusted for age 20 AFQT  #
#--------------------------------------------------------------------------#

# Load dataset to be adjusted: Age 20 afqt adjusted, raw score scale
nas201adj_df = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_NASAdj.csv",
                              stringsAsFactors = F)

# Load practice effect values calculated based scores adjusted for AFQT but on raw score scale
pracEffects = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/results/PracEffectValues_NASAdj.csv",
                               row.names=1, stringsAsFactors = F)

## Apply practice effect adjustments ## 

nas201adj_PEadj = nas201adj_df
idxV1V2 = which(nas201adj_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2_nas")
  newVarName_V2 = paste0(varName_V2,"p")
  peVal = pracEffects[varName,"PracticeEffect"]
  nas201adj_PEadj[, newVarName_V2] = nas201adj_PEadj[, varName_V2]
  nas201adj_PEadj[idxV1V2, newVarName_V2] = nas201adj_PEadj[idxV1V2, newVarName_V2] - peVal
  nas201adj_PEadj[[varName_V2]] = NULL
}


# Replace invalid negative numbers with 0
negVars = names(nas201adj_PEadj)[grepl("TRAN", names(nas201adj_PEadj)) | grepl("DPRIME", names(nas201adj_PEadj))]
posVars = names(nas201adj_PEadj)[(!names(nas201adj_PEadj) %in% negVars) & (!sapply(nas201adj_PEadj, is.character))]  
nas201adj_PEadj = nas201adj_PEadj %>% mutate_at(.cols=posVars, .funs=zeroFloor)

# Replace trails times over limit with the max value allowed
trl240vars = names(nas201adj_PEadj)[grepl("TRL4T", names(nas201adj_PEadj))]
nas201adj_PEadj = nas201adj_PEadj %>% mutate_at(.cols=trl240vars, .funs=timeCeiling, maxt=240)
trl150vars = names(nas201adj_PEadj)[grepl("TRL[1235]T", names(nas201adj_PEadj))]
nas201adj_PEadj = nas201adj_PEadj %>% mutate_at(.cols=trl150vars, .funs=timeCeiling, maxt=150)

# Save out dataset of scores adjusted for AFQT, z-scored to VETSA 1, practice effect adjusted
write.csv(nas201adj_PEadj, '~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/data/V1V2_CogData_NASAdj_PE.csv',
          row.names=FALSE)
