#########################################################################################
# This script adjusts cognitive test scores for practice effects. Scores have *not*     # 
# been adjusted for age 20 AFQT and remain on the raw test score scale. These are       #
# adjusted by practice effects calculated from scores that *have* been adjusted for     #
# AFQT, but remain on raw score scale. Can be used when age 20 AFQT is a predictor of   #
# interest.                                                                             #
#                                                                                       #
# Note: Only follow-up scores of returnee subjects are adjusted for practice            #
# effects.                                                                              #
#                                                                                       #
#########################################################################################

library(dplyr)

###############################################
#     Define variables names and functions    #
###############################################

# Function to replace negative numbers with zero
zeroFloor = function(x){
  x = (x + abs(x)) / 2
  x}

# Function to replace times over a given cutoff
timeCeiling = function(x, maxt){
  x[x>log(maxt) & !is.na(x)] = log(maxt)
  x
}


#############################################
#     Apply practice effect adjustments     # 
#############################################

## Apply practice effect adjustments ## 

# Load dataset to be adjusted: No age 20 afqt adjustment, raw score scale
unadj_df = read.csv("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1V2V3_CogData_Unadj.csv",
                    stringsAsFactors = F)

# Load practice effect values calculated based scores adjusted for AFQT but on raw score scale
pracEffects = read.csv("~/netshare/M/PSYCH/KREMEN/Practice Effect Cognition/results/PracEffectValues_NASAdj.csv",
                       row.names=1, stringsAsFactors = F)

# Get basenames of test scores to adjust
varNames = row.names(pracEffects)
unadj_df_PEadj = unadj_df
idxV1V2 = which(unadj_df_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2")
  newVarName_V2 = paste0(varName_V2,"p")
  peVal = pracEffects[varName,"PracticeEffect"]
  unadj_df_PEadj[, newVarName_V2] = unadj_df_PEadj[, varName_V2]
  unadj_df_PEadj[idxV1V2, newVarName_V2] = unadj_df_PEadj[idxV1V2, newVarName_V2] - peVal
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
