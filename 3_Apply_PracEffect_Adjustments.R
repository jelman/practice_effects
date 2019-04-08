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

# Get date for filenames
dstamp = Sys.Date()

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


# Calculate standard errors for practice effects using bootstrap resampling
# Inputs:
# ---------
# unadj_df: dataframe with variables to be adjusted 
# pracEffects: dataframe with practice effect values. Row names should contain variable names to adjust
# idx: Indices of returnees to whose scores should be adjusted
# colSuffix: Column name suffix denoting correct timepoint to apply adjustment to
#
# Returns:
# ----------
# unadj_df_PEadj: dataset with practice effect corrected variables included
applyPEadjustment = function(unadj_df, pracEffects, idx, colSuffix){
  varNames = row.names(pracEffects)
  unadj_df_PEadj = unadj_df
  for (varName in varNames) {
    varName_t2 = paste0(varName, colSuffix)
    newVarName_t2 = paste0(varName_t2,"p")
    peVal = pracEffects[varName,"PracticeEffect"]
    unadj_df_PEadj[idx, newVarName_t2] = unadj_df_PEadj[idx, varName_t2] - peVal
  }
  unadj_df_PEadj
}


#############################################
#     Apply practice effect adjustments     # 
#############################################

## Apply practice effect adjustments ## 

# Load dataset to be adjusted: No age 20 afqt adjustment, raw score scale
unadj_df = read.csv("~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1V2V3_CogData_Unadj_2019-04-08.csv",
                    stringsAsFactors = F)
unadj_df_PEadj = unadj_df

#---------------------------------#
#     V1V2 & V1V2V3: t1 -> t2     #
#---------------------------------#
pracEffects_V1V2_V1V2V3_t1t2 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/results/PracEffects_NAS201TRAN_V1V2-V1V2V3-t1t2_2019-04-08.csv",
                       row.names=1, stringsAsFactors = F)
idxV1V2_V1V2V3 = which(unadj_df$VETSAGRP %in% c("V1V2", "V1V2V3"))
unadj_df_PEadj = applyPEadjustment(unadj_df_PEadj, pracEffects_V1V2_V1V2V3_t1t2, idxV1V2_V1V2V3, "_V2")


#---------------------------------#
#        V1V2V3: t2 -> t3         #
#---------------------------------#
pracEffects_V1V2V3_t2t3 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/results/PracEffects_NAS201TRAN_V1V2V3-t2t3_2019-04-08.csv",
                                        row.names=1, stringsAsFactors = F)
idxV1V2V3 = which(unadj_df$VETSAGRP %in% c("V1V2V3"))
unadj_df_PEadj = applyPEadjustment(unadj_df_PEadj, pracEffects_V1V2V3_t2t3, idxV1V2V3, "_V3")


#---------------------------------#
#        V2V3: t2 -> t3           #
#---------------------------------#
pracEffects_V2V3_t2t3 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/results/PracEffects_NAS201TRAN_V2V3-t2t3_2019-04-08.csv",
                                   row.names=1, stringsAsFactors = F)
idxV2V3 = which(unadj_df$VETSAGRP %in% c("V2V3"))
unadj_df_PEadj = applyPEadjustment(unadj_df_PEadj, pracEffects_V2V3_t2t3, idxV2V3, "_V3")


#---------------------------------#
#        V1V3: t1 -> t3           #
#---------------------------------#
pracEffects_V1V3_t1t3 = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/results/PracEffects_NAS201TRAN_V2V3-t2t3_2019-04-08.csv",
                                 row.names=1, stringsAsFactors = F)
idxV1V3 = which(unadj_df$VETSAGRP %in% c("V1V3"))
unadj_df_PEadj = applyPEadjustment(unadj_df_PEadj, pracEffects_V1V3_t1t3, idxV1V3, "_V3")


#---------------------------------#
#        V1neV3: t1 -> t2         #
#---------------------------------#
# V1ne subjects are treated like V1V2 subjects for the purposes of correction. Their t1 and t2 
# assessments took place at V2 and V3, respectively. 
idxV1neV3 = which(unadj_df$VETSAGRP %in% c("V1neV3"))
unadj_df_PEadj = applyPEadjustment(unadj_df_PEadj, pracEffects_V1V2_V1V2V3_t1t2, idxV1neV3, "_V3")


##########################################################
#     Replace invalid scores and update empty values     # 
##########################################################

# Replace invalid negative numbers with 0
negVars = names(unadj_df_PEadj)[grepl("TRAN", names(unadj_df_PEadj)) | grepl("DPRIME", names(unadj_df_PEadj))]
posVars = names(unadj_df_PEadj)[(!names(unadj_df_PEadj) %in% negVars) & (!sapply(unadj_df_PEadj, is.character))]  
unadj_df_PEadj = unadj_df_PEadj %>% mutate_at(.vars=posVars, .funs=zeroFloor)

# Replace trails times over limit with the max value allowed
trl240vars = names(unadj_df_PEadj)[grepl("TRL4T", names(unadj_df_PEadj))]
unadj_df_PEadj = unadj_df_PEadj %>% mutate_at(.vars=trl240vars, .funs=timeCeiling, maxt=240)
trl150vars = names(unadj_df_PEadj)[grepl("TRL[1235]T", names(unadj_df_PEadj))]
unadj_df_PEadj = unadj_df_PEadj %>% mutate_at(.vars=trl150vars, .funs=timeCeiling, maxt=150)

# Add in unadjusted score to PE adjusted variables for replacements because currently only data from 
# follow-up subjects exists in these columns.  
# e.g., Copy values from MR1COR_V3 into MR1COR_V3p for subjects who only came at V3.
v2Vars = names(unadj_df_PEadj)[grep("_V2$", names(unadj_df_PEadj))]
for(varName in v2Vars){
  varNamePE = paste0(varName, "p")
  unadj_df_PEadj[varNamePE] = ifelse(is.na(unadj_df_PEadj[[varNamePE]]), unadj_df_PEadj[[varName]], unadj_df_PEadj[[varNamePE]]) 
}
v3Vars = names(unadj_df_PEadj)[grep("_V3$", names(unadj_df_PEadj))]
for(varName in v3Vars){
  varNamePE = paste0(varName, "p")
  unadj_df_PEadj[varNamePE] = ifelse(is.na(unadj_df_PEadj[[varNamePE]]), unadj_df_PEadj[[varName]], unadj_df_PEadj[[varNamePE]]) 
}


# Save out dataset of scores not adjusted for AFQT, raw score scale, practice effect adjusted
outname = paste0('~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/VETSA 3/data/V1V2V3_CogData_PE_',dstamp,'.csv')
write.csv(unadj_df_PEadj, outname, row.names=FALSE)
