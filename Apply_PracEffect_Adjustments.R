#######################################################################################
# This script adjusts cognitive test scores for practice effects. Two datasets are    #
# adjusted:                                                                           #
#                                                                                     #
# 1.) Scores on the raw test score scale that are not adjusted for age 20 AFQT.       #
#     These are adjusted by practice effects calculated from scores that *have*       #
#     been adjusted for AFQT, but remain on raw score scale. The z-score units        #
#     will therefore not equal exactly the SD of scores unadjusted for age 20 AFQT.   #
#                                                                                     #
# 2.) Scores that have been adjusted for age 20 AFQT and z-scored based on VETSA1     #
#     means and standard deviations. These are adjusted by practice effects           #
#     calculated from scores that have been adjusted for AFQT and z-scored based      #
#     on VETSA1 means and standard deviations.                                        #
#                                                                                     #
# Note: Only time 2 scores of longitudinal (V1V2) subjects are adjusted               #
#                                                                                     #
#######################################################################################


## Load datasets to be adjusted ##

# Scores that have not been adjusted for AFQT and are on raw test score scale.
rawscores_df = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/V1V2_PracticeEffect_Raw.csv",
                        stringsAsFactors = F)
# Scores that have been adjusted for AFQT and z-scored based on VETSA 1 means and SD's
nas201adj_df = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_Adj_Zscored.csv",
                        stringsAsFactors = F)

## Load calculated practice effects values to adjust by ##

# Values that have been calculated based on scores adjusted for AFQT but on raw test score scale.
pracEffects_RawScale = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/results/PracEffects_NAS201TRAN_Results.csv",
                       row.names=1, stringsAsFactors = F)
# Values that have been calculated based scores adjusted for AFQT and z-scored based on VETSA 1 means and SD's
pracEffects_Zscored = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/results/PracEffects_NAS201TRAN_Zscored_Results.csv",
                                row.names=1, stringsAsFactors = F)

# Get basenames of test scores to adjust
varNames = c("MR1COR","TRL1T","TRL2T","TRL3T","TRL4T","TRL5T","CSSACC","MTXRAW","CVA1RAW","CVATOT",
             "CVSDFR","CVLDFR","AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN",
             "AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN","AFQTBXPCTTRAN","DSFRAW","DSBRAW",
             "SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
             "STRWRAW","STRCRAW","STRCWRAW","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR",
             "CFCOR","CSCOR","SRTLMEAN","SRTLSTD","SRTRMEAN","SRTRSTD","SRTGMEAN","SRTGSTD","CHRTLMEAN",
             "CHRTRMEAN","CHRTLSTD","CHRTRSTD","CHRTGMEAN","CHRTGSTD","RSATOT")


#-------------------------------------------#
#     Apply practice effect adjustments     # 
#-------------------------------------------#

# Apply practice effect adjustment to scores that have had age 20 AFQT regressed out and z-scored 
# using VETSA 1 mean and SD.

nas201adj_PEadj = nas201adj_df
idxV1V2 = which(nas201adj_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0("z", varName, "_V2_adj")
  varName_PE = paste0("z", varName)
  pe_Zscored = pracEffects_Zscored[varName_PE,"PracticeEffect"]
  nas201adj_PEadj[idxV1V2, varName_V2] = nas201adj_PEadj[idxV1V2, varName_V2] - pe_Zscored
}

# Save out dataset that has been adjusted for AFQT and Practice Effects, as well as z-scored based on VETSA 1 values 
write.csv(nas201adj_PEadj, '/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_PracEffect_Adj_Zscored.csv',
          row.names=FALSE)

############################################

# Apply practice effect adjustment to raw scores.
# Z-score is converted to raw score scale based on mean and SD of scores that have had 
# age 20 AFQT regressed out. 

rawscore_PEadj = rawscores_df
idxV1V2 = which(rawscore_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2")
  varName_PE = varName
  pe_RawScale = pracEffects_RawScale[varName_PE,"PracticeEffect"]
  rawscore_PEadj[idxV1V2, varName_V2] = rawscore_PEadj[idxV1V2, varName_V2] - pe_RawScale
}

# Save out dataset that has been adjusted for AFQT and Practice Effects, and is kept on raw test score scale 
write.csv(rawscore_PEadj, '/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_PracEffect_Adj.csv',
          row.names=FALSE)

