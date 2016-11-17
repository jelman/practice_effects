#######################################################################################
# This script adjusts cognitive test scores for practice effects. Two datasets are    #
# adjusted:                                                                           #
#                                                                                     #
# 1.) Scores on the raw test score scale that are not adjusted for age 20 AFQT.       #
#     These are adjusted by practice effects calculated from scores that *have*       #
#     been adjusted for AFQT, but remain on raw score scale.                          #
#                                                                                     #
# 2.) Scores that have been adjusted for age 20 AFQT and z-scored based on VETSA1     #
#     means and standard deviations. These are adjusted by practice effects           #
#     calculated from scores that have been adjusted for AFQT and z-scored based      #
#     on VETSA1 means and standard deviations.                                        #
#                                                                                     #
# Note: Only time 2 scores of longitudinal (V1V2) subjects are adjusted               #
#                                                                                     #
#######################################################################################

#-----------------------------------------#
#   1. Scores on raw score scale          #
#-----------------------------------------#

## Load dataset to be adjusted ##

# Scores that have been adjusted for AFQT and z-scored based on VETSA 1 means and SD's
unadj_df = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_Unadj.csv",
                        stringsAsFactors = F)

# Values that have been calculated based scores adjusted for AFQT and z-scored based on VETSA 1 means and SD's
pracEffects = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/results/PracEffects_NAS201TRAN_Results.csv",
                                row.names=1, stringsAsFactors = F)

# Get basenames of test scores to adjust
varNames = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
             "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
             "AFQTBXPCTTRAN","DSFRAW","DSBRAW","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
             "STRWRAW","STRCRAW","STRCWRAW","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","SRTLMEANLOG",
             "SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG","CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG",
             "CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG","RSATOT")



## Apply practice effect adjustments ## 

# Apply practice effect adjustment to scores that have had age 20 AFQT regressed out and z-scored 
# using VETSA 1 mean and SD.

unadj_df_PEadj = unadj_df
idxV1V2 = which(unadj_df_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2")
  varName_PE = paste0(varName)
  pe_Zscored = pracEffects[varName_PE,"PracticeEffect"]
  unadj_df_PEadj[idxV1V2, varName_V2] = unadj_df_PEadj[idxV1V2, varName_V2] - pe_Zscored
}

# Save out dataset that has been adjusted for AFQT and Practice Effects, as well as z-scored based on VETSA 1 values 
write.csv(unadj_df_PEadj, '/home/jelman/netshare/K/Projects/PracticeEffects/results/CogData_Unadj_RawScale.csv',
          row.names=FALSE)

#---------------------------------------#
#   2. Scores on z-score scale          #
#---------------------------------------#

## Load dataset to be adjusted ##

# Scores that have been adjusted for AFQT and z-scored based on VETSA 1 means and SD's
nas201adjzscore_df = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_Adj_Zscored.csv",
                        stringsAsFactors = F)

## Load calculated practice effects values to adjust by ##


# Values that have been calculated based scores adjusted for AFQT and z-scored based on VETSA 1 means and SD's
pracEffects_Zscored = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/results/PracEffects_NAS201TRAN_Zscored_Results.csv",
                               row.names=1, stringsAsFactors = F)

# Get basenames of test scores to adjust
varNames = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
             "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
             "AFQTBXPCTTRAN","DSFRAW","DSBRAW","SSPFRAW","SSPBRAW","LNTOT","LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR",
             "STRWRAW","STRCRAW","STRCWRAW","LFFCOR","LFACOR","LFSCOR","LFCOR","CFANCOR","CFBNCOR","CFCOR","CSCOR","SRTLMEANLOG",
             "SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG","CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG",
             "CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG","RSATOT")



## Apply practice effect adjustments ## 

# Apply practice effect adjustment to scores that have had age 20 AFQT regressed out and z-scored 
# using VETSA 1 mean and SD.

nas201adjzscore_PEadj = nas201adjzscore_df
idxV1V2 = which(nas201adjzscore_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0("z", varName, "_V2_adj")
  varName_PE = paste0("z", varName)
  pe_Zscored = pracEffects_Zscored[varName_PE,"PracticeEffect"]
  nas201adjzscore_PEadj[idxV1V2, varName_V2] = nas201adjzscore_PEadj[idxV1V2, varName_V2] - pe_Zscored
}

# Save out dataset that has been adjusted for AFQT and Practice Effects, as well as z-scored based on VETSA 1 values 
write.csv(nas201adjzscore_PEadj, '/home/jelman/netshare/K/Projects/PracticeEffects/results/CogData_NAS201TRAN_PracEffect_Adj_Zscored.csv',
          row.names=FALSE)

