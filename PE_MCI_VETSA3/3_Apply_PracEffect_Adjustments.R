#########################################################################################
# This script adjusts cognitive test scores used in MCI diagnosis for practice effects. # 
# It should be run after adjusting for age 20 AFQT and calculating practice effects.    #
# The resulting scores will be adjusted for age 20 AFQT and practice effects, yet will  #
# remain on the raw score scale.                                                        #
#                                                                                       #
# Note: Only follow-up scores of returnee subjects are adjusted for practice            #
# effects.                                                                              #
#                                                                                       #
#########################################################################################


# Scores that have been adjusted for AFQT 
nas201adj_df = read.csv("/home/jelman/netshare/K/Projects/PracEffects_MCI/data/V1V2_NAS201TRAN_Adj.csv",
                        stringsAsFactors = F)

## Load calculated practice effects values to adjust by ##

# Values that have been calculated based on scores adjusted for AFQT but on raw test score scale.
pracEffects_RawScale = read.csv("/home/jelman/netshare/K/Projects/PracEffects_MCI/results/PracEffectsMCI_NAS201TRAN_Results.csv",
                       row.names=1, stringsAsFactors = F)


# Get basenames of test scores to adjust
varNames = row.names(pracEffects_RawScale)


#-------------------------------------------#
#     Apply practice effect adjustments     # 
#-------------------------------------------#

# Apply practice effect adjustment to scores that have had age 20 AFQT regressed out 
nas201adj_PEadj = nas201adj_df
idxV1V2 = which(nas201adj_PEadj$VETSAGRP=="V1V2")
for (varName in varNames) {
  varName_V2 = paste0(varName, "_V2_adj")
  varName_V2_PE = paste0(varName_V2, "_pe")
  peVal = pracEffects_RawScale[varName,"PracticeEffect"]
  nas201adj_PEadj[idxV1V2, varName_V2_PE] = nas201adj_PEadj[idxV1V2, varName_V2] - peVal
}

# Save out dataset that has been adjusted for AFQT and Practice Effects
write.csv(nas201adj_PEadj, '/home/jelman/netshare/K/Projects/PracEffects_MCI/results/V1V2_NAS201TRANadj_PEadj.csv',
          row.names=FALSE)

# Save out V1 and V2 separately
nas201adj_PEadjV1 = nas201adj_PEadj %>% dplyr::select(-contains("_V2")) 
nas201adj_PEadjV2 = nas201adj_PEadj %>% dplyr::select(VETSAID, CASE, NAS201TRAN, VETSAGRP, contains("_V2"))

write.csv(nas201adj_PEadjV1, '/home/jelman/netshare/K/Projects/PracEffects_MCI/results/V1_NAS201TRANadj_PEadj.csv',
          row.names=FALSE)
write.csv(nas201adj_PEadjV2, '/home/jelman/netshare/K/Projects/PracEffects_MCI/results/V2_NAS201TRANadj_PEadj.csv',
          row.names=FALSE)
