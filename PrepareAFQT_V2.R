################################################################################
# Standardized AFQT score for VETSA 2.                                         #
#                                                                              #
# Individual sub-scores and total AFQT are filtered based on quality           #
# score ("z" rating). Tests scores are then standardized based on              #
# VETSA 1 means and standard deviations.                                       #
################################################################################


library(sjmisc)
library(dplyr)
library(psych)

# Load vetsa 1 merged data
vetsa2Dat = read_sas("K:/data/VETSA2_April2015/vetsa2merged_1dec2015_edits.sas7bdat")

###########################################
# Set scores with poor quality to missing #
###########################################

# VETSA AFQT Percentile Score Transformed
vetsa2Dat$afqtpcttran_v2[which(vetsa2Dat$ZAFQT_v2=="2")] = NA

# VETSA AFQT Vocabulary Percentile Score Transformed
vetsa2Dat$afqtvocpcttran_v2[which(vetsa2Dat$ZAFQT_v2=="2")] = NA

# VETSA AFQT Arithmetic Percentile Score Transformed
vetsa2Dat$afqtarpcttran_v2[which(vetsa2Dat$ZAFQT_v2=="2")] = NA

# VETSA AFQT Tools Percentile Score Transformed
vetsa2Dat$afqttlpcttran_v2[which(vetsa2Dat$ZAFQT_v2=="2")] = NA

# VETSA AFQT Boxes Percentile Score Transformed
vetsa2Dat$afqtbxpcttran_v2[which(vetsa2Dat$ZAFQT_v2=="2")] = NA


#########################################################
# Creating standardized scores and composite scores.    #
# Standardization is based off of VETSA1 Means and SDs #
#########################################################

# Load means and SDs from Vetsa 1
scaleValues = read.csv("K:/Projects/PracticeEffects/data/V1_AFQTscores_Means_SDs.csv")

# Standardize VETSA AFQT Percentile Score Transformed
vetsa2Dat$zafqtpcttran_v2 = scale(vetsa2Dat$afqtpcttran_v2, 
                      center=scaleValues$Mean[scaleValues$Variable=="afqtpcttran"],
                      scale=scaleValues$SD[scaleValues$Variable=="afqtpcttran"])


# Standardize VETSA AFQT Vocabulary Percentile Score Transformed
vetsa2Dat$zafqtvocpcttran_v2 = scale(vetsa2Dat$afqtvocpcttran_v2, 
                    center=scaleValues$Mean[scaleValues$Variable=="afqtvocpcttran"],
                    scale=scaleValues$SD[scaleValues$Variable=="afqtvocpcttran"])

# Standardize VETSA AFQT Arithmetic Percentile Score Transformed
vetsa2Dat$zafqtarpcttran_v2 = scale(vetsa2Dat$afqtarpcttran_v2, 
                      center=scaleValues$Mean[scaleValues$Variable=="afqtarpcttran"],
                      scale=scaleValues$SD[scaleValues$Variable=="afqtarpcttran"])

# Standardize VETSA AFQT Tools Percentile Score Transformed
vetsa2Dat$zafqttlpcttran_v2 = scale(vetsa2Dat$afqttlpcttran_v2, 
                      center=scaleValues$Mean[scaleValues$Variable=="afqttlpcttran"],
                      scale=scaleValues$SD[scaleValues$Variable=="afqttlpcttran"])

# VETSA AFQT Boxes Percentile Score Transformed
vetsa2Dat$zafqtbxpcttran_v2 = scale(vetsa2Dat$afqtbxpcttran_v2, 
                    center=scaleValues$Mean[scaleValues$Variable=="afqtbxpcttran"],
                    scale=scaleValues$SD[scaleValues$Variable=="afqtbxpcttran"])







#-------------------#
#  Save out datset  #
#-------------------#

# Select cognitive domain variables
vetsa2afqt = vetsa2Dat %>%
  dplyr::select(vetsaid,zafqtpcttran_v2,zafqtvocpcttran_v2,zafqtarpcttran_v2,
                zafqttlpcttran_v2,zafqtbxpcttran_v2) 

# Save out data
write.csv(vetsa2afqt, 
          "K:/Projects/PracticeEffects/data/V2_AFQTscores.csv",
          row.names = F)

