################################################################################
# Standardized AFQT score for VETSA 1.                                         #
#                                                                              #
# Individual sub-scores and total AFQT are filtered based on quality           #
# score ("z" rating). Tests scores are then standardized based on              #
# VETSA 1 means and standard deviations.                                       #
################################################################################


library(sjmisc)
library(dplyr)
library(psych)

# Load vetsa 1 merged data
vetsa1Dat = read_sas("K:/data/VETSA1_Aug2014/vetsa1merged_25aug2015_nomiss.sas7bdat")

###########################################
# Set scores with poor quality to missing #
###########################################

# VETSA AFQT Percentile Score Transformed
vetsa1Dat$afqtpcttran[which(vetsa1Dat$ZAFQT=="2")] = NA

# VETSA AFQT Vocabulary Percentile Score Transformed
vetsa1Dat$afqtvocpcttran[which(vetsa1Dat$ZAFQT=="2")] = NA

# VETSA AFQT Arithmetic Percentile Score Transformed
vetsa1Dat$afqtarpcttran[which(vetsa1Dat$ZAFQT=="2")] = NA

# VETSA AFQT Tools Percentile Score Transformed
vetsa1Dat$afqttlpcttran[which(vetsa1Dat$ZAFQT=="2")] = NA

# VETSA AFQT Boxes Percentile Score Transformed
vetsa1Dat$afqtbxpcttran[which(vetsa1Dat$ZAFQT=="2")] = NA


#########################################################
# Creating standardized scores and composite scores.    #
# Standardization is based off of VETSA1 Means and SDs  #
#########################################################

## Create function to save mean and SD of all variables ##
addScaleVals = function(df,varname, x) {
  meanVal = attr(x, which="scaled:center")
  sdVal = attr(x, which="scaled:scale")
  rbind(df, data.frame(Variable=varname, Mean=meanVal, SD=sdVal))
}

## Initialize dataframe to hold means and SDs # #
scaleValues = data.frame()

# Standardize VETSA AFQT Percentile Score Transformed
vetsa1Dat$zafqtpcttran = scale(vetsa1Dat$afqtpcttran)
scaleValues = addScaleVals(scaleValues, "afqtpcttran", vetsa1Dat$zafqtpcttran)

# Standardize VETSA AFQT Vocabulary Percentile Score Transformed
vetsa1Dat$zafqtvocpcttran = scale(vetsa1Dat$afqtvocpcttran)
scaleValues = addScaleVals(scaleValues, "afqtvocpcttran", vetsa1Dat$zafqtvocpcttran)

# Standardize VETSA AFQT Arithmetic Percentile Score Transformed
vetsa1Dat$zafqtarpcttran = scale(vetsa1Dat$afqtarpcttran)
scaleValues = addScaleVals(scaleValues, "afqtarpcttran", vetsa1Dat$zafqtarpcttran)

# Standardize VETSA AFQT Tools Percentile Score Transformed
vetsa1Dat$zafqttlpcttran = scale(vetsa1Dat$afqttlpcttran)
scaleValues = addScaleVals(scaleValues, "afqttlpcttran", vetsa1Dat$zafqttlpcttran)

# Standardize VETSA AFQT Boxes Percentile Score Transformed
vetsa1Dat$zafqtbxpcttran = scale(vetsa1Dat$afqtbxpcttran)
scaleValues = addScaleVals(scaleValues, "afqtbxpcttran", vetsa1Dat$zafqtbxpcttran)


#-------------------#
#  Save out datset  #
#-------------------#

# Select cognitive domain variables
vetsa1afqt = vetsa1Dat %>%
  dplyr::select(vetsaid,zafqtpcttran,zafqtvocpcttran,zafqtarpcttran,
                zafqttlpcttran,zafqtbxpcttran) 

# Save out data
write.csv(vetsa1afqt, 
          "K:/Projects/PracticeEffects/data/V1_AFQTscores.csv",
          row.names = F)

# Save out Means and SDs for use in scaling Vetsa 2 data
write.csv(scaleValues, 
          "K:/Projects/PracticeEffects/data/V1_AFQTscores_Means_SDs.csv",
          row.names = F)
