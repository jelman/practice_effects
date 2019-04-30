library(dplyr)
library(psych)
library(knitr)
library(permute)
library(boot)

###################################################################################################
# Adjustment for Age 20 AFQT:                                                                     #
# ---------------------------                                                                     #
# The scores have been adjusted for age 20 AFQT by regressing the (scaled) nas201tran variable    # 
# from each raw test score.                                                                       #
#                                                                                                 #
#                                                                                                 #
# Calculation of practice effects:                                                                #
# --------------------------------                                                                #
# The difference (D) between time 2 scores of longitudinal returnees (S1T2) and time 2 attrition  #
# replacements (S2T2) is the sum of attrition effects (A) and practice effects (P). The attrition #
# effect is calculated as the difference in time 1 scores of returnees (S1T1ret) compared to the  #
# entire group (S1T1all). The practice effect is therefore the difference D minus the attrition   #
# effect.                                                                                         #
#                                                                                                 #
#                                                                                                 #
#  D = A + P                                                                                      #
#                                                                                                 #
#  Difference score:                                                                              #
#    D = S1T2 - S2T2                                                                              #
#                                                                                                 #
#  Attrition effect:                                                                              #
#    A = S1T1ret - S1T1all                                                                        #
#                                                                                                 #
#  Practice effect:                                                                               #
#    P = D - A                                                                                    #
#                                                                                                 #
# Follow-ups to calculate practice effects for:                                                   #
# ---------------------                                                                           #
# V1V2V3:   time1 -> time2, time2 -> time3                                                        #
# V1V2:     time1 -> time2                                                                        #
# V2V3:     time2 -> time3                                                                        #
# V1V3:     time1 -> time3                                                                        #
#                                                                                                 #
###################################################################################################
  
# Get date for filenames
dstamp = Sys.Date()

#---------------------------------------------------------------------------#
#   Load data, filter for subjects of interest and define tests             #
#   to calculate practice effects for.                                      #
#---------------------------------------------------------------------------#
# Load data that has been adjusted for age 20 AFQT
allDat = read.csv("/home/jelman/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/data/intermediate_files/V1V2V3_CogData_NoMissingNAS201TRAN_Unadj_2019-04-30.csv")

# Select subjects from groups of interest
subsetDat = allDat %>%
  filter(VETSAGRP=="V1V2V3" | VETSAGRP=="V1V2" | VETSAGRP=="V1V3" | VETSAGRP=="V2V3" | VETSAGRP=="V1" | VETSAGRP=="V2" | VETSAGRP=="V3")

# # Take out V1ne subject in order to add back in later
# V1neDat = allDat %>% filter(VETSAGRP=="v1ne")

# Create vector of all variable names to calculate practice effects for
testVarsV1V2 = c("MR1COR","TRL1TLOG","TRL2TLOG","TRL3TLOG","TRL4TLOG","TRL5TLOG","CSSACC","MTXRAW","CVA1RAW","CVATOT","CVSDFR","CVLDFR",
                 "AFQTPCT","AFQTVOCPCT","AFQTARPCT","AFQTTLPCT","AFQTBXPCT","AFQTPCTTRAN","AFQTVOCPCTTRAN","AFQTARPCTTRAN","AFQTTLPCTTRAN",
                 "AFQTBXPCTTRAN","DSFRAW","DSBRAW","DSFMAX","DSTOT","SSPFRAW","SSPBRAW","SSPTOTP","LNTOT","LM1A","LM1B","LM2A","LM2B",
                 "LMITOT","LMDTOT","VRITOT","VRDTOT","VRCTOT","HFTOTCOR","STRWRAW","STRCRAW","STRCWRAW","STRIT","LFFCOR","LFACOR","LFSCOR","LFCOR",
                 "CFANCOR","CFBNCOR","CFCOR","CSCOR","RSATOT","SRTLMEANLOG","SRTLSTDLOG","SRTRMEANLOG","SRTRSTDLOG","SRTGMEANLOG","SRTGSTDLOG",
                 "CHRTLMEANLOG","CHRTRMEANLOG","CHRTLSTDLOG","CHRTRSTDLOG","CHRTGMEANLOG","CHRTGSTDLOG","AXHITRATE","AXFARATE","AXMISSRATE",
                 "BXHITRATE","BXFARATE","BXMISSRATE")
# Remove AX-CPT variables from calculations involving V3 data
testVarsV3 = testVarsV1V2[! testVarsV1V2 %in% c("AXHITRATE","AXFARATE","AXMISSRATE","BXHITRATE","BXFARATE","BXMISSRATE")]



#-----------------------------------------------#
#           START DEFINING FUNCTIONS            #
#-----------------------------------------------#


# Calculate practice effects for a given test score. 
# Inputs:
# ---------
# df: dataset 
# varName: Variable name to calculate practice effects for
# suffix: List of suffixes that distinguish timepoints
# idxReturn : Subjects that are returning for follow-up                               
# idxReplace : Attrition replacements tested for the first time at follow-up          
# idxAll : Full sample assessed at baseline                                           
#
# Returns:
# ----------
# Value of estimated practice effect for varName

calcPracticeEffect = function(df, varName, suffix, idxReturn, idxReplace,idxAll){
  varV1 = paste0(varName, suffix[1])
  varV2 = paste0(varName, suffix[2])
  # Longitudinal Subjects Time 2
  S1T2 = mean(df[idxReturn, varV2], na.rm=T)
  # Attrition Replacement Subjects Time 2
  S2T2 = mean(df[idxReplace, varV2], na.rm=T)
  # Longitudinal Returnees Time 1
  S1T1ret = mean(df[idxReturn, varV1], na.rm=T)
  # All Subjects Time 1
  S1T1all = mean(df[idxAll, varV1], na.rm=T)
  
  # Difference score
  D = S1T2 - S2T2
  # Attrition effect
  A = S1T1ret - S1T1all
  # Practice Effect
  P = D - A
  P
}


# Calculate p-value for practice effects using permutation testing
# Inputs:
# ---------
# df: dataset 
# testVars: List of variable names to calculate p-values for
# suffix: List of suffixes that distinguish timepoints
# pracEffects: List of estimates practice effects for variables listed in testVars
# idxReturn : Subjects that are returning for follow-up                               
# idxReplace : Attrition replacements tested for the first time at follow-up          
# idxAll : Full sample assessed at baseline                                           
#
# Returns:
# ----------
# List of p-values for all variables in testVars

calcPvalues = function(df, testVars, suffix, pracEffects, idxReturn, idxReplace, idxAll){
  ### Run permutation testing to generate p-values for practice effects ###
  set.seed(21)
  # Set parameters for permutation testing of practice effects
  N = nrow(df)                   # Numer of subjects
  nPerm = 10000                      # Number of permutations to run
  nLong = length(idxReturn)         # Number of longitudinal subjects
  nAR = length(idxReplace)              # Number of attrition replacement subjects
  # Initialize empty matrix for permutation results
  permResults = matrix(ncol=length(testVars), nrow=nPerm)
  colnames(permResults) = testVars
  # Run permutations and collect results into matrix
  for(i in 1:nPerm){
    idxT2 = sample(c(idxReturn, idxReplace))
    idxT1 = sample(idxAll)
    idxReturnPerm = idxT2[1:nLong]
    idxReplacePerm = idxT2[(nLong+1):(nLong+nAR)]
    idxAllPerm = idxT1[1:nLong]
    permResults[i,] = sapply(testVars, function(x) calcPracticeEffect(df, x, suffix,
                                                                      idxReturnPerm,
                                                                      idxReplacePerm,
                                                                      idxAllPerm))
  }
  permResults = data.frame(permResults)
  # Calculate p values based on permutations and observed values
  pvalsPerm = apply(permResults, 1, function(x) abs(x) >= abs(pracEffects))
  pvals = rowMeans(pvalsPerm)
  pvals
}



# Calculate standard errors for practice effects using bootstrap resampling
# Inputs:
# ---------
# df: dataset 
# testVars: List of variable names to calculate p-values for
# suffix: List of suffixes that distinguish timepoints
# namesReturn : Names of groups that are returning for follow-up                               
# namesReplace : Names of attrition replacements tested for the first time at follow-up          
# namesAll : Names of groups in full sample assessed at baseline                                           
#
# Returns:
# ----------
# List of standard errors for all variables in testVars

calcStdError = function(df, testVars, suffix, namesReturn, namesReplace, namesAll){
  # Define bootstrap function
  bootPracticeEffect = function(data, strata){
    dat = data[strata,]
    idxReturn = which(dat$VETSAGRP %in% namesReturn)
    idxReplace = which(dat$VETSAGRP %in% namesReplace)
    idxAll = which(dat$VETSAGRP %in% namesAll)
    sampResults = sapply(testVars, function(x) calcPracticeEffect(dat, x, 
                                                                  suffix,
                                                                  idxReturn,
                                                                  idxReplace,
                                                                  idxAll))
    return(sampResults)
  }
  set.seed(21)
  nBoot = 10000
  boot.out = boot(df, statistic=bootPracticeEffect, strata=df$VETSAGRP, R=nBoot)
  SEvals = apply(boot.out$t, 2, sd)
  SEvals
}


#-----------------------------------------------#
#           STOP DEFINING FUNCTIONS             #
#-----------------------------------------------#


#---------------------------------------------------------------------------------------#
# Calculate practice effects for all groups.                                            #
#                                                                                       #
# Define function to calculate practice effects for a given measure. For each set       #
# of practice effect calculations, appropriate groups need to be defined.               #
#                                                                                       #
# Set names and indices of the following groups:                                        #
# namesReturn : Subjects that are returning for follow-up                               #
# namesReplace : Attrition replacements tested for the first time at follow-up          #
# namesAll : Full sample assessed at baseline                                           #
# suffix : List of suffixes that distinguish timepoints (e.g., c("_V2_nas","_V3_nas"))  #
#---------------------------------------------------------------------------------------#


#################################
#   V1V2V3:   time2 -> time3    #
#################################

# Set names of groups
namesReturn = c("V1V2V3")
namesReplace = c("V3")
namesAll = c("V1V2V3", "V1V2")
suffix = c("_V2_nas", "_V3_nas")

# Define indices of groups
idxReturn = which(subsetDat$VETSAGRP %in% namesReturn)
idxReplace = which(subsetDat$VETSAGRP %in% namesReplace)
idxAll = which(subsetDat$VETSAGRP %in% namesAll)

# Calculate practice effects for all cognitive domains and tests
pracEffects = sapply(testVarsV3, function(x) calcPracticeEffect(subsetDat, x, suffix, idxReturn, idxReplace, idxAll))
# Calculate p-values for all tests
pvals = calcPvalues(subsetDat, testVarsV3, suffix, pracEffects, idxReturn, idxReplace, idxAll)
# Calculate standard errors for all tests
SEvals = calcStdError(subsetDat, testVarsV3, suffix, namesReturn, namesReplace, namesAll)
# Combine practice effects results and permutation p-values
results = data.frame("PracticeEffect" = pracEffects, SE=SEvals, "P" = pvals)
# Write out practice effect results (adjustment value, estimate of precision, and p value)
outname = paste0('~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/results/PracEffects_NAS201TRAN_V1V2V3-t2t3_',dstamp,'.csv')
write.csv(results, outname)



########################################
#   V1V2V3 & V1V2:   time1 -> time2    #
########################################

# Set names of groups
namesReturn = c("V1V2V3", "V1V2")
namesReplace = c("V2V3", "V2")
namesAll = c("V1V2V3", "V1V2", "V1V3", "V1", "V1neV3", "V1ne")
suffix = c("_nas", "_V2_nas")

# Define indices of groups
idxReturn = which(subsetDat$VETSAGRP %in% namesReturn)
idxReplace = which(subsetDat$VETSAGRP %in% namesReplace)
idxAll = which(subsetDat$VETSAGRP %in% namesAll)

# Calculate practice effects for all cognitive domains and tests
pracEffects = sapply(testVarsV1V2, function(x) calcPracticeEffect(subsetDat, x, suffix, idxReturn, idxReplace, idxAll))
# Calculate p-values for all tests
pvals = calcPvalues(subsetDat, testVarsV1V2, suffix, pracEffects, idxReturn, idxReplace, idxAll)
# Calculate standard errors for all tests
SEvals = calcStdError(subsetDat, testVarsV1V2, suffix, namesReturn, namesReplace, namesAll)
# Combine practice effects results and permutation p-values
results = data.frame("PracticeEffect" = pracEffects, SE=SEvals, "P" = pvals)
# Write out practice effect results (adjustment value, estimate of precision, and p value)
outname = paste0('~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/results/PracEffects_NAS201TRAN_V1V2-V1V2V3-t1t2_',dstamp,'.csv')
write.csv(results, outname)


###############################
#   V2V3:   time2 -> time3    #
###############################

# Set names of groups
namesReturn = c("V2V3")
namesReplace = c("V3")
namesAll = c("V2V3", "V2")
suffix = c("_V2_nas", "_V3_nas")

# Define indices of groups
idxReturn = which(subsetDat$VETSAGRP %in% namesReturn)
idxReplace = which(subsetDat$VETSAGRP %in% namesReplace)
idxAll = which(subsetDat$VETSAGRP %in% namesAll)

# Calculate practice effects for all cognitive domains and tests
pracEffects = sapply(testVarsV3, function(x) calcPracticeEffect(subsetDat, x, suffix, idxReturn, idxReplace, idxAll))
# Calculate p-values for all tests
pvals = calcPvalues(subsetDat, testVarsV3, suffix, pracEffects, idxReturn, idxReplace, idxAll)
# Calculate standard errors for all tests
SEvals = calcStdError(subsetDat, testVarsV3, suffix, namesReturn, namesReplace, namesAll)
# Combine practice effects results and permutation p-values
results = data.frame("PracticeEffect" = pracEffects, SE=SEvals, "P" = pvals)
# Write out practice effect results (adjustment value, estimate of precision, and p value)
outname = paste0('~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/results/PracEffects_NAS201TRAN_V2V3-t2t3_',dstamp,'.csv')
write.csv(results, outname)


###############################
#   V1V3:   time1 -> time3    #
###############################

# Set names of groups
namesReturn = c("V1V3")
namesReplace = c("V3")
namesAll = c("V1V2V3", "V1V2", "V1V3", "V1", "V1neV3", "V1ne")
suffix = c("_nas", "_V3_nas")

# Define indices of groups
idxReturn = which(subsetDat$VETSAGRP %in% namesReturn)
idxReplace = which(subsetDat$VETSAGRP %in% namesReplace)
idxAll = which(subsetDat$VETSAGRP %in% namesAll)

# Calculate practice effects for all cognitive domains and tests
pracEffects = sapply(testVarsV3, function(x) calcPracticeEffect(subsetDat, x, suffix, idxReturn, idxReplace, idxAll))
# Calculate p-values for all tests
pvals = calcPvalues(subsetDat, testVarsV3, suffix, pracEffects, idxReturn, idxReplace, idxAll)
# Calculate standard errors for all tests
SEvals = calcStdError(subsetDat, testVarsV3, suffix, namesReturn, namesReplace, namesAll)
# Combine practice effects results and permutation p-values
results = data.frame("PracticeEffect" = pracEffects, SE=SEvals, "P" = pvals)
# Write out practice effect results (adjustment value, estimate of precision, and p value)
outname = paste0('~/netshare/M/PSYCH/KREMEN/VETSA DATA FILES_852014/a_Practice effect revised cog scores/Practice Effect Cognition/V1V2V3/results/PracEffects_NAS201TRAN_V1V3-t1t3_',dstamp,'.csv')
write.csv(results, outname)
