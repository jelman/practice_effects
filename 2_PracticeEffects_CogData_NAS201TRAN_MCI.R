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
# Follow-ups to adjust:                                                                           #
# ---------------------                                                                           #
# V1V2V3:   time1 -> time2, time2 -> time3                                                        #
# V1V2:     time1 -> time2                                                                        #
# V2V3:     time2 -> time3                                                                        #
# V1V3:     time1 -> time3                                                                        #
# V1newV2:  time2 -> time3 (calculated as time1 -> time2)                                         #
#                                                                                                 #
###################################################################################################
  
#---------------------------------------------------------------------------#
#   Load data, filter for subjects of interest and define tests             #
#   to calculate practice effects for.                                      #
#---------------------------------------------------------------------------#
# Load data that has been adjusted for age 20 AFQT
allDat = read.csv("/home/jelman/netshare/K/Projects/PracEffects_MCI/data/V1V2_NAS201TRAN_Adj.csv")

# Select subjects from groups of interest
subsetDat = allDat %>%
  filter(VETSAGRP=="V1V2" | VETSAGRP=="V1" | VETSAGRP=="V2AR")

# Take out V1ne subject in order to add back in later
V1neDat = allDat %>% filter(VETSAGRP=="v1ne")

# Create vector of all variable names to calculate practice effects for
testVars = c("VRCTOTSS","MTXT","DSPSS","SSPSS","LNSC","TRL1TSC","TRL4TSC",
             "STRIT","LFCORSC","CFCORSC","CSSACCSC","MR1CORZ","HFTOTCORZ",
             "CVLT","LM","VR","TRL","STR")


#-----------------------------------------------#
#                 Define functions              #
#-----------------------------------------------#


# Calculate practice effects for a given test score. 
# Inputs:
# ---------
# df: dataset 
# varName: Variable name to calculate practice effects for
# idxReturn : Subjects that are returning for follow-up                               
# idxReplace : Attrition replacements tested for the first time at follow-up          
# idxAll : Full sample assessed at baseline                                           
#
# Returns:
# ----------
# Value of estimated practice effect for varName

calcPracticeEffect = function(df, varName, idxReturn, idxReplace,idxAll){
  varV1 = varName
  varV2 = paste0(varV1, "_V2")
  varV1 = paste0(varV1,"_adj")
  varV2 = paste0(varV2,"_adj")
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
# pracEffects: List of estimates practice effects for variables listed in testVars
# idxReturn : Subjects that are returning for follow-up                               
# idxReplace : Attrition replacements tested for the first time at follow-up          
# idxAll : Full sample assessed at baseline                                           
#
# Returns:
# ----------
# List of p-values for all variables in testVars

calcPvalues = function(df, testvars, pracEffects, idxReturn, idxReplace, idxAll){
  ### Run permutation testing to generate p-values for practice effects ###
  set.seed(21)
  # Set parameters for permutation testing of practice effects
  N = nrow(dat)                   # Numer of subjects
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
    permResults[i,] = sapply(testVars, function(x) calcPracticeEffect(data, x, 
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
# namesReturn : Names of groups that are returning for follow-up                               
# namesReplace : Names of attrition replacements tested for the first time at follow-up          
# namesAll : Names of groups in full sample assessed at baseline                                           
#
# Returns:
# ----------
# List of standard errors for all variables in testVars

calcStdError = function(df, testVars, namesReturn, namesReplace, namesAll){
  # Define bootstrap function
  bootPracticeEffect = function(data, strata){
    dat = data[strata,]
    idxReturn = which(dat$VETSAGRP %in% namesReturn)
    idxReplace = which(dat$VETSAGRP %in% namesReplace)
    idxAll = which(dat$VETSAGRP %in% namesAll)
    sampResults = sapply(testVars, function(x) calcPracticeEffect(dat, x, 
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




#-------------------------------------------------------------------------------------#
# Calculate practice effects for all groups.                                          #
#                                                                                     #
# Define function to calculate practice effects for a given measure. For each set     #
# of practice effect calculations, appropriate groups need to be defined.             #
#                                                                                     #
# Get names and indices of the following groups:                                      #
# [idx/names]Return : Subjects that are returning for follow-up                       #
# [idx/names]Replace : Attrition replacements tested for the first time at follow-up  #
# [idx/names]All : Full sample assessed at baseline                                   #
#-------------------------------------------------------------------------------------#


#################################
#   V1V2V3:   time2 -> time3    #
#################################

# Define names of groups
namesReturn = c("V1V2V3")
namesReplace = c("V3AR")
namesAll = c("V1V2V3", "V1V2")
# Define indices of groups
idxReturn = which(subsetDat$VETSAGRP=="V1V2V3")
idxReplace = which(subsetDat$VETSAGRP=="V3AR")
idxAll = which(subsetDat$VETSAGRP=="V1V2V3" | subsetDat$VETSAGRP=="V1V2")

# Calculate practice effects for all cognitive domains and tests
pracEffects = sapply(testVars, function(x) calcPracticeEffect(subsetDat, x, idxReturn, idxReplace, idxAll))
# Calculate p-values for all tests
pvals = calcPvalues(subsetDat, testvars, pracEffects, idxReturn, idxReplace, idxAll)
# Calculate standard errors for all tests
SEvals = calcStdError(subsetDat, testVars, namesReturn, namesReplace, namesAll)
# Combine practice effects results and permutation p-values
results = data.frame("PracticeEffect" = pracEffects, SE=SEvals, "P" = pvals)
# Write out practice effect results (adjustment value, estimate of precision, and p value)
write.csv(results, '~/netshare/M/PSYCH/KREMEN/Practice Effect MCI/Results/PracEffectsMCI_NAS201TRAN_V1V2V3-t2t3.csv')

