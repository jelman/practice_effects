library(dplyr)
library(psych)
library(knitr)
library(permute)
library(boot)

###################################################################################################
#  Adjustment for Age 20 AFQT                                                                     #
#  --------------------------                                                                     #
# The scores have been adjusted for age 20 AFQT by regressing the (scaled) nas201tran variable    # 
# from each raw test score.                                                                       #
#                                                                                                 #
#                                                                                                 #
#  Calculation of practice effects                                                                #
#  -------------------------------                                                                #
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
###################################################################################################
  
  
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

# Create indices of different groups
# idxReturn : Returnees 
# idxReplace : Replacements
# idxAll : Full sample assessed at baseline
idxReturn = which(subsetDat$VETSAGRP=="V1V2")
idxReplace = which(subsetDat$VETSAGRP=="V2AR")
idxAll = which(subsetDat$VETSAGRP=="V1V2" | subsetDat$VETSAGRP=="V2AR")

calcPracticeEffect = function(dat, varName, idxReturn, idxReplace,idxAll){
  varV1 = varName
  varV2 = paste0(varV1, "_V2")
  varV1 = paste0(varV1,"_adj")
  varV2 = paste0(varV2,"_adj")
  # Longitudinal Subjects Time 2
  S1T2 = mean(dat[idxReturn, varV2], na.rm=T)
  # Attrition Replacement Subjects Time 2
  S2T2 = mean(dat[idxReplace, varV2], na.rm=T)
  # Longitudinal Returnees Time 1
  S1T1ret = mean(dat[idxReturn, varV1], na.rm=T)
  # All Subjects Time 1
  S1T1all = mean(dat[idxAll, varV1], na.rm=T)
  
  # Difference score
  D = S1T2 - S2T2
  # Attrition effect
  A = S1T1ret - S1T1all
  # Practice Effect
  P = D - A
  P
}


# Calculate practice effects for all cognitive domains and tests
pracEffects = sapply(testVars, function(x) calcPracticeEffect(subsetDat, x, idxReturn, idxReplace,idxAll))


### Run permutation testing to generate p-values for practice effects ###

set.seed(21)
# Set parameters for permutation testing of practice effects
N = nrow(subsetDat)                   # Numer of subjects
nPerm = 10000                      # Number of permutations to run
nLong = length(idxReturn)         # Number of longitudinal subjects
nAR = length(idxReplace)              # Number of attrition replacement subjects

# Get indices of different groups
idxReturn = which(subsetDat$VETSAGRP=="V1V2")
idxReplace = which(subsetDat$VETSAGRP=="V2AR")
idxDrop = which(subsetDat$VETSAGRP=="V1")

# Initialize empty matrix for permutation results
permResults = matrix(ncol=length(testVars), nrow=nPerm)
colnames(permResults) = testVars

# Run permutations and collect results into matrix
for(i in 1:nPerm){
    idxT2 = sample(c(idxReturn, idxReplace))
    idxT1 = sample(c(idxReturn, idxDrop))
    
    idxReturnPerm = idxT2[1:nLong]
    idxReplacePerm = idxT2[(nLong+1):(nLong+nAR)]
    idxAllPerm = idxT1[1:nLong]
    permResults[i,] = sapply(testVars, function(x) calcPracticeEffect(subsetDat, x, 
                                                      idxReturnPerm,
                                                      idxReplacePerm,
                                                      idxAllPerm))
}
permResults = data.frame(permResults)

# Calculate p values based on permutations and observed values
pvalsPerm = apply(permResults, 1, function(x) abs(x) >= abs(pracEffects))
pvals = rowMeans(pvalsPerm)

### Generate bootstrapped confidence intervals and standard error ###

bootPracticeEffect = function(data, idx){
  dat = data[idx,]
  idxReturnboot = which(dat$VETSAGRP=="V1V2")
  idxReplaceboot = which(dat$VETSAGRP=="V2AR")
  idxAllboot = which(subsetDat$VETSAGRP=="V1V2" | subsetDat$VETSAGRP=="V2AR")
  sampResults = sapply(testVars, function(x) calcPracticeEffect(dat, x, 
                                                  idxReturnboot,
                                                  idxReplaceboot,
                                                  idxAllboot))
  return(sampResults)
}

set.seed(21)
nBoot = 10000
boot.out = boot(subsetDat, statistic=bootPracticeEffect, strata=subsetDat$VETSAGRP, R=nBoot)

# # Alternative way to conduct bootstrapping
# set.seed(21)
# # Set parameters for permutation testing of practice effects
# N = nrow(subsetDat)                   # Numer of subjects
# nBoot = 1000                      # Number of boostrap resamples to run
# nLong = length(idxS1T1ret)         # Number of longitudinal subjects
# nAR = length(idxS2T2)              # Number of attrition replacement subjects
# 
# # Get indices of different groups
# idxReturn = which(subsetDat$VETSAGRP=="V1V2")
# idxReplace = which(subsetDat$VETSAGRP=="V2AR")
# 
# # Initialize empty matrix for boostrap results
# bootResults = matrix(ncol=length(testVars), nrow=nBoot)
# colnames(bootResults) = testVars
# 
# # Run bootstrap resamples and collect results into matrix
# for(i in 1:nBoot){
#     idxReturnboot = sample(idxReturn, replace=TRUE)
#     idxReplaceboot = sample(idxReplace, replace=TRUE)
#     idxAllboot = which(subsetDat$VETSAGRP=="V1V2" | subsetDat$VETSAGRP=="V2AR")
#     bootResults[i,] = sapply(testVars, function(x) calcPracticeEffect(subsetDat, x, 
#                                                       idxReturnboot,
#                                                       idxReplaceboot,
#                                                       idxAllboot))
# }


### Gather results into dataframe ###

# Combine practice effects results and permutation p-values
results = data.frame("PracticeEffect" = pracEffects, SE=apply(boot.out$t, 2, sd), "P" = pvals)


kable(results, digits=3, caption="Individual Tests")


# Write out practice effect results (adjustment value, estimate of precision, and p value)
write.csv(results, '~/netshare/M/PSYCH/KREMEN/Practice Effect MCI/Results/PracEffectsMCI_NAS201TRAN_Results.csv')

