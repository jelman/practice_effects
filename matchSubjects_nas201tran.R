###############################################################################
# Script to generate matched groups used to calculate Practice Effects.       #
#                                                                             #
# VETSA Attrition Replacements tend to score lower on age 20 AFQT.            #
# Therefore, practice effects may be contaminated by longstanding             #
# differences in general cognitive ability. As an alternative to              #
# regressing out age 20 AFQT (nas201tran) from test scores, we can            #
# match subjects on variables of interest.                                    #
###############################################################################


# Load libraries
library(MatchIt)

#-------------------------#
#   Match Vetsa 2 groups  #
#-------------------------#

# Read in dataset containing longitudinal subjects (V1V2) & attrition replacements (V2AR)
vetsa2Data = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/GroupComparisons_V1V2_V2AR.csv", 
                      stringsAsFactors = F)
# Create treatment variable considering V2AR subjects as treatment group
vetsa2Data$treat = ifelse(vetsa2Data$VETSAGRP=="V2AR",1,0)
# Convert Age to integer to coarsen matching 
vetsa2Data$AGE = as.integer(round(vetsa2Data$AGE_FU1, digits=0))
# Only include subjects with Age 20 AFQT score
vetsa2Data = subset(vetsa2Data, !is.na(vetsa2Data$nas201tran))
# Subset data. MatchIt does not like missing data, even in variables that are not included in model.
dataV2 = vetsa2Data[c("vetsaid","treat","AGE","nas201tran")]
# Generate match on age and age 20 AFQT. Match 2 controls per each treatment subject
m.outV2 = matchit(treat ~ AGE + nas201tran, data=dataV2, method="optimal",ratio=2)
# Display matching results
summary(m.outV2)
plot(m.outV2)
# Get subset of data including only matched subjects
m.dataV2 = match.data(m.outV2)
# Verify no significant group difference on age 20 AFQT
t.test(nas201tran ~ treat, data=m.dataV2)

#-------------------------#
#   Match Vetsa 1 groups  #
#-------------------------#

# Read in dataset containing attriters (V1) & attrition replacements (V2AR)
vetsa1ARData = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/GroupComparisons_V1_V2AR.csv", 
                      stringsAsFactors = F)
# Create treatment variable considering V2AR subjects as treatment group
vetsa1ARData$treat = ifelse(vetsa1ARData$VETSAGRP=="V2AR",1,0)
# Only include subjects with Age 20 AFQT score
vetsa1ARData = subset(vetsa1ARData, !is.na(vetsa1ARData$nas201tran))
# Subset data. MatchIt does not like missing data, even in variables that are not included in model.
dataV1 = vetsa1ARData[c("vetsaid","treat","nas201tran")]
# Generate match on age 20 AFQT (ages should differ). Match 1 control per each treatment subject
m.outV1 = matchit(treat ~ nas201tran, data=dataV1, method="optimal", ratio=1)
# Display matching results
summary(m.outV1)
plot(m.outV1)
# Get subset of data including only matched subjects
m.dataV1 = match.data(m.outV1)
# Verify no significant group difference on age 20 AFQT
t.test(nas201tran ~ treat, data=m.dataV1)

#--------------------------------------------------------#
#   Create Cognitive Domain dataset of mathced subjects  #
#--------------------------------------------------------#

# Load unadjusted practice effects data (cognitive domains and afqt scores)
peData = read.csv("/home/jelman/netshare/K/Projects/PracticeEffects/data/PracEffectData_Unadj.csv")
# Subset data for only subjects who were matched
m.subjects = union(m.dataV2$vetsaid, m.dataV1$vetsaid)
m.peData = peData[peData$vetsaid %in% m.subjects,]
# Write out dataset
write.csv(m.peData, "/home/jelman/netshare/K/Projects/PracticeEffects/data/PracEffectData_Matched_nas201tran.csv",
          row.names = F)
