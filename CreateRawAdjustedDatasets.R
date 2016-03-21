######################################################################
# Script to create datasets for practice effect analyses.            #
# All datasets will include composite cognitive domain scores as     #
# well as individual tests comprising them. AFQT and subtests        #
# are also included.                                                 #
#                                                                    #
# In addition to unadjusted dataset, the script currently creates    #
# datasets adjusted for:                                             #
# nas201tran (Age 20 AFQT)                                           #
#                                                                    #
######################################################################

# Import libraries
library(dplyr)

# Create vector of all variable names for which  practice effects are 
# calculated. These variables will be adjusted
varsV1 = c("zVisSpat","zMR1COR","zHFTOTCOR","zSTWKMem","zdsfraw","zdsbraw",
            "zlntot","zsspfraw","zsspbraw","zrsatottran","zEpsMem","zcvatot",
            "zcvsdfr","zcvldfr","zlmitot","zlmdtot","zvritot","zvrdtot",
            "zAbsReason","zMTXTRAN","zVerbFlu","zlfcor","zcfcor",
            "zExecTrailsSwitch","ztrl4adjtran","zProcSpeed","zstrwraw",
            "zstrcraw","ztrl2tran","ztrl3tran","zExecCategorySwitch",
            "zCSSACCADJ","zExecInhibit","zstrit","zafqtpcttran","zafqtvocpcttran",
            "zafqtarpcttran","zafqttlpcttran","zafqtbxpcttran")

# rawVarsV1 = c("MR1COR","HFTOTCOR","MTXTRAN","dsfraw","dsbraw","lntot","sspfraw",
#             "sspbraw","RSATOTrevtran","cvatot","CVSDFR","CVLDFR","lmitot","lmdtot",
#             "vritot","vrdtot","LFCOR","CFCOR","strwraw","strcraw","TRL2TRAN","TRL3TRAN",
#             "TRL4ADJTRAN","CSSACCADJ","STRIT")
# 
# rawVarsV2 = c("MR1COR_v2","HFTOTCOR_V2","MTXTRAN_v2","dsfraw_V2","dsbraw_V2","lntot_V2",
#             "sspfraw_V2","sspbraw_V2","RSATOTTRAN_V2","cvatot_v2","CVSDFR_v2",
#             "CVLDFR_v2","lmitot_V2","lmdtot_V2","vritot_V2","vrdtot_V2","LFCOR_V2",
#             "CFCOR_V2","strwraw_V2","strcraw_V2","TRL2TRAN_v2","TRL3TRAN_v2",
#             "TRL4ADJTRAN_v2","CSSACCADJ_v2","strit_V2")



varsV1
varsV2 = paste0(varsV1,"_v2")
allVars = c(varsV1, varsV2)

#---------------------------#
# Create unadjusted dataset #
#---------------------------#

# Load demographic data
demoData = read.csv("K:/data/VETSA_demo_vars.csv", stringsAsFactors = F)

# Load VETSA 1 Cognitive Domain data
vetsa1CogDomains = read.csv("K:/Projects/PracticeEffects/data/V1_CognitiveDomains.csv", stringsAsFactors = F)

# Rename zrsatotrevtran variable in V1 to match variable name in V2
vetsa1CogDomains = dplyr::rename(vetsa1CogDomains, zrsatottran=zrsatotrevtran)
# Remove zVerbal and zvoctran. These were not collected at time 2
vetsa1CogDomains = vetsa1CogDomains %>% dplyr::select(-one_of(c("zVerbal","zvoctran")))

# Load VETSA 2 Cognitive Domain data
vetsa2CogDomains = read.csv("K:/Projects/PracticeEffects/data/V2_CognitiveDomains.csv", stringsAsFactors = F)

# Load VETSA 1 AFQT data
vetsa1afqt = read.csv("K:/Projects/PracticeEffects/data/V1_AFQTscores.csv",
                      stringsAsFactors = F)
# Load VETSA 2 AFQT data
vetsa2afqt = read.csv("K:/Projects/PracticeEffects/data/V2_AFQTscores.csv",
                      stringsAsFactors = F)

# Join demographic data with VETSA1 & VETSA2 datasets
allData = demoData %>%
  left_join(vetsa1CogDomains, by="vetsaid") %>% 
  left_join(vetsa2CogDomains, by="vetsaid") %>%
  left_join(vetsa1afqt, by="vetsaid") %>%
  left_join(vetsa2afqt, by="vetsaid")

# Select subjects from groups of interest
allData = allData %>%
  filter(VETSAGRP=="V1V2" | VETSAGRP=="V1" | VETSAGRP=="V2AR")

# Write out unadjusted dataset
write.csv(allData,"K:/Projects/PracticeEffects/data/PracEffectData_Unadj.csv", 
          row.names=F)

#-------------------------------------------------------#
# Create function to adjusted dataset by regressing out #
# specified variables                                   #
#-------------------------------------------------------#

adjustDataset = function(adjVars,data){
  #####################################################################
  # Adjust dataset for specified set of variables.Regresses passed    #
  # variables from each measure using mixed effects models. Residuals #
  # are scaled to 0 mean and unit standard deviation.                 #
  # Input:                                                            #
  # adjVars = List of variables to regress out                        #
  #####################################################################
  
  # Read variable names from data and store in a vector
  allNames <- names(data)
  
  #*** Check variables are correct
  allVars
  nVars <- length(allVars)	
  
  ### Creating Storate Data Frame ###
  
  # Set number of individuals 
  n <- dim(data)[1]
  tot <- dim(data)[2]
  
  # Create Data Frame
  data <- cbind(data,matrix(NA,nrow=n,ncol=nVars))
  names(data) <- c(allNames,paste(allVars,"_adj",sep=""))
  
  ### Running Loop Using lapply ###
  
  # fitting models
  models <- lapply(allVars, function(x) {
    fmla = as.formula(paste0(x," ~ 0 + ",adjVars," + (1|case)"))
    lmer(formula=fmla, data = data, na.action=na.exclude)
  })
  
  # storing residuals from each model into data frame
  for(v in 1:nVars){
    data[,tot+v] <- residuals(models[[v]], scale=TRUE)
  }
  
  #dataR is now your residualized parameters

  dataR <- data[,c(1:16,(tot+1):(tot+nVars))]
  dataR
}


#------------------------------------------------------#
# Create dataset adjusted for nas201tran (Age 20 AFQT) #
#------------------------------------------------------#

# Filter out subjects missing variable to be regressed out
data = subset(allData, !is.na("nas201tran"))

# Specify nas201tran (Age 20 AFQT as variable to regress out)
adjVars = paste("nas201tran", sep=" + ")

# Regress nas201tran out of dataset
nasAdjData = adjustDataset(adjVars, data)

# Save out adjusted dataset
write.csv(nasAdjData, 
          "K:/Projects/PracticeEffects/data/PracEffectData_nas201tranAdj.csv",
          row.names = FALSE)
