# Create dataset adjusted for practice effects
# Note: Only time 2 scores of longitudinal (V1V2) subjects are adjusted

peAdjDat = subsetDat
for (varName in allVars) {
  varName_V2 = paste0(varName, "_V2_adj")
  peAdjDat[idxV1V2, varName_V2] = peAdjDat[idxV1V2, varName_V2] - pracEffects[varName]
}

# Add back in V1ne subjects
peAdjDat = rbind(peAdjDat, V1neDat)

# Save out adjusted dataset
write.csv(peAdjDat, '/home/jelman/netshare/K/Projects/PracticeEffects/data/CogData_NAS201TRAN_PracEffect_Adj.csv',
          row.names=FALSE)