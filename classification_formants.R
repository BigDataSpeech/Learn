predictors = c("F1", "F2")
prediction = "voyelle"
datafile = "acoustique_voy_orales_20loc_ESTER_NCCFr_contexte_freqLex_distCentroide.txt"

datasetFull = read.table(file = datafile, sep="\t", header = T, fileEncoding = "UTF-8", quote= "", comment.char = "")

dataset = datasetFull[datasetFull$corpus=="ESTER" & datasetFull$dureeMs>=60,]

# Normalisation
for (iPredictor in 1:length(predictors)) {
  dataset[,predictors[iPredictor]] = scale(dataset[,predictors[iPredictor]])
}

ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.67, 0.33))
model.training <- dataset[ind==1, predictors]
model.test <- dataset[ind==2, predictors]
model.trainLabels <- dataset[ind==1, prediction]
model.testLabels <- dataset[ind==2, prediction]
modelTestLabels <- data.frame(model.testLabels)
model_pred <- knn(train = model.training, test = model.test, cl = model.trainLabels, k=3)

library(gmodels)
matConf = CrossTable(x = model.testLabels, y = model_pred, prop.chisq=FALSE, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE)

modelTestLabels <- data.frame(model.testLabels)
mergedDataFrame <- data.frame(model_pred, model.testLabels)
names(mergedDataFrame) <- c("Predicted Species", "Observed Species")
mergedDataFrame$correct = (mergedDataFrame$`Predicted Species`==mergedDataFrame$`Observed Species`)
correctClassifRate = nrow(mergedDataFrame[mergedDataFrame$correct==T,]) / nrow(mergedDataFrame)
print(correctClassifRate)
prop.table(table(mergedDataFrame$`Observed Species`, mergedDataFrame$correct), margin=1)
