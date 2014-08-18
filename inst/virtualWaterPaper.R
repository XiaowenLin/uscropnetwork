require(uscropnetwork)

TradeTable07 <- TradeTable$new("data/stateLevel.csv")

VWCTable <- read.table("data/VWC.csv", header = T, sep = ',')
rownames(VWCTable) <- VWCTable$State
VWCTable <- VWCTable[,-1]
VWCTable <- as.matrix(VWCTable)
weightTable <- TradeTable07$weightTable
valueTable = TradeTable07$valueTable
SCTGs_ = c(1, 2, 4, 5, 6)
origins = TradeTable07$labels

cropTradeMatrix07 <- TradeMatrix$new(
  weightTable = TradeTable07$weightTable, 
  valueTable = TradeTable07$valueTable, 
  SCTGs_ = c(1, 2, 4, 5, 6), 
  origins = TradeTable07$labels, 
  VWCTable)

cropTradeMatrix07$analysis(cropTradeMatrix07$virtualMatrix, cropTradeMatrix07$labels, result.name="data/virtualResult.csv", index.name="data/virtualIndexes.csv", matrix.name="data/virtualMatrix.csv")
cropTradeMatrix07$generateCircos(cropTradeMatrix07$virtualMatrix)
cropTradeMatrix07$getNetworkLorenz(cropTradeMatrix07$virtualMatrix, filename="data/virtualGini.pdf")

colors <- c("r", "g", "b", "y", "o")
filenames <- c("data/animalVWCFinal.pdf", "data/cerealVWCFinal.pdf", "data/feedVWCFinal.pdf", "data/meatVWCFinal.pdf", "data/milledVWCFinal.pdf")
for (i in 1:5) {
  cropTradeMatrix07$heatMap(VWCTable[,i], rownames(VWCTable), colors[i], filenames[i])
}
