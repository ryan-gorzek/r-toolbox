
contSplitTrainTest <- function(dataSize,nFolds,bfrLen,currFold) {

  foldSize = floor(dataSize/nFolds)
  
  foldIdx = seq(from = 1, to = dataSize, by = foldSize)

  # get test indices
  if (currFold != nFolds) {
    testIdx = c(foldIdx[currFold]:(foldIdx[currFold+1]-1))
  }
  else {
    testIdx = c(foldIdx[currFold]:dataSize)
    if (length(testIdx) > foldSize) {
      sizeDiff = length(testIdx) - foldSize
      testIdx = testIdx[1:(length(testIdx)-sizeDiff)]
    }
    
  }

  # get train indices and exclude buffer region
  if (currFold == 1) {
    trainIdx = c((testIdx[length(testIdx)]+bfrLen+1):dataSize)
  }
  else if (currFold == nFolds) {
    trainIdx = c(1:(testIdx[1]-bfrLen-1))
  }
  else {
    trainIdx = c(c(1:(testIdx[1]-(bfrLen)-1)),c((testIdx[length(testIdx)]+bfrLen+1):dataSize))
  }

  return(list(trainIdx,testIdx))

}
