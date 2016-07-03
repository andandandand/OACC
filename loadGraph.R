loadGraph <- function(dataPath, 
                      separator, 
                      quoteSymbol)
{
  
  loadedDf <- read.csv(dataPath,
                       header=FALSE,
                       sep=separator,
                       quote=quoteSymbol,
                       stringsAsFactors = FALSE,
                       check.names = FALSE)

  #selects numeric values, drops the rest
  loadedDf <- loadedDf[sapply(loadedDf, is.numeric)]
  
  rownames(loadedDf) <- colnames(loadedDf)
  loadedMat <- as.matrix(loadedDf)
  loadedMat
}

graph <- loadGraph("m88.csv", sep = ",", quote = '"' )
graph
