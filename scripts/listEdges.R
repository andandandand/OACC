listEdges <- function(gra){
  
  edgeList <- as_edgelist(gra, names = TRUE)
  
  printableEdgeList <- character(nrow(edgeList))
  
  if (nrow(edgeList) > 0){ 
    for (i in 1:nrow(edgeList)) {
      
      printableEdgeList[i] <- paste0(edgeList[i,1],
                                     "|", 
                                     edgeList[i, 2])
      
    }
    
    return(printableEdgeList)
  }
  return("no links")
}