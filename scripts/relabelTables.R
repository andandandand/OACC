relabelVertexTable <- function(oldVertexTable){
  
  vertexTable <- oldVertexTable
  
  vertexTable$name <- NULL  
  vertexTable$color <- NULL
  
  #print(vertexTable)
  
  vertexTable$bdmIncrease[vertexTable$bdmIncrease] <- "negative"
  vertexTable$bdmIncrease[vertexTable$bdmIncrease == FALSE] <- "positive"
  
  colnames(vertexTable) <- c("Node label",
                             "BDM of graph after node knockout",
                             "BDM diff",
                             "Information shift",
                             "Perturbation rank")
  
  return(vertexTable)
  
}

relabelEdgeTable <- function(oldEdgeTable){
  
  edgeTable <- oldEdgeTable
  
  edgeTable$color <- NULL
  
  edgeTable$bdmIncrease[edgeTable$bdmIncrease] <- "negative"
  edgeTable$bdmIncrease[edgeTable$bdmIncrease == FALSE] <- "positive"
  colnames(edgeTable) <- c("Link origin",
                           "Link end",
                           "BDM value after link knockout",
                           "BDM diff",
                           "Information shift",
                           "Perturbation rank")
  return (edgeTable)
}