relabelVertexTable <- function(oldVertexTable){
  
  vertexTable <- oldVertexTable
  
  vertexTable$name <- NULL  
  vertexTable$color <- NULL
  
  #print(vertexTable)
  
  vertexTable$bdmIncrease[vertexTable$bdmIncrease] <- "positive"
  vertexTable$bdmIncrease[vertexTable$bdmIncrease == FALSE] <- "negative"
  
  colnames(vertexTable) <- c("Node label",
                             "BDM of graph after node knockout",
                             "BDM diff",
                             "Information value",
                             "Perturbation rank")
  
  return(vertexTable)
  
}

relabelEdgeTable <- function(oldEdgeTable){
  
  edgeTable <- oldEdgeTable
  
  edgeTable$color <- NULL
  
  edgeTable$bdmIncrease[edgeTable$bdmIncrease] <- "positive"
  edgeTable$bdmIncrease[edgeTable$bdmIncrease == FALSE] <- "negative"
  colnames(edgeTable) <- c("Link origin",
                           "Link end",
                           "BDM value after link knockout",
                           "BDM diff",
                           "Information value",
                           "Perturbation rank")
  return (edgeTable)
}