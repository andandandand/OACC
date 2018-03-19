

library(acss)

maxKnownKs <- read.csv("data/maxKnownKs.csv")
maxKnownKs$X <- NULL

ldData <- read.csv("data/logicalDepthsBinaryStrings.csv", 
                   colClasses = c('character',"numeric"))

colnames(ldData)        <- c('string','ld')
logicalDepths           <- data.frame(ldData$ld)
rownames(logicalDepths) <- ldData$string


countSymbols <- function(string){
  return(length(table(strsplit(string, NULL))))
}

number2binary <- function(number, noBits) {
  
  binary_vector = rev(as.numeric(intToBits(number)))
  
  if(missing(noBits)) {
  
      return(binary_vector)
  } 
  else {
  
      binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}

getBinString <-function(string) {
  
  string <- utf8ToInt(string)

  bitList <- lapply(string, number2binary, noBits=8)
  
  bitList2 <- lapply(bitList, paste0, collapse="")
  
  binString <- paste0(bitList2, collapse="")
  
  return (binString)
}

splitString <- function(string, blockSize, offset){
  
  if(blockSize > nchar(string)){
    return (string)
  }
  
  if(offset > blockSize){
    return ("ERROR: offset cannot be greater than blockSize.")
  }
  
  subs <- character()
  startIndices <- seq(1, nchar(string), offset)
  
  for(i in startIndices){
    
    first <- i
    
    last <- -1
    
    if (last > nchar(string)){
      last <- nchar(string) -1
    } else{
      last <- i + blockSize -1  
    }
    
    sub <- substr(string, first, last)
    subs <- append(subs, sub)
    
    lastStep = FALSE
    if (nchar(sub) == blockSize && last == nchar(string)){
      lastStep = TRUE
    }
    if(lastStep)break
  }
  return (subs)
}

#receives the already splitted vector of input strings
stringBDM <- function (stringsVector, base) {
  
  stringCounts <- as.data.frame(table(stringsVector))
  
  #complexities
  
  stringCounts["ks"] <- acss(as.vector(stringCounts[["stringsVector"]]), base)[, 1]
  
  naIndices <- as.integer(which(is.na(stringCounts$ks)))
  
  naStrings <- as.vector(stringCounts$stringsVector[naIndices])
  
  naLengths <- unlist(lapply(naStrings, nchar))
  
  # more complex (+1) than the highest known values 
  naKs <- maxKnownKs[, paste0("K.", toString(base))] + 1
    
  stringCounts[is.na(stringCounts)] <- naKs
  
  bdm <- sum(log2(stringCounts$Freq)) + sum(stringCounts$ks)
  
  return(bdm) 
  
}

#receives the already splitted vector of input strings
stringBDMLD <- function (stringsVector, base) {
  
  stringCounts <- as.data.frame(table(stringsVector))
  
  #stringCounts
  sum(logicalDepths[as.character(stringCounts$stringsVector), ] * (log2(stringCounts$Freq)+1))
}

# ## should print 80.12 bits
#testBDM <- stringBDM(c("000110100111","111001011000"),2)
#testBDM

# ## should print 1002 steps
# testLD <- stringBDMLD(c("000110100111","111001011000"),2)
# testLD

# ##should print 31 * (log2(3) + 1) = 80.13 steps
#testLD2 <- stringBDMLD(c("010101010101", "010101010101", "010101010101"),2)
#testLD2 
