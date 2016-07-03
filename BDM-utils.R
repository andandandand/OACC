

library(acss)

# http://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location
# set wd to this file location
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

maxKs <- read.csv("./maxKs.csv")

countSymbols <- function(string){
  return(length(table(strsplit(string, NULL))))
}

#http://stackoverflow.com/a/12088263/45963
number2binary <- function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
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

stringBDM <- function (stringsVector, base) {
  
  
  #tally blocks
  stringCounts <- table(stringsVector)
  
  counts <- as.data.frame(stringCounts)[,2]
  
  #complexities
  ks     <- acss(names(stringCounts), base)[,1]
  
  bdm <- sum(log2(counts) + ks)
  
  return(bdm)  
}

#uses acss_data and assigns max complexity to NAs
# test with "000110100111" and "111001011000"
stringBDMv2 <- function (stringsVector, base) {
  
  #StackOverflow question: getting the index of NA elements in a vectorized function
  
  #tally blocks
  stringCounts <- table(stringsVector)
  
  counts <- as.data.frame(stringCounts)[,2]
  
  #complexities
  ks     <- acss(names(stringCounts), base)[,1]
  
  bdm <- sum(log2(counts) + ks)
  
  return(bdm)  
}

#should return NA
stringBDMv2(c("000110100111","111001011000", "0110"),2)


