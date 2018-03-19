require(acss)
require(stringr)
source("BDM1D.R")


#1) Minimal Algorithmic Loss Data Reduction:
  
# Input: strings or arrays
# User parameters: data target size, recursive reevaluation, network (yes/no)
# Output: reduced string/array

# Method: Calculation of neutral elements, as we do for the nodes 
# and edges of networks 
# in the latest addition to the online algorithmic complexity calculator. 
# We then calculate the algorithmic information contribution of each bit by taking the difference of BDMs 
# (original versus every bit deleted), then we delete the neutral elements one by one until reaching desired reduced size. 
# Absolute neutrality means we remove elements near to 0 contribution first, relative neutrality means we remove elements 
# closest to the median of all values first. 
# Recursive reevaluation means recalculating BDM for every mutated bit after every deletion 
# (this process is of exponential time complexity and so only available for objects of, say, 1K bits).

#testString1 = "111" 
#testString1 = "10101010101010101"
testString1 = "110001101010111101"

evaluateBDM1D <- function(string, blockSize, offset, base){
  
  splittedString <- splitString(string, blockSize, offset)
  
  bdm <- stringBDM(splittedString, base)
  
  return(bdm)
}


#missing: the splitting of the string passed to stringBDM  
origStringBDMValue <- evaluateBDM1D(testString1, 12, 1, 2)

stringVector <- unlist(str_split(testString1, pattern=""))

deletionStrings <- c()
for(i in 1 :(length(stringVector))){

  boolIndexVector <- !logical(length(stringVector))
  boolIndexVector[i] <- FALSE
  back <- paste(stringVector[boolIndexVector], sep="", collapse="")
  deletionStrings <- c(deletionStrings, back)
}

deletionStrings

#missing: the splitting of the strings passed to stringBDM
deletionStringsBDMValues <- unlist(lapply(deletionStrings, evaluateBDM1D, 12, 1, 2))
median(deletionStringsBDMValues)
max(deletionStringsBDMValues)
min(deletionStringsBDMValues)
sort(deletionStringsBDMValues)

bdmDifferencesToOrig <- origStringBDMValue - deletionStringsBDMValues
bdmDifferencesToOrig

bdmDifferencesToMedian <- (median(deletionStringsBDMValues) - 
                             deletionStringsBDMValues)

bdmDifferencesToMedian

bdmDf <- data.frame(deletionStrings= deletionStrings,
                    bdmDifferencesToOrig = bdmDifferencesToOrig, 
                    stringsAsFactors=FALSE) 

 bdmDf$diffRankToOrig <- rank(
  -as.numeric(bdmDf$bdmDifferencesToOrig), ties.method ="min"
)
 
bdmDf



