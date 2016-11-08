require(acss)
maxKs <- read.csv("./maxKs.csv")
maxKs$X <- NULL

base <- 2

stringsVector <- c("000110100111","111001011000")

acss(stringsVector, 2)

stringCounts <- as.data.frame(table(stringsVector))

stringCounts["ks"] <- acss(as.vector(stringCounts[["stringsVector"]]), base)[, 1]

naIndices <- as.integer(which(is.na(stringCounts$ks)))

naStrings <- as.vector(stringCounts$stringsVector[naIndices])

naLengths <- unlist(lapply(naStrings, nchar))

# to do: test NA strings with different lengths, these should appear
# in bases different of 2 
naKs <- maxKs[naLengths, paste0("K.", toString(base))] + 1 

stringCounts[is.na(stringCounts)] <- naKs