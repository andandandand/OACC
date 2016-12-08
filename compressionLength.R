compressionLength <- function(string, compressionType){
  len <- length(memCompress(string, compressionType))
  return(len)
}

## https://en.wikipedia.org/wiki/Gzip
#g <- compressionLength("0101010101010101", "gzip")
#g

## https://en.wikipedia.org/wiki/Bzip2 
# b <- compressionLength("0101010101010101", "bzip")
# b
# 

## https://en.wikipedia.org/wiki/Xz
# x <- compressionLength("0101010101010101", "xz")
# x

#testList <- c("00000000001", "0000000000", "010101010101")
#test <- lapply(testList, compressionLength, compressionType = "gzip")
#test

# if (!is.matrix(test)) {
#     test <- as.matrix(test)
#     colnames(test) <- "Compression length by gzip"
# }
# test
# 
# test2 <- "000000000000000000000000111111111000"
# len <- compressionLength(test2, "gzip")
# len
# gz1 <- gzfile("test2.gz", "w")
# write.csv(test2, gz1)
# close(gz1)