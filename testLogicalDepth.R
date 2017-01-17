ldData <- read.csv("logicalDepthsBinaryStrings.csv", 
                          colClasses = c('character',"numeric"))

 colnames(ldData) <- c('string','ld')
 
 logicalDepths <- data.frame(ldData$ld)
 
 rownames(logicalDepths) <- ldData$string
 
 logicalDepths[c("01", "00"),]
 
 logicalDepths[c("111001011000","000110100111"),]
 
 max(logicalDepths$ld)
 
 t <- logicalDepths[c("111001011000","000110100111"),] 
 r <- t * c(1,1)
 r