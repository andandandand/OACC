ldData <- read.csv("./logicalDepthsBinaryStrings.csv", 
                          colClasses = c('character',"numeric"))

 colnames(ldData) <- c('string','ld')
 
 logicalDepths <- data.frame(ldData$ld)
 
 rownames(logicalDepths) <- ldData$string
 
 logicalDepths[c("01", "00"),]
 
 logicalDepths[c("111001011000","000110100111"),]
 
 max(logicalDepths$ld)
 
 t1 <- logicalDepths[c("111001011000","000110100111"),] 
 t1
 
 t2 <-logicalDepths[c("010101010101", "010101010101", "010101010101"),] 
 t2