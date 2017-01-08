
source("BDM-utils.R")
source("loadGraph.R")
source("BDM2D.R")
source("compressionLength.R")

shinyServer(function(input, output, session) {
  
  #updates the slider for overlap in BDM 1D dynamically
  observeEvent(input$blockSize, {
    updateSliderInput(session, 
                      "blockOverlap", 
                      max=input$blockSize-1)
  })
  
  
  output$symbolCount <- renderText({
    
    input$goButton
    
    y <- isolate(
      paste0(
        'The string has length ', 
        nchar(input$bdmInputString),
        ' and contains ',  
        countSymbols(input$bdmInputString), 
        ' different symbols. \n It has Shannon entropy = ', 
        entropy(input$bdmInputString), 
        ' bit(s), and compressed length in bytes = ',
        compressionLength(input$bdmInputString, "gzip"), 
        ' (using gzip)'))
  })
  
  ### CTM Tab
  output$resultCTM <- renderTable({
    input$goButtonCTM
    isolate({
      
      if(input$funct == "entropy"){
        strings <- unlist(strsplit(input$ctmInputStrings, " "))
        z <- lapply(strings, entropy)
        if (!is.matrix(z)) {
          z <- as.matrix(z)
          colnames(z) <- "Shannon entropy"
          rownames(z) <- strings
        }
      }
      
      else if(input$funct == "entropy2"){
        
        strings <- unlist(strsplit(input$ctmInputStrings, " "))
        z <- lapply(strings, entropy2)
        if (!is.matrix(z)) {
          z <- as.matrix(z)
          colnames(z) <- "Second order entropy"
          rownames(z) <- strings
        }
      }
      
      else if(input$funct == "compression-gzip"){
        
        strings <- unlist(strsplit(input$ctmInputStrings, " "))
        
        z <- lapply(strings,
                    compressionLength,
                    compressionType = "gzip")
        
        if (!is.matrix(z)) {
          z <- as.matrix(z)
          colnames(z) <- "Compression length (bytes)"
          rownames(z) <- strings
        } 
        
      }
      
      else if(input$funct == "compression-bzip2"){
        
        strings <- unlist(strsplit(input$ctmInputStrings, " "))
        
        z <- lapply(strings,
                    compressionLength,
                    compressionType = "bzip2")
        
        if (!is.matrix(z)) {
          z <- as.matrix(z)
          colnames(z) <- "Compression length (bytes)"
          rownames(z) <- strings
        } 
        
      }
      
      else if(input$funct == "compression-xz"){
        
        strings <- unlist(strsplit(input$ctmInputStrings, " "))
        
        z <- lapply(strings,
                    compressionLength,
                    compressionType = "xz")
        
        if (!is.matrix(z)) {
          z <- as.matrix(z)
          colnames(z) <- "Compression length (bytes)"
          rownames(z) <- strings
        } 
        
      }
      
      else{
        z <- do.call(input$funct, 
                     args = list(string = unlist(strsplit(input$ctmInputStrings, " ")),
                                 alphabet = as.numeric(input$ctmAlphabet)))
        if (!is.matrix(z)){
          z <- as.matrix(z)
          colnames(z) <- paste0(input$funct, ": ", input$ctmAlphabet)
        }
      }
    })
    z
  }, digits = 16)
  
  #### BDM 1D Tab
  
  output$evaluatedString <- renderText({
    
    input$goButtonBDM1D
    isolate({
      x <- paste0("Evaluated string = \"", input$bdmInputString, "\"")
    })
     x
  })
  

  #BDM 1D table of results
  output$resultBDMTable <- renderTable({
    
    input$goButtonBDM1D
    isolate({
      
      values <- c ()
    
      if (input$bdmAlphabet == 256){
        
        # convert UTF-8 strings to binary
        binString <- getBinString(input$bdmInputString)
        
        values[1] <- paste0(
          sprintf("%.2f",stringBDM(
            splitString(binString,
                        blockSize = input$blockSize, 
                        offset = input$blockSize -input$blockOverlap),
            base = 2)), 
          " bits")
      }
      else {
      values[1] <- paste0(
                        sprintf("%.2f",stringBDM(
                        splitString(input$bdmInputString,
                                      blockSize = input$blockSize, 
                                      offset = input$blockSize -input$blockOverlap),
                                    base = input$bdmAlphabet)), 
                        " bits")
      }
      #entropy
      values[2] <- paste0(sprintf("%.2f",entropy(input$bdmInputString)), " bit(s)")
      
      #second order entropy
      values[3] <- paste0(sprintf("%.2f",entropy2(input$bdmInputString)), " bit(s)")
      
      #compression length
      values[4] <- paste0(compressionLength(input$bdmInputString, "gzip") * 8, " bits")
      
      values[5] <- nchar(input$bdmInputString)
      values[6] <- countSymbols(input$bdmInputString)
      values[7] <- input$bdmAlphabet
      values[8] <- input$blockSize
      values[9] <- input$blockOverlap
      
      result <- data.frame(values)
      
      rownames(result) <- c("BDM", 
                            "Shannon entropy", 
                            "Second order entropy",
                            "Compression length (using gzip)",
                            "String length",
                            "# of symbols in string", 
                            "# of symbols in CTM alphabet",
                            "Block size",
                            "Block overlap")
      
      
    }) 
    result}, 
    rownames = TRUE, colnames = FALSE)
  
  
  #####BDM 2D Tab
  loadedGraph <- reactive({
    
    inFile <- input$file1
    if(input$goButtonBDM2D == 0 || is.null(inFile$datapath)){
      graph <- loadGraph("m88.csv", sep = ",", quote = '"' )
      return (graph)
    }
    
    graph <- loadGraph(inFile$datapath, 
                       sep = ",", 
                       quote = '"')
    
    graph
  })
  
  #render adjacency matrix as a dataframe
  output$loadedGraph <- renderTable({
    
    loadedGraph()
    
  })
  
  output$resultBDM2DTable <- renderTable({

    input$goButtonBDM2D
    isolate({
      
      #BDM2D
      values <- c ()
    
      
      values[1] <- paste0(
        
        sprintf("%.2f", bdm2D(loadedGraph(),
              blockSize = as.numeric(input$bdm2DBlockSize),
              offset = (as.numeric(input$bdm2DBlockSize) - 
                          as.numeric(input$bdm2DOverlap)) )), 
          " bits")

      # Shannon entropy
      values[2] <- paste0(sprintf("%.2f",entropy(toString(loadedGraph()))[[1]]), 
                 " bit(s)")
      
      # compression length
      values[3] <- paste0(compressionLength(toString(loadedGraph()), "gzip") * 8,
                 " bits")
      
      # matrix dimensions
      values[4] <- paste0(nrow(loadedGraph()), " x ", ncol(loadedGraph()))

      # the number of symbols for the BDM2D alphabet is always 2
      values[5] <- 2
      
      #2D block size
      values[6] <- paste0(input$bdm2DBlockSize, " x ", input$bdm2DBlockSize)
      
      #2D block overlap
      values[7] <- input$bdm2DOverlap
      
      result <- data.frame(values)
      
      rownames(result) <- c("BDM", 
                            "Shannon entropy", 
                            "Compression length (using gzip)",
                            "Matrix  dimensions",
                      
                            "# of symbols in CTM alphabet",
                            "Block size",
                            "Block overlap (both rows and columns)")
      
      
    }) 
    result}, rownames = TRUE, colnames = FALSE)
  
  
  output$resultBDM2D <- renderText({
    
    input$goButtonBDM2D
    isolate({
      
      if(input$bdm2DOverlap >= input$bdm2DBlockSize){
        x <- "The maximum possible overlap is block size - 1."
      }
      else{
        x <- paste0('BDM of the evaluated adjacency matrix considering ',
                    input$bdm2DBlockSize, ' x ', 
                    input$bdm2DBlockSize, 
                    " blocks, with overlap of ", 
                    input$bdm2DOverlap,
                    ' rows and ',
                    input$bdm2DOverlap,
                    ' columns is \n',
                    bdm2D(loadedGraph(),
                          blockSize = as.numeric(input$bdm2DBlockSize),
                          offset = (as.numeric(input$bdm2DBlockSize) - 
                                      as.numeric(input$bdm2DOverlap)) ), 
                    
                    ' bit(s).'
                    )
           }
           x
         })
    })
    
  output$result2DEntropyAndCompLength <- renderText({
      
      input$goButtonBDM2D
      isolate({
        
        
      
          x <- paste0("The Shannon entropy of the adjacency matrix is ",
                      entropy(toString(loadedGraph()))[[1]], 
                      " bit(s), and its compressed length  is ", 
                      compressionLength(toString(loadedGraph()), "gzip"),
                      ' bytes (using gzip).'
          )
        
        x
      })
    
  })
  
  
})#end shinyServer function



