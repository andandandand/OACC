
source("BDM-utils.R")
source("loadGraph.R")
source("BDM2D.R")

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
        ' different symbols.' ))
  })
  
  output$resultCTM <- renderTable({
    input$goButtonCTM
    isolate({
      z <- do.call(input$funct, 
                   args = list(string = unlist(strsplit(input$ctmInputStrings, " ")),
                               alphabet = as.numeric(input$ctmAlphabet)))
      if (!is.matrix(z)){
        z <- as.matrix(z)
        colnames(z) <- paste0(input$funct, ": ", input$ctmAlphabet)
      }
    
    })
    z
  }, digits = 16)
  
  #### BDM 1D
  output$resultBDM <- renderText({
    
    input$goButton
    isolate({
      if(countSymbols(input$bdmInputString) > 9 || input$bdmAlphabet == 256) { 
        
        binString <- getBinString(input$bdmInputString)
        
        x <- paste0('Unnormalized K_BDM of the string `',
                    input$bdmInputString ,
                    '` \n with block size = ', 
                    input$blockSize,
                    ' and block overlap = ', 
                    input$blockOverlap,
                    ', using a UTF-8 to binary conversion of the string is ', 
                    stringBDM(
                      splitString(
                        binString,
                        blockSize = input$blockSize, 
                        offset = input$blockSize -input$blockOverlap),
                      base = 2)
        )
      } 
      else{
        
        x <- paste0('Unnormalized K_BDM of the string ',
                    input$bdmInputString ,
                    ' with block size = ', 
                    input$blockSize,
                    ' and block overlap = ', 
                    input$blockOverlap,
                    ', considering ',
                    input$bdmAlphabet,
                    ' possible symbols, is ', 
                    stringBDM(splitString(input$bdmInputString, 
                                          blockSize=input$blockSize, 
                                          offset= input$blockSize - input$blockOverlap), 
                              as.numeric(input$bdmAlphabet))
        )
      }
    }) #end isolate()
    x
  })
  
  #####BDM 2D
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
  
  output$resultBDM2D <- renderText({
    
    input$goButtonBDM2D
    isolate({
      
      if(input$bdm2DOverlap >= input$bdm2DBlockSize){
        x <- "The maximum possible overlap is block size - 1."
      }
      else{
        x <- paste0('Unnormalized K_BDM of the evaluated adjacency matrix considering ',
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
                                      as.numeric(input$bdm2DOverlap)) )
        )
      }
      x
    })
    
  })
  
  
})#end shinyServer function



