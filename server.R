

require("igraph")

source("scripts/BDM1D.R")
source("scripts/BDM2D.R")
source("scripts/compressionLength.R")
source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")
source("scripts/relabelTables.R")

source("scripts/listEdges.R")



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
      
      #call acss's CTM
      else if(input$funct == "acss"){
        
        z <- do.call(acss, 
                     args = list(string = unlist(strsplit(input$ctmInputStrings, " ")),
                                 alphabet = as.numeric(input$ctmAlphabet)))
        
        if (input$ctmAlphabet == "2"){
         colnames(z) <- c("K(5, 2)", "D(5, 2)")
        }
        else if (input$ctmAlphabet == "4"){
          colnames(z) <- c("K(4, 4)", "D(4, 4)")
        }
        else if (input$ctmAlphabet == "5"){
          colnames(z) <- c("K(4, 5)", "D(4, 5)")
        }
        else if (input$ctmAlphabet == "6"){
          colnames(z) <- c("K(4, 6)", "D(4, 6)")
        }
        else if (input$ctmAlphabet == "9"){
          colnames(z) <- c("K(4, 9)", "D(4, 9)")
        }
        
        
      }
      
      #call likelihood of production
      else{
        z <- do.call(input$funct, 
                     args = list(string = unlist(strsplit(input$ctmInputStrings, " ")),
                                 alphabet = as.numeric(input$ctmAlphabet)))
        if (!is.matrix(z)){
          z <- as.matrix(z)
          
          
          colnames(z) <- paste0(input$funct, ": ",
                                input$ctmAlphabet)
        }
      }
    })
    z
  }, rownames = TRUE, digits = 16)
  
  #### BDM 1D Tab
  
  output$evaluatedString <- renderText({
    
    input$goButtonBDM1D
    isolate({
      x <- paste0("Evaluated string = \"", 
                  input$bdmInputString, "\"")
    })
     x
  })
  

  #BDM 1D table of results
  output$resultBDMTable <- renderTable({
    
    input$goButtonBDM1D
    isolate({
      
      values <- c ()
      
      if (input$bdmAlphabet == 256){
        
        # convert UTF-8 string to binary
        binString <- getBinString(input$bdmInputString)
        
        values[1] <- paste0(
          sprintf("%.4f",stringBDM(
            splitString(binString,
                        blockSize = input$blockSize, 
                        offset = input$blockSize -input$blockOverlap),
            base = 2)), 
          " bits")
              
        values[2] <- paste0(
          sprintf("%.4f",stringBDMLD(
            splitString(binString,
                        blockSize = input$blockSize, 
                        offset = (input$blockSize -input$blockOverlap)),
            base = input$bdmAlphabet)), 
          " steps")
      }
      else {
      values[1] <- paste0(
                    sprintf("%.4f",stringBDM(
                      splitString(input$bdmInputString,
                                  blockSize = input$blockSize, 
                                  offset = input$blockSize -input$blockOverlap),
                                  base = input$bdmAlphabet)), 
                        " bits")
      }
      
      if (input$bdmAlphabet == 2){
      values[2] <- paste0(
        sprintf("%.4f",stringBDMLD(
          splitString(input$bdmInputString,
                      blockSize = input$blockSize, 
                      offset = input$blockSize -input$blockOverlap),
          base = input$bdmAlphabet)), 
        " steps")
      }
      #entropy
      values[3] <- paste0(sprintf("%.4f",
                                  entropy(input$bdmInputString)), 
                          " bit(s)")
      
      #second order entropy
      values[4] <- paste0(sprintf("%.4f",
                                  entropy2(input$bdmInputString)),
                          " bit(s)")
      
      #compression length
      values[5] <- paste0(compressionLength(input$bdmInputString, 
                                            "gzip") * 8, 
                          " bits")
      
      values[6] <- nchar(input$bdmInputString)
      values[7] <- countSymbols(input$bdmInputString)
      values[8] <- input$bdmAlphabet
      values[9] <- input$blockSize
      values[10] <- input$blockOverlap
      
      
      
      resultRowNames  <-  c("BDM algorithmic complexity estimation",
                            "BDM logical depth estimation",
                            "Shannon entropy", 
                            "Second order entropy",
                            "Compression length (using gzip)",
                            "String length",
                            "# of symbols in string", 
                            "# of symbols in CTM alphabet",
                            "Block size",
                            "Block overlap")
      
      if (!(input$bdmAlphabet == 2 || input$bdmAlphabet == 256))
      {
           values <- values[-2]
           resultRowNames <- resultRowNames[-2]
      } 
    
      result <- data.frame(values)
      rownames(result) <- resultRowNames
      
      
    }) 
    result}, 
    rownames = TRUE, colnames = FALSE)
  
  
  ##### BDM 2D Tab
  loadedGraph <- reactive({
    
    inFile <- input$file1
    if(input$goButtonBDM2D == 0 || is.null(inFile$datapath)){
      graph <- loadGraph("data/m88.csv", sep = ",", quote = '"' )
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
        
                sprintf("%.4f", 
                  bdm2D(loadedGraph(),
                      blockSize = as.numeric(input$bdm2DBlockSize),
                      offset = (as.numeric(input$bdm2DBlockSize) - 
                                as.numeric(input$bdm2DOverlap)) )), 
                   " bits")

      # Shannon entropy
      values[2] <- paste0(sprintf("%.4f",
                                  entropy(toString(loadedGraph()))[[1]]), 
                 " bit(s)")
      
      # Block entropy
      values[3] <- paste0(
        
        sprintf("%.4f", blockEntropy(loadedGraph(),
                              blockSize = as.numeric(input$bdm2DBlockSize),
                              offset = (as.numeric(input$bdm2DBlockSize) - 
                                          as.numeric(input$bdm2DOverlap)) )), 
        " bits")
      
      # compression length
      values[4] <- paste0(compressionLength(toString(loadedGraph()), "gzip") * 8,
                 " bits")
      
      # matrix dimensions
      values[5] <- paste0(nrow(loadedGraph()), " x ", ncol(loadedGraph()))

     
      #2D block size
      values[6] <- paste0(input$bdm2DBlockSize, " x ", input$bdm2DBlockSize)
      
      #2D block overlap
      values[7] <- input$bdm2DOverlap
      
      result <- data.frame(values)
      
      rownames(result) <- c("BDM algorithmic complexity estimation", 
                            "Shannon entropy",
                            "Block entropy",
                            "Compression length (using gzip)",
                            "Matrix  dimensions",
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
  
  ###### Perturbation Analysis Tab
  
  #runs once, when server starts
  g <- loadGraphPA("./data/m88.csv")
  pv <- calculatePerturbationByVertexDeletion(g, 4, 1)
  pe <- calculatePerturbationByEdgeDeletion(g, 4, 1)
  g  <- setGraphColors(g, pv, pe)
  
  #all of these must be cleared and updated when a new graph is loaded
  
  sizeOfHistory <- vcount(g) + ecount(g)

  graphHistory    <- list()

  pvHistory       <- list()
  
  peHistory       <- list()
  
  delEventHistory <- vector("list", sizeOfHistory)
  
  graphHistory[[1]]    <- g
  pvHistory[[1]]       <- relabelVertexTable(pv)
  peHistory[[1]]       <- relabelEdgeTable(pe)
  delEventHistory[[1]] <- "Initial state of the network"
  
  #starts with 1 even though the network hasn't been changed at the start
  perturbationCounter <- as.integer(1)
    
  my <- reactiveValues(g = g,
                       pv = pv,
                       pe = pe,
                       perturbationCounter = perturbationCounter, 
                       sizeOfHistory       = sizeOfHistory,
                       graphHistory        = graphHistory,
                       pvHistory           = pvHistory,
                       peHistory           = peHistory,
                       delEventHistory     = delEventHistory)
        
  observe({
    
    updateSelectInput(session, "vertexToDelete",
                      choices = V(my$g)$label)
    
    updateSelectInput(session, "edgeToDelete",
                      choices = listEdges(my$g))
    
  })
  
  observeEvent(input$file2, {
    
    inFile <- input$file2
    
    if (is.null(inFile$datapath)){
      
      
    } else {
      
      g <- loadGraphPA(inFile$datapath)
      my$pv <- calculatePerturbationByVertexDeletion(g, 4, 1)
      my$pe <- calculatePerturbationByEdgeDeletion(g, 4, 1)
      my$g  <- setGraphColors(g, my$pv, my$pe)
      
      my$sizeOfHistory <- vcount(my$g) + ecount(my$g)
   
      my$perturbationCounter <- as.integer(1)
      
      #todo: check if this vector("list", my$sizeOfHistory) works with list() in Report.Rmd
      graphHistory     <- vector("list", my$sizeOfHistory)
      ptHistory        <- vector("list", my$sizeOfHistory)
      pvHistory        <- vector("list", my$sizeOfHistory)
      delEventHistory  <- vector("list", my$sizeOfHistory) 
      
    }
    
  }, ignoreInit = FALSE) 
  # ignoreInit = FALSE is default behavior
  # these initializations run when the server is first set up
  
  observeEvent(input$goButtonDeleteVertex, {
    
    if(vcount(my$g) > 5){
      
      my$perturbationCounter <- my$perturbationCounter + 1
      
      
      
      my$g <- delete_vertices(my$g,
                              input$vertexToDelete)
      
      my$pv <- calculatePerturbationByVertexDeletion(my$g, 4, 1)
      my$pe <- calculatePerturbationByEdgeDeletion(my$g, 4, 1)
      
      my$g  <- setGraphColors(my$g, my$pv, my$pe)
      
      my$graphHistory[[my$perturbationCounter]] <- my$g
      my$pvHistory[[my$perturbationCounter]]    <- relabelVertexTable(my$pv)
      my$peHistory[[my$perturbationCounter]]    <- relabelEdgeTable(my$pe)
      
      my$delEventHistory[[my$perturbationCounter]] <- paste0("Deletion of node ", 
                                                             input$vertexToDelete)    
  
      
    }
    
    else {
      
      output$cantDeleteVertex <- renderText({
        
        "can't delete more nodes"
        
      })
      
    }
    
  })
  
  observeEvent (input$goButtonDeleteEdge, {
    
    if(ecount(my$g) > 1){
      
      my$g <- delete_edges(my$g,
                           input$edgeToDelete)
      
      my$pv <- calculatePerturbationByVertexDeletion(my$g, 4, 1)
      my$pe <- calculatePerturbationByEdgeDeletion(my$g, 4, 1)
      
      my$g  <- setGraphColors(my$g, my$pv, my$pe)
      
      my$perturbationCounter <- my$perturbationCounter + 1
      
      my$graphHistory[[my$perturbationCounter]] <- my$g
      my$pvHistory[[my$perturbationCounter]]    <- relabelVertexTable(my$pv)
      my$peHistory[[my$perturbationCounter]]    <- relabelEdgeTable(my$pe)
      
      
      my$delEventHistory[[my$perturbationCounter]] <- paste0("Deletion of link ", 
                                                             input$edgeToDelete) 
      
    } else {
      
      output$cantDeleteLink <- renderText("can't delete more links")
      
    }
    
  })
  
  output$graphPlot <- renderPlot({
    
    coords <- layout_(my$g, as_star())
    
    plot(my$g,
         layout = coords,
         edge.arrow.size = 0.4,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    
  })
  
  output$perturbationTable <- renderTable ({
    
    if(input$printTable == "edges"){
      edgeTable <- relabelEdgeTable(my$pe)
      return(edgeTable)
    }
    else {
      
      vertexTable <- relabelVertexTable(my$pv)
      
      return(vertexTable)
      
    }
    
  }, digits = 3)
  
  
  ### downloadHandler for HTML report
  
  output$report <- downloadHandler (
    
    filename = "report.html",
    
    content = function(file){
      
      tempReport <- file.path(tempdir(),
                              "report.Rmd")
      
      file.copy("report.Rmd", tempReport,
                overwrite = TRUE)
      
      printG <- my$g
      
      printVT <- relabelVertexTable(my$pv)
      
      printET <- relabelEdgeTable(my$pe)

      
      params <- list(graphHistory = my$graphHistory,
                     pvHistory = my$pvHistory,
                     peHistory = my$peHistory,
                     perturbationCounter = my$perturbationCounter,
                     delEventHistory = my$delEventHistory)
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(globalenv()))
            
    }
    
  )
  
})#end shinyServer function



