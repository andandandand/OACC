library(shiny)

shinyUI(
  fluidPage(
    
    #titlePanel("The Online Algorithmic Complexity Calculator"),
    
   
    sidebarLayout(
      
      column(6,
             tabsetPanel(
               tabPanel("BDM 1D",
                        value = 1, 
                        h3("Block Decomposition Method for Strings"),
                       
                        wellPanel( 
                          
                          textInput(inputId = "bdmInputString",
                                    label = "Enter a string",
                                    value ="010101010101010101010101010101010101",
                                    width ="800px"),
                          
                          sliderInput(inputId = "blockSize",
                                      label = "Block size",
                                      min = 2, max = 12, value = 8, step = 1),
                          
                          #max becomes the current value of blockSize -1
                          #dynamically
                          sliderInput(inputId = "blockOverlap",
                                      label = "Block overlap",
                                      min = 0, max = 11, value = 0, step = 1),
                          
                          radioButtons(inputId = "bdmAlphabet",
                                       label = "Number of possible symbols",
                                       inline = TRUE,
                                       choices = list("2" = 2,
                                                      "4" = 4,
                                                      "5" = 5,
                                                      "6" = 6,
                                                      "9" = 9,
                                                      "256 (utf-8)" = 256),
                                       selected = 2),
                          
                          br(),
                          actionButton("goButton", "Evaluate")
                          
                        )
                        
               ), 
               
               tabPanel("BDM 2D",
                        value = 2,
                        h3(
                          "Block Decomposition Method for Unweighted Graphs"), 
                        wellPanel(
                          fileInput(inputId = 'file1',
                                    label = "Choose a CSV file",
                                    accept = c('text/comma-separated-values', 
                                             'text/plain', 
                                             'text/csv', '.csv')
                                    ),
                          
                          radioButtons(inputId = 'bdm2DBlockSize',
                                       label = 'Block size',
                                       choices = c('4 x 4' = 4,
                                                   '3 x 3' = 3), 
                                       selected = 4),
                          
                          sliderInput(inputId = 'bdm2DOverlap',
                                      label = "Block overlap (rows and columns)",
                                      min = 0,
                                      max = 3,
                                      step = 1,
                                      value = 0),
                          
                          actionButton("goButtonBDM2D", "Evaluate")
                          
                                    
                          
                        ) #end wellPanel BDM 2D
               ), #end tabPanel BDM 2D
               
               tabPanel("CTM",
                        value = 3,
                        h3("Algorithmic Complexity for Short Strings"),
                        
                        wellPanel(
                          
                          
                          textInput(inputId = "ctmInputStrings",
                                    label = "Strings to evaluate",
                                    value ="AAAAAAAAAA ATATATATAT ATGCCGGCCT",
                                    width = "800px")
                          
                          ,
                          
                          p("Evaluate the complexity of strings with length < 13. Use space to separate them."),
                          
                          radioButtons(inputId = "ctmAlphabet",
                                       label = "Number of possible symbols", 
                                       inline = TRUE,
                                       choices = list("2" = 2, "4" = 4, "5" = 5, "6" = 6, "9" = 9),
                                       selected = 4), 
                          
                          selectInput("funct", 
                                      label = "Function used to evaluate the strings",
                                      choices = list("CTM Kolmogorov complexity estimated by algorithmic probability " = "acss",
                                                     "Shannon entropy" = "entropy",
                                                     "Second order entropy" = "entropy2",
                                                     "Compression length by gzip"="compression-gzip",
                                                     "Compression length by bzip2" = "compression-bzip2",
                                                     "Compression length by xz" = "compression-xz",
                                                     "Likelihood of production by Turing machines (deterministic process)" = "likelihood_d",
                                                     "Likelihood of production by Turing machines (random process)" = "likelihood_ratio",
                                                     "Conditional probability of random appearance" = "prob_random"
                                      ), 
                                      selected = "acss"),
                          
                          
                          actionButton("goButtonCTM", "Evaluate")
                          
                        )), # end wellPanel
               id = "conditionedPanels"
             )
      ),
      
      mainPanel(
        withMathJax(),
        conditionalPanel(condition="input.conditionedPanels==1",
                         h3("Result of BDM Evaluation"),
                         textOutput("resultBDM"),
                         br(),
                         textOutput("symbolCount"),
                        
                         br(),
                         HTML("<p><strong> \\(K_{BDM} =
                              \\sum_{i=1}^{n} CTM(block_{i})+log_{2}(|block_{i}|)\\) </strong></p>")
        ),
        conditionalPanel(condition="input.conditionedPanels==2",
                         h3("Result of 2D BDM Evaluation"),
                         tableOutput("loadedGraph"),
                         textOutput("resultBDM2D"),
                         br(),
                         textOutput("result2DEntropyAndCompLength"),
                         br(),
                         HTML("<p><strong> \\(K_{BDM} =
                              \\sum_{i=1}^{n} CTM(block_{i})+log_{2}(|block_{i}|)\\) </strong></p>")
        ), 
        conditionalPanel(condition="input.conditionedPanels==3",
                         tableOutput("resultCTM"), 
                         br(),
                         
                         HTML("<p><strong>\\(CTM~\\)\\(K_{alphabetSize} =
                              -log_{2}(D_{alphabetSize})\\) </strong></p>"),
                         
                         
                         p("\\(CTM~\\)\\(K_{alphabetSize}\\)
                           indicates the estimated Kolmogorov complexity of the string by the Coding Theorem Method."),
                         
                         p("\\(D_{alphabetSize}\\) indicates the estimated algorithmic probability, 
                           which is the output frequency of the string 
                           by small Turing machines with the same alphabet size."),
                         
                         br(),
                         HTML("More information on the other complexity functions is available in the "),
                         a(href="https://cran.r-project.org/web/packages/acss/acss.pdf", 
                           "documentation of the ACSS package @ CRAN."))
                         ) 
      
      )
))