library(shiny)

shinyUI(
  fluidPage(
    
    #titlePanel("The Online Algorithmic Complexity Calculator"),
    
    sidebarLayout(
      
      column(6,
             tabsetPanel(
               tabPanel("For any string",
                        
                        value = 1, 
                        
                        h3("Block Decomposition Method for Strings"),
                       
                        div(wellPanel( 
                          
                          textInput(inputId = "bdmInputString",
                                    label = "Enter a string",
                                    value ="010101010101010101010101010101010101",
                                    width ="800px"),
                          
                          
                          sliderInput(inputId = "blockSize",
                                      label = "Block size",
                                      min = 2, max = 12, value = 12, step = 1),
                          
                          #max becomes the current value of blockSize -1
                          #dynamically
                          sliderInput(inputId = "blockOverlap",
                                      label = "Block overlap",
                                      min = 0, max = 11, value = 0, step = 1),
                          
                          radioButtons(inputId = "bdmAlphabet",
                                       label = "Alphabet size",
                                       inline = TRUE,
                                       choices = list("2" = 2,
                                                      "4" = 4,
                                                      "5" = 5,
                                                      "6" = 6,
                                                      "9" = 9,
                                                      "256 (utf-8)" = 256),
                                       selected = 2),
                          
                          br(),
                          actionButton("goButtonBDM1D", "Evaluate")
                          
                        ), style="font-size:115%")
                        
               ), 
               
               tabPanel("For binary arrays",
                        value = 2,
                        h3(
                          "Block Decomposition Method for Unweighted Adjacency Matrices"), 
                        div(wellPanel(
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
                        , style="font-size: 115%")
               ), #end tabPanel BDM 2D
               
               tabPanel("For short strings",
                        value = 3,
                        h3("Algorithmic Complexity for Short Strings"),
                        
                        div(wellPanel(
                          
                          
                          textInput(inputId = "ctmInputStrings",
                                    label = "Strings to evaluate",
                                    value ="AAAAAAAAAAAA ATATATATATAT ATTGCCGGCCTA",
                                    width = "800px")
                          
                          ,
                          
                          p("Use space to separate strings."),
                          p("The length of each string must be shorter than 13 characters."),
                          
                          
                          radioButtons(inputId = "ctmAlphabet",
                                       label = "Alphabet size", 
                                       inline = TRUE,
                                       choices = list("2" = 2, 
                                                      "4" = 4,
                                                      "5" = 5, 
                                                      "6" = 6, 
                                                      "9" = 9),
                                       selected = 4), 
                          
                          selectInput(#inputId="shortStringsEvalFunction", #"argument is not interpretable as logical" error
                                      inputId="funct", 
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
                          
                        )), # end wellPanel "For short strings", 
                        style="font-size:115%"), #end tabPanel "For short strings"
               
               tabPanel("Network perturbation", 
                        value = 4, 
                        h3("Perturbation Analysis of Unweighted Networks"),
                        div(wellPanel(
                          
                          fileInput(inputId = "file2",
                                    label = "Choose a CSV file",
                                    accept = c('text/comma-separated-values',
                                               'text/plain',
                                               'text/csv',
                                               '.csv')
                                    ),
                          
                          selectInput(inputId = "vertexToDelete",
                                      label = "Node to delete",
                                      choices = ""), # choices filled in by server
                          
                          actionButton(inputId = "goButtonDeleteVertex",
                                       label = "Delete node"),
                          
                          span(textOutput(outputId = "cantDeleteVertex"),
                               style = "color:red"),
                          
                          hr(),
                          
                          selectInput(inputId = "edgeToDelete",
                                      label   = "Edge to delete",
                                      choices = ""), # choices filled in by server
                          
                          actionButton(inputId = "goButtonDeleteEdge",
                                       label   = "Delete link"),
                          
                          
                          span(textOutput(outputId = "cantDeleteLink"), 
                               style="color:red"),
                          
                          hr(),
                          
                          radioButtons(inputId = "printTable",
                                       label = h4("Perturbation Table"),
                                       choices = list("Nodes" = "vertices",
                                                      "Links" = "edges"),
                                       selected = "vertices"),
                          
                          hr(),
                          
                          downloadButton('report', # name of downloadHandler in server
                                         'Download report')
                          
                        )),
                        style = "font-size:115%"), # end tabPanel "Network Perturbation"
               
               id = "conditionedPanels"
             )
      ),
      
      mainPanel(
        withMathJax(),
        conditionalPanel(condition="input.conditionedPanels==1",
                         
                         br(),
                         
                         h3("Result of Evaluation"),
              
                         br(),
                         
                         div(p(textOutput("evaluatedString")), 
                             style="font-size:120%", 
                             align="center"),
                         
                         br(),
                         
                         div(tableOutput("resultBDMTable"), 
                             style="font-size : 120%; 
                             font-family: Arial, Helvetica, sans-serif;", 
                             align="center"),
                         
                         hr(),
                         
                         div(p("$$\\textit{BDM} =
                              \\sum_{i=1}^{n} \\textit{K}(\\textit{block}_{i})
                               +\\textit{log}_{2}(|\\textit{block}_{i}|)$$"),
                             style="font-size: 120%",
                             align="center"), 
                         
                         hr(),
                         
                         div(p("Strings that don't appear in the
                            \\(D(\\#\\textit{of states}, \\#\\textit{ of symbols})\\) 
                               distribution have their
                               \\(\\textit{K}\\) value estimated as"),
                             style="font-size:110%"),
                         
                         div(p("$$ \\textit{Max}(K(\\#\\textit{ of states}, \\#\\textit{ of symbols}))
                               + 1 $$"), 
                             style="font-size:110%")
        ), ##end BDM 1D tab
        
        conditionalPanel(condition="input.conditionedPanels==2",
                         br(),
                         
                         h3("Adjacency Matrix"),
                         
                         div(tableOutput("loadedGraph"), align="center", style="font-size: 110%"),
                         
                         br(),
                         h3("Result of Evaluation"),
                         div(tableOutput("resultBDM2DTable"), style="font-size: 120%", align="center"),

                         hr(),
                         
                         div(p("$$BDM =
                              \\sum_{i=1}^{n} K(block_{i})+log_{2}(|block_{i}|)$$"),
                             style ="font-size: 120%")
        ), ##end BDM 2D tab
        
        conditionalPanel(condition ="input.conditionedPanels==3",
                         br(),
                         h3("Result of Evaluation"),
                         br(),
                         div(tableOutput("resultCTM"), 
                             style = "font-size: 120%", 
                             align = "center"), 
                         hr(),
                         
                  conditionalPanel(condition = "input.funct == 'acss'",
                                          
                         div(p("$$K(\\#\\textit{ of states}, \\#\\textit{ of symbols}) =
                              -log_{2}(D(\\#\\textit{of states}, \\textit{# of symbols})$$"), 
                             style = "font-size: 120%"),
                         
                         hr(),
                         
                        div(p("\\(\\textit{}~\\)\\(K(\\#\\textit{ of states}, \\#\\textit{ of symbols})\\)
                           indicates the estimated Kolmogorov complexity of 
                           the string by the Coding Theorem Method."),
                            style="font-size:110%"),
                          
                         hr(),
                         
                         div(p("\\(D(\\#\\textit{of states}, \\#\\textit{ of symbols})\\) indicates the 
                          estimated algorithmic probability, 
                          which is the output frequency of the string 
                           by Turing machines with the same alphabet."),
                             style="font-size:110%"),
                         
                         hr(),
                         
                         div(p("Strings that don't appear in the
                            \\(D(\\#\\textit{of states}, \\#\\textit{ of symbols})\\) 
                           distribution have their
                           \\(\\textit{K}\\) value estimated as"),
                             style="font-size:110%"),
                         
                         div(p("$$ \\textit{Max}(K(\\#\\textit{ of states}, \\#\\textit{ of symbols}))
                           + 1 $$"), 
                             style="font-size:110%"),
                         hr(),
                         
                         div(p("More information on the other complexity 
                               functions is available in the ",
                         a(href="https://cran.r-project.org/web/packages/acss/acss.pdf", 
                           "documentation of the ACSS package @ CRAN.")),
                         style="font-size:110%", align="center")
                  )
                  
              ), ## #end conditionalPanel CTM chosen 
        
        conditionalPanel(condition ="input.conditionedPanels==4", 
                         br(),
                         plotOutput("graphPlot"),
                         tableOutput("perturbationTable")
                         )
        
        ) ## end mainPanel 
      
      )
))