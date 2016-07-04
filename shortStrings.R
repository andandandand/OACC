### start ###
source("compressionLength.R")
require(acss)

acss("00")

server <- shinyServer(
  function(input, output) {
    output$text1 <- renderTable({
      input$action
      isolate({
        
        if(input$funct == "entropy"){
          x <- do.call(input$funct, args = list(string = unlist(strsplit(input$string, " "))))
          if (!is.matrix(x)) {
            x <- as.matrix(x)
            colnames(x) <- "Shannon entropy"
          }
        }
        
        if(input$funct == "entropy2"){
  
          strings <- unlist(strsplit(input$string, " "))
          x <- lapply(strings, entropy2)
          if (!is.matrix(x)) {
            x <- as.matrix(x)
            colnames(x) <- "Second order entropy"
            rownames(x) <- strings
          }
        }
        
        else if(input$funct == "compression-gzip"){
          
          strings <- unlist(strsplit(input$string, " "))
          
          x <- lapply(strings,
                      compressionLength,
                      compressionType = "gzip")
          
          if (!is.matrix(x)) {
            x <- as.matrix(x)
            colnames(x) <- "Compression length (bytes)"
            rownames(x) <- unlist(strsplit(input$string, " "))
          } 
         
        }
        
        else if(input$funct == "compression-bzip2"){
          
          strings <- unlist(strsplit(input$string, " "))
          
          x <- lapply(strings,
                      compressionLength,
                      compressionType = "bzip2")
          
          if (!is.matrix(x)) {
            x <- as.matrix(x)
            colnames(x) <- "Compression length (bytes)"
            rownames(x) <- unlist(strsplit(input$string, " "))
          } 
          
        }
        
        else if(input$funct == "compression-xz"){
          
          strings <- unlist(strsplit(input$string, " "))
          
          x <- lapply(strings,
                      compressionLength,
                      compressionType = "xz")
          
          if (!is.matrix(x)) {
            x <- as.matrix(x)
            colnames(x) <- "Compression length (bytes)"
            rownames(x) <- unlist(strsplit(input$string, " "))
          } 
          
        }
        
        else{
          x <- do.call(input$funct,
                       args = list(string = unlist(strsplit(input$string, " ")),
                                   alphabet = as.numeric(input$alphabet)))
          if (!is.matrix(x)) {
            x <- as.matrix(x)
            colnames(x) <- paste0(input$funct, ": ", input$alphabet)
          }
        }
      })
      x
    }, digits = 16)
  }
)
### end ###


#and a ui.R file:
  ### start ###
 ui<- shinyUI(fluidPage(
    titlePanel("Obtain ACSS for multiple strings"),
    
    sidebarLayout(
      sidebarPanel(tags$textarea(id="string", rows=3, cols=40, "enter strings separated by spaces"),
                   #textInput("string", label = h3("String"), value = "0"),
                   radioButtons("alphabet", label = "Alphabet (number of possible symbols)", inline = TRUE,
                                choices = list("2" = 2, "4" = 4, "5" = 5, "6" = 6, "9" = 9), selected = 9),
                   selectInput("funct", label = "Which function?",
                               choices = list("acss" = "acss",
                                              "Shannon entropy" = "entropy",
                                              "Second order entropy" = "entropy2",
                                              "Compression length by gzip" = "compression-gzip",
                                              "Compression length by bzip2" = "compression-bzip2",
                                              "Compression length by xz" = "compression-xz",
                                              "likelihood d" = "likelihood_d",
                                              "likelihood ratio" = "likelihood_ratio",
                                              "prob random" = "prob_random"
                                              
                                              
                               ), selected = "acss"),
                   actionButton("action", label = "Get output")
      ),
      mainPanel(
        h3("Result:"),
        tableOutput("text1")
      )
      
    )
    
  ))
### end ###
 
 shinyApp(ui = ui, server = server)