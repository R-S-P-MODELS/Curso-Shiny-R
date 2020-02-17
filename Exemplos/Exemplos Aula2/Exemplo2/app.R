#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("Dataset Descriptor"),
   
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "Arq1",label = "Arquivo de entrada",accept = ".csv",placeholder = "Esperando Arquivo csv")
        #selectInput(inputId = "Opcao",label="Dataset",choices=c("iris","mtcars")),
       # uiOutput("iX"),
      #  uiOutput("iY")
    
      ),
      
      mainPanel(
        verbatimTextOutput(outputId = "Informacoes")
        
         #plotOutput("distPlot")
      )
   )
)

# Define server logic 
server <- function(input, output) {
  options(shiny.maxRequestSize=1000*1024^2)
  
  
  dataset<-reactive({
    if(!is.null(input$Arq1)){
      dataset<-read.csv(input$Arq1$datapath)
      return(dataset)
    }
  })
  
  output$iX<-renderUI({
    data<-dataset()
    selectInput("X",label="Eixo X",choices = names(data) )
    
  })
  
  output$iY<-renderUI({
    data<-dataset()
    selectInput("Y",label="Eixo Y",choices = names(data) )
    
  })
  
  output$Informacoes<-renderPrint({
    data=dataset()
    cat("O arquivo se chama",input$Arq1$name,"\nSeu tamanho é de ",input$Arq1$size/1e6," Megabytes\nEle esta Localizado em",input$Arq1$datapath)
    cat("\nEste arquivo possui",nrow(data),"linhas e ",ncol(data)," colunas")
    cat("\nO numero de variaveis categóricas sera",sum(sapply(data,class) %in% c('character','factor') )," e o numero de variaveis numericas",sum(sapply(data,class) %in% c('integer','numeric')) )
    })
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     set<-dataset()
      x    <- set[input$X]
      y<-set[input$Y]
      
      x<-x[,1]
      y=y[,1]
      
      plot(x,y,xlab=input$X,ylab=input$Y)
      
      # draw the histogram with the specified number of bins
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

