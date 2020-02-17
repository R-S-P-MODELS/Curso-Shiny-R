#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Iris Plot Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
    tabsetPanel(id="Referencia",
     tabPanel("Graficos",selectInput(inputId = "X",label = "variavel",choices = names(iris)),
        selectInput(inputId = "Y",label = "variavel",choices = names(iris))
    ),
    tabPanel("Tabelas",tableOutput("Tab"))
    )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
         plotOutput("distPlot"),
         tableOutput("Tabela")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$Tabela<-renderTable({
    if(input$Referencia=="Tabelas"){
    a<-dataset()
    return(a)
    }
    
  })
  dataset<-reactive({
    dataset<-iris
    return(dataset)
  })
   output$distPlot <- renderPlot({
     if(input$Referencia=="Tabelas")
        return(-1)
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

