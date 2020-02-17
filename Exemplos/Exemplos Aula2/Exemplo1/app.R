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
   titlePanel("Iris Distribution Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
  
        selectInput(inputId = "X",label = "variavel",choices = names(iris)[1:4]),
        sliderInput(inputId ="valores",min = 2,max=10,value=3,label="Caixas" )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  dataset<-reactive({
    dataset<-iris
    return(dataset)
  })
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     set<-dataset()
      x    <- set[input$X] 
      
      x<-c(x[,1])
      bins <- seq(min(x), max(x), length.out = input$valores + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',main=paste("Histograma de",input$X))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

