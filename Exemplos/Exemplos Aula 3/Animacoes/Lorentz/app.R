#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(plotly)
ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
   # Application title
   titlePanel("Lorentz System"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("steps","Number of Simulation steps",min = 1,max=500,value = 50),
        numericInput('alfa','alfa',value=10),
        numericInput('beta','Beta',value=8/3),
        numericInput('rho','rho',value=28),
         uiOutput("AnimationSlider")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot"),
         plotOutput("XZ"),
         plotOutput("XY"),
         plotOutput("YZ")
         
      )
   )
)

server <- function(input, output) {
  LorentzSimulation<-reactive({
   x<-y<--9
   z<-20
   dt=1e-2
   alfa<-input$alfa
   beta<-input$beta
   rho<-input$rho
   for(i in 2:input$steps){
     x[i]<-x[i-1]+dt*alfa*(y[i-1]-x[i-1])
     y[i]<-y[i-1]+dt*(x[i]*(rho-z[i-1]) -y[i-1] )
     z[i]=z[i-1]+dt*(x[i]*y[i] -beta*z[i-1]  )
   }
   return(data.frame(x=x,y=y,z=z))
  }) 
  
  output$AnimationSlider<-renderUI({
    sliderInput('animacao',label = "Lorentz Step",min = 1,max=0.1*input$steps,value = 1,step=1,animate = TRUE)
  })
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
      df<-LorentzSimulation()
      df<-df[1:(10*input$animacao),]
      plot_ly(x=df$x,y=df$y,z=df$z,size=0.1,type="scatter3d",mode="markers" ) %>% layout(
        title = "3D",
        scene = list(
          xaxis = list(title = "X"),
          yaxis = list(title = "Y"),
          zaxis = list(title = "Z")
        ) )
   })
   
   output$XZ <- renderPlot({
     # generate bins based on input$bins from ui.R
     df<-LorentzSimulation()
     df<-df[1:(10*input$animacao),]
     require(ggplot2)
     p<-ggplot(df,aes(x,z)) + geom_point() + labs(x="X",y="Z")
     #ggplotly(p)
    p
   })
   output$XY <- renderPlot({
     # generate bins based on input$bins from ui.R
     df<-LorentzSimulation()
     df<-df[1:(10*input$animacao),]
     require(ggplot2)
     p<-ggplot(df,aes(x,y)) + geom_point() + labs(x="X",y="Y")
     #ggplotly(p)
     p
   })
   
   output$YZ <- renderPlot({
     # generate bins based on input$bins from ui.R
     df<-LorentzSimulation()
     df<-df[1:(10*input$animacao),]
     require(ggplot2)
     p<-ggplot(df,aes(y,z)) + geom_point() + labs(x="Y",y="Z")
     #ggplotly(p)
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

