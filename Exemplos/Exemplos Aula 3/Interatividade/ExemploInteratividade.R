ui <- basicPage(
  plotOutput("plot1",
    click = "plot_click",
    dblclick = "plot_dblclick",
    hover = "plot_hover",
    brush = "plot_brush"
  ),
  verbatimTextOutput("info"),
  verbatimTextOutput("simples"),
  verbatimTextOutput("duplo")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })

  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })


  output$simples <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg", threshold = 10, maxpoints = 5,
               addDist = TRUE)
    # nearPoints() also works with hover and dblclick events
  })


  output$duplo <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(mtcars, input$plot_dblclick, xvar = "wt", yvar = "mpg", threshold = 10, maxpoints = 5,
               addDist = TRUE)
    # nearPoints() also works with hover and dblclick events
  })
  
}

shinyApp(ui, server)
