library(shiny)

ui = basicPage(
  plotOutput("plot1", click = "plot_click", width = 400),
  verbatimTextOutput("info1"),
  verbatimTextOutput("info2")
)

server = function(input, output) {
  
  output$plot1 = renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
  
  output$info1 <- renderText({
    paste0("x=", input$plot_click$x, "\n",
           "y=", input$plot_click$y)
  })
  
  output$info2 = renderPrint({
    row = nearPoints(mtcars, input$plot_click,
                      xvar = "wt", yvar = "mpg",
                      threshold = 5, maxpoints = 1)
    
    cat("Nearest point within 5 pixels:\n")
    print(row)
  })
}

shinyApp(ui, server)
