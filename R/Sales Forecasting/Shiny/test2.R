library(shiny)

# reactive()
# isolate()

ui = fluidPage(
  sliderInput(inputId = "num",
              label="Choose a number",
              value=25, min=1, max=100),
  
  textInput(inputId="title",label="Label for the histogram",
            value="Histogram of Random Normal Values"),
  
  plotOutput(outputId = "out_hist"),
  
  verbatimTextOutput(outputId = "out_summary")
)

server = function(input, output){
  
  data = reactive({
    
    # Reactive Expression
    
    rnorm(input$num)
    
    # Code used to build/rebuild reactive object.
    # Will respond to every reactive value in the code.
    # Reactive exp cache their values (the exp will return
    # its most recent value, unless it has become invalidated)
    
  })
  
  output$out_hist = renderPlot({
    
    hist( data(), main=isolate(input$title) )
    
    # Isolate : makes a non-reactive object
    #  value is updated when other reactive 
    #  element is updated.
    
  })
  
  output$out_summary = renderText({
    summary(data())
  })
  
}

shinyApp(ui=ui, server=server)
