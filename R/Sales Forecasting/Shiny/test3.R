library(shiny)

# obseverEvent()
# actionButton()
# observe()
# eventReactive()


ui = fluidPage(

  sliderInput(inputId = "num",
              label="Choose a number",
              value=25, min=1, max=100),
  
  textInput(inputId="title",label="Label for the histogram",
            value="Histogram of Random Normal Values"),
  
  actionButton(inputId="button_update", label="Update Histogram"),
  
  actionButton(inputId="button_clicks", label="Show Clicks"),
  
  plotOutput(outputId = "out_hist")
  
)


server = function(input, output){
  
  data = eventReactive(input$button_update, {
                    rnorm(input$num)
                  })
  
  output$out_hist = renderPlot({
    hist( data(), main=isolate(input$title) )
  })
  
  observeEvent(input$button_clicks,  # reactive value(s) to respond to 
               {
                 print(as.numeric(input$button_clicks))
                 # observer does not respond to changes
                 # in reactive values within this block
               })
  
  # observe() : more implicit syntax
  
}


shinyApp(ui=ui, server=server)
