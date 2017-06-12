library(shiny)

# obseverEvent()
# actionButton()
# observe()
# eventReactive()
# reactiveValues()


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

# TIP: Code outside the server function will be run
# once per R session (worker)

server = function(input, output){
  
  # TIP: Code inside the server function will be run
  # once per end user (connection)
  
  data = eventReactive(input$button_update, {
                    rnorm(input$num)
                  })
  
  output$out_hist = renderPlot({
    # TIP: code inside a reactive function will be run
    # once per reaction
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

# 1:32:40
