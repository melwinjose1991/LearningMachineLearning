library(shiny)

ui = fluidPage(
  sliderInput(inputId = "num",
              label="Choose a number",
              value=25, min=1, max=100),
  
  textInput(inputId="title",label="Label for the histogram",
            value="Histogram of Random Normal Values"),
  
  actionButton(inputId = "action_button", label = "Change"),
  
  plotOutput(outputId = "out_1")
)

server = function(input, output){
  #1 : Save objects to display to output$<outputId>
  #2 : Build objects to display with render<type_of_object>(<code_block>)
  #3 : use input values with input$<inputId>
  
  output$out_1 = renderPlot({
    # code used to build/re-build object
    hist(rnorm(input$num), main=input$title)
  })
  
  # Reactivity : when the value of input$value is change
  # Shiny notifies all the things that rely
  # on the its value to update
  # Reactions = Reactive Functions + Reactive Values
  
}

shinyApp(ui=ui, server=server)
