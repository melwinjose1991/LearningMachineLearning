library(shiny)


# tags$
# HTML("<raw HTML>")
# fluidRow()
# column()
# tabsetPanel() / navlistPanel()
# navbarPanel()
# shinyDashboard() ???


ui = fluidPage(
  
  fluidRow(
    column(3,tags$h1("Header 1"))
  ),
  
  fluidRow(
    column(4, offset=1,
      tags$p(style="font-family:Impact",
              "See other apps in the",
              tags$a("Shiny Showcase",
                      href="http://shiny.rstudio.com/gallery/"
                    )
            )
    )#columns
  ),
  
  tabsetPanel(
    tabPanel(title="Tab-1","You can place anything you want here :)"),
    tabPanel(title="Tab-2","Again, you can place anything you want here :|"),
    navbarMenu(title="More Tabs",
               tabPanel(title="Tab-3", "hmmm....")
               )
  )
  # tabsetPanel : can be used inside a page,
  # but with navbarPage() the entire page is
  # the page.
    
)


### CSS

#1: external css : place it in www/
#     theme = "<file>.css"

#2: tags$head( tags$link(rel, type, href),
#              tags$style(HTML("<CSS>"))
#             )

#3: tags$h1(..., style"...")


server = function(input, output){
}


shinyApp(ui=ui, server=server)
