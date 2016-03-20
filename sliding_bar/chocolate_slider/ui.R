



library(shiny)
library(plotly)





shinyUI(fluidPage(
  titlePanel("Chocolate!"),
  sidebarPanel(
    sliderInput("bins", "Participant Number:", min = 2401, max = 2417, value = 2415)
  ),
  mainPanel(
    plotlyOutput("heatPlot")
  )
))


