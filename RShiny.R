install.packages("Rtools")
library(Rtools)
library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Data science SAIL!"),
  
  sidebarPanel(
    h3('Sidebar text')
  ),
  mainPanel(
    h3('Main Panel text')
  )
))
library(shiny)
shinyServer(
  function(input, output) {
  }
)
runApp("~/RShiny")
