library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel("Chord Progression Finder"),
    column(5,
        uiOutput("init"), 
        uiOutput("search")
    ),
      
    column(7,tabsetPanel(
        tabPanel("Song Table", DT::dataTableOutput("ref")),
        tabPanel("Chords", uiOutput("table"))
    ))
  )
)