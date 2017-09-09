#library(shiny)
  
ui <- shinyUI(
  fluidPage(
    titlePanel("Chord Progression Finder"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("init"), 
        uiOutput("search")
      ),
      
      mainPanel(tabsetPanel(
        tabPanel("Song Table", DT::dataTableOutput("ref")),
        tabPanel("Chords", uiOutput("table"))
      ))
    )
  )
)

server <- shinyServer(function(input, output) {
  output$init <- renderUI({
    numericInput("numChords", "Number of Chords", 2, min = 1, max = 40, 
                 width = '50%')
  })

  output$search <- renderUI({
    roots <- 'list("I" = "I", "II" = "II", "III" = "III", "IV" = "IV", 
      "V" = "V", "VI" = "VI", "VII" = "VII")'
    
    chordCol <- character()
    for(i in 1:input$numChords) {
      chordCol <- c(chordCol, sprintf(
              "fluidRow(
              column(3, selectInput('root%d', 'Root %d', %s, width = '70px')),
              column(2, checkboxInput('sharp%d', '#', FALSE), style='padding:15px;'),
              column(4, textInput('chord%d', 'Chord %d')),
              column(2, checkboxInput('exact%d', 'exact', FALSE), style='padding:15px;'))",
              i, i, roots, i, i, i, i)
              )
    }
    
    chordPanel <- paste0(
      "wellPanel(", 
      do.call(paste, c(as.list(chordCol), sep = ",")), 
      ", numericInput('position', 'Position of Interest', 2, min = -1, max = 40, 
       width = '100px'), actionButton('button', 'submit')",
      ")"
    )
    
    eval(parse(text = chordPanel))
  })
  
  matchList <- eventReactive(input$button, {
      withProgress(message = 'Please Wait', value = 0, {
        stringInput <- sprintf("paste0(
                               '^',
                               ifelse(input$sharp%d, '#',''),
                               input$root%d,
                               '_', 
                               input$chord%d,
                               ifelse(input$exact%d, '$','')
                               )",
                               1, 1, 1, 1)
        
        if(input$numChords > 1){
          for(i in 2:input$numChords) {
            stringInput <- paste(stringInput, 
                                 sprintf("paste0(
                                         '^',
                                         ifelse(input$sharp%d, '#',''),
                                         input$root%d,
                                         '_', 
                                         input$chord%d,
                                         ifelse(input$exact%d, '$','')
                                         )",
                               i, i, i, i), sep = ",")
          }
        }
        
        listInput <- eval(parse(text = paste0("list(", stringInput,")")))
        
        matchList <- matchProg(chordProgression, 
                              listInput, input$position - 1)
      })
  })
  
  output$table <- renderTable({
    head(data.frame(matchList()[[1]]), 10)
  })
  
  output$ref <- DT::renderDataTable({
    songTable <- t(songRef[,matchList()[[2]][[1]]])
    songTable <- data.frame(cbind(songTable, matchList()[[2]][[2]]))
    colnames(songTable) <- c("Artist", "Song", "Views", "Occurances")
    songTable$Views <- as.numeric(levels(songTable$Views))[songTable$Views]
    songTable$Occurances <- 
      as.numeric(levels(songTable$Occurances))[songTable$Occurances]
    DT::datatable(songTable, options = list(pageLength = 25))
  })
})

shinyApp(ui = ui, server = server)
