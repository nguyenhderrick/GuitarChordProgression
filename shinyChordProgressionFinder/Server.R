library(shiny)
chordProgression <- readRDS("chordProgression.rds")
songRef <- readRDS("songRef.rds")

#Fina all uses of this chord in all tabs
findPos <- function(chordProgression, string) {
  #string example would be "III_7\\S*"
  lapply(chordProgression, 
         function(a) {
           lapply(a, function(b) {grep(string, b)})
         }
  )
}

matchProg <- function(chordProgression, chordList, relation) {
  # Relation is only in relation to the first chord on the chordList.
  # A list of 3 with relation 2 will give two chords after the first, the third
  # chord on the list.
  pos <- findPos(chordProgression, chordList[[1]])
  
  if(length(chordList) > 1) {
    for(i in 2:length(chordList)) {
      rollPos <- findPos(chordProgression, chordList[[i]])
      testPos <- lapply(pos, 
                        function(x) {lapply(x, function(y) {y + 1})}
      )
      pos <- mapply(function(x, y) {
        mapply(function(a, b) {intersect(a, b)}, x, y, SIMPLIFY = FALSE)
      }, rollPos, testPos)
    }
  }
  
  relate <- function(a, b) {
    mapply(function(x, y) {y[x - length(chordList) + 1 + relation]}, a, b)
  }
  
  output <- mapply(relate, pos, chordProgression)
  occurrences <- sapply(pos, function(x) length(unlist(x)))
  indexes <- which(occurrences > 0)
  
  return(list(sort(table(unlist(output)), decreasing = TRUE), 
              data.frame(indexes, occurrences[indexes])))  
}

matchProg <- function(chordProgression, chordList) {
  # Relation is only in relation to the first chord on the chordList.
  # A list of 3 with relation 2 will give two chords after the first, the third
  # chord on the list.
  pos <- findPos(chordProgression, chordList[[1]])
  
  if(length(chordList) > 1) {
    for(i in 2:length(chordList)) {
      rollPos <- findPos(chordProgression, chordList[[i]])
      testPos <- lapply(pos, 
                        function(x) {lapply(x, function(y) {y + 1})}
      )
      pos <- mapply(function(x, y) {
        mapply(function(a, b) {intersect(a, b)}, x, y, SIMPLIFY = FALSE)
      }, rollPos, testPos)
    }
  }
  
  return(pos)
}

relater <- function(chordProgression, chordList, relation, pos) {
  relate <- function(a, b) {
    mapply(function(x, y) {y[x - length(chordList) + 1 + relation]}, a, b)
  }
  
  output <- mapply(relate, pos, chordProgression)
  occurrences <- sapply(pos, function(x) length(unlist(x)))
  indexes <- which(occurrences > 0)
  
  return(list(sort(table(unlist(output)), decreasing = TRUE), 
              data.frame(indexes, occurrences[indexes])))  
}

server <- shinyServer(function(input, output) {
  output$init <- renderUI({
    numericInput("numChords", "Number of Chords", 2, min = 1, max = 40, 
                 width = '50%')
  })
  
  output$search <- renderUI({
    roots <- 'list("I" = "I", "II" = "II", "III" = "III", "IV" = "IV", 
    "V" = "V", "VI" = "VI", "VII" = "VII", "any" = ".+")'
    
    chordCol <- character()
    for(i in 1:input$numChords) {
      chordCol <- c(chordCol, sprintf(
        "fluidRow(
        column(4, selectInput('root%d', 'Root %d', %s, , selected = '.+', width = '200%%')),
        column(1, checkboxInput('sharp%d', '#', FALSE), style='padding:20px 0px 0px 0px'),
        column(4, textInput('chord%d', 'Chord %d')),
        column(3, checkboxInput('exact%d', 'exact', FALSE), style='padding:20px 0px 0px 0px'))",
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
  
  values <- reactiveValues(listInput = NULL, pos = NULL)
  
  buttonInputs <- eventReactive(input$button, {
      
      listInput <- list()
      for(i in 1:input$numChords) {
        listInput <- append(listInput,
                            eval(parse(text = sprintf("paste0(
                                    '^',
                                    ifelse(input$sharp%d, '#',''),
                                    input$root%d,
                                    '_', 
                                    input$chord%d,
                                    ifelse(input$exact%d, '$','')
                            )",
                               i, i, i, i)))
        )
      }
      
      if(!identical(isolate(values$listInput), listInput)){
        values$listInput <- listInput
        
        withProgress(message = 'Please Wait', value = 0, {
          values$pos <- matchProg(chordProgression, listInput)
          relater(chordProgression, listInput, input$position -1, values$pos)
        })
      }
      else
        relater(chordProgression, listInput, input$position -1, values$pos)
  })
  
  output$table <- renderTable({
    head(data.frame('chord at position' = buttonInputs()[[1]]), 10)
  })
  
  output$ref <- DT::renderDataTable({
    songTable <- t(songRef[,buttonInputs()[[2]][[1]]])
    songTable <- data.frame(cbind(songTable, buttonInputs()[[2]][[2]]))
    colnames(songTable) <- c("Artist", "Song", "Views", "Occurances")
    songTable$Views <- as.numeric(levels(songTable$Views))[songTable$Views]
    songTable$Occurances <- 
      as.numeric(levels(songTable$Occurances))[songTable$Occurances]
    DT::datatable(songTable, options = list(pageLength = 25))
  })
})