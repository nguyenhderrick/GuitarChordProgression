mongo <- mongo.create()
collection <- "guitar.collection"
allChords <- mongo.find.all(mongo, collection, '{"chords" : {"$exists" : 1}}')
chordList <- lapply(allChords, function(x) x$chords)

seperate <- function(y) {
  sapply(y, function(x) {
    if (str_sub(x, 2, 2) == "b" | str_sub(x, 2, 2) == "#")
      chords <- c(str_sub(x, end = 2), str_sub(x, 3))
    else
      chords <- c(str_sub(x, end = 1), str_sub(x, 2))
  })
}

chordPieces <- sapply(chordList, seperate)

modRoot <- function(x) {
  if (!class(x) == "list") {
    sapply(x[1,], function(y) {
      root <- switch(y,
             "A" = 1,
             "A#" = 2,
             "Bb" = 2,
             "Hb" = 2,
             "B" = 3,
             "H" = 3,
             "Cb" = 3,
             "B#" = 4,
             "H#" = 4,
             "C" = 4,
             "C#" = 5,
             "Db" = 5,
             "D" = 6,
             "D#" = 7,
             "Eb" = 7,
             "E" = 8,
             "E#" = 9,
             "F" = 9,
             "F#" = 10,
             "Gb" = 10,
             "G" = 11,
             "G#" = 12,
             "Ab" = 12)
      ifelse(is.null(root), NA, root)
    })
  }
}

moddedRoots <- sapply(chordPieces, modRoot)

#chords are modified to have 4 notes for matrix manipulation
modChord <- function(x) {
  if (!is.list(x)) {
    chord <- sapply(x[2,], function(y) {
      y <- sub("/.*$", "", y)
      if(y == "") 
        y <- c(0, 4, 7, NA)
      else if(y == "m") 
        y <- c(0, 3, 7, NA)
      else if(y == "dim") 
        y <- c(0, 3, 6, NA)
      else if(y == "M7"| y == "maj7") 
        y <- c(0, 4, 7, 12)
      else if(y == "7") 
        y <- c(0, 4, 7, 11)
      else if(y == "m7") 
        y <- c(0, 3, 7, 11)
      else if(y == "5"| y == "sus") 
        y <- c(0, 7, NA, NA)
      else if(y == "add9" | y == "2") 
        y <- c(0, 4, 7, 2)
      else if(y == "sus2") 
        y <- c(0, 2, 7, NA)
      else if(y == "sus4") 
        y <- c(0, 5, 7, NA)
      else
        y <- c(NA, NA, NA, NA)
    })
    return(chord)
  } else
    return(NULL)
}

moddedChords <- sapply(chordPieces, modChord)

keyCal <- function(x, y) {
  if(!is.null(x) & !is.null(y)) {
    #nrow and each are 4 because chords are 4 notes
    gg <- (matrix(rep(x, each = 4), nrow = 4) + y) %% 12
  }
}

calcs <- mapply(keyCal, moddedRoots, moddedChords)

guessKey <- function(calculation) {
  vecNotes <- as.vector(calculation)
  ionianScale <- c(0, 2, 4, 5, 7, 9, 11)
  
  notesInScale <- numeric(0)
  for (i in 1:12) {
    notesInScale[i] <- sum(vecNotes %in% ((ionianScale + i) %% 12))
  }
  #notesInScale represents the number of times the chords in the song fits
  #within the tested key. The first key in the vector is A, the last is Ab
  return(notesInScale)
}

keyNotes <- lapply(calcs, guessKey)
#possibleKeys is what key the algorithm believes the song to be in. 
possibleKeys <- lapply(keyNotes, function(x) which(max(x) == x))

#This table will show that most songs are played in G (11) and C (4)
#The data has disregarded the capo.
table(unlist(possibleKeys))

possibleKeysLength <- sapply(possibleKeys, length)
#The table will show how many keys the song could be in, it has failed in 
#identifying the key.
table(possibleKeysLength)

rootProgression <- mapply(function(x, y) {
  lapply(y, function(a) {(x - a) %% 12})}, 
  moddedRoots, possibleKeys)

switchNotation <- function(x) {
  roman <- sapply(x, function(a) {
    switch(as.character(a),
                 "0" = "I",
                 "1" = "#I",
                 "2" = "II",
                 "3" = "#II",
                 "4" = "III",
                 "5" = "IV",
                 "6" = "#IV",
                 "7" = "V",
                 "8" = "#V",
                 "9" = "VI",
                 "10" = "#VI",
                 "11" = "VII"
    )
  })
}

chordProgression <- mapply(function(x, y) {
  if(is.list(x))
    list()
  else
    lapply(y, function(b) {paste0(switchNotation(b),"_", x[2,])})
  }, 
  chordPieces, rootProgression)


