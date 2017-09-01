
#Fina all uses of this chord in all tabs
findPos <- function(chordProgression, string) {
  #string example would be "III_7\\S*"
  lapply(chordProgression, 
                   function(a) {
                     lapply(a, function(b) {grep(string, b)})
                   }
  )
}
  
#function to find a chord and chords coming before or after the given chord
findChord <- function(chordProgression, string, relation) {
  #relation is finding the chord in relation to the string chord
  #i.e. directly before the string "-1" or after "1"
  #Make sure to not partial match the string "^I_" instead of "I_"
  
  chordPos <- findPos(chordProgression, string)
  relate <- function(a, b) {
    mapply(function(x, y) {y[x + relation]}, a, b)
  }
  output <- mapply(relate, chordPos, chordProgression)
  sort(table(unlist(output)), decreasing = TRUE)
}

#function to find chords coming before or after a list of chords
#chordList retains its order.
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

