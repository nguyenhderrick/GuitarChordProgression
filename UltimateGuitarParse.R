library(XML)
library(RMySQL)
library(dplyr)
library(rmongodb)
library(httr)
library(stringr)

TabNoThres <- 20
thresh <- function(stars, votes) {stars >= 4 & votes * stars > 27}
pass <- "0000"

getLetterList <- function(url) {
  response <- GET(url)
  doc <- htmlParse(content(response, as = "text"))
  letterLinks <- xpathSApply(doc, "//span[@class='alph']/a/@href")
}

getLetterPages <- function(url){
  
  response <- GET(url)
  doc <- htmlParse(content(response, as = "text"))
  pages <- xpathSApply(doc, "//td[@style='padding:10px;border-top:solid 1px #4F4C46']/b/a/@href")
  names(pages) <- NULL
  pages <- sapply(pages[1:length(pages)-1], appendUrl)
  c(url, pages)
}

getPages <- function(bandDat) {
  if(bandDat[[2]] > 100) {
    url <- bandDat[[1]]
    response <- GET(url)
    doc <- htmlParse(content(response, as = "text"))
    pages <- xpathSApply(doc, 
      "//td[@style='padding:10px;border-top:solid 1px #4F4C46']/b/a/@href")
    names(pages) <- NULL
    pages <- sapply(pages[1:length(pages)-1], appendUrl)
    list(c(url, pages), bandDat[[2]])
  } else {
    list(bandDat[[1]], bandDat[[2]])
  }
}

appendUrl <- function(x) {
  if(!grepl("http", x))
    paste0("http://www.ultimate-guitar.com", x)
  else
    x
}
    
getBand <- function(url) {
  response <- GET(url)
  doc <- htmlParse(content(response, as = "text"))
  timedownloaded <<- date()
  bandLinks <- xpathSApply(doc, "//table[@cellpadding='2']/tr/td/a/@href")
  TabCount <- xpathSApply(doc, "//table[@cellpadding='2']/tr/td/font", xmlValue)
  
  data.frame(bandLinks, TabCount = as.numeric(TabCount))
}
  

listParse <- function(x) {
  ratingString <- toString.XMLNode(x)
  stars <- str_count(ratingString, "active")
  if(str_detect(ratingString, "half")) 
    stars <- stars + 0.5
  
  votes <- as.integer(xmlToList(x)$font$text)
  if(length(votes) == 0) 
    votes <- 0
  
  elem <- c(stars = stars, votes = votes)
}

#gets passed a page of tabs for a single band and returns a data.frame of tab links, type, and rating
getTabPage <- function(url) {
  response <- GET(url)
  doc <- htmlParse(content(response, as = "text"))
  tabLinks <- xpathSApply(doc, 
    "//td[@style='padding:8px']/table[@cellpadding='2']/tr/td/a/@href")
  tabName <- xpathSApply(doc, 
    "//td[@style='padding:8px']/table[@cellpadding='2']/tr/td/a", xmlValue)
  ratingNode <- getNodeSet(doc, 
    "//tr/td/table/tr/td/table/tr/td[@width='20%' and not(@height)]")
  ratingList <- lapply(ratingNode, listParse)
  type <- xpathSApply(doc, "//td[@style='color:#DDDDCC']/b", xmlValue)
  
  tabPage <- cbind.data.frame(tabName, tabLinks, type,do.call(rbind,ratingList),
                              deparse.level = 0)
  tabPage$tabLinks <- as.character(tabPage$tabLinks)
  tabPage$tabName <- as.character(tabPage$tabName)
  
  return(tabPage)
}

createId <- function(bandPages) {
  name <- gsub("_tabs.*", "", gsub(".*/", "", bandPages[[1]]))
  #in case of missing an odd name
  name <- gsub("[[:punct:]]", " ", name)
}

#gets passed a list of pages for a single band
tabPageWrapper <- function(bandDat, con, collection) {
  bandPages <- bandDat[[1]]
  bandName <- createId(bandPages)
  names(bandName) <- "artist"
  print(bandName)
  
  tabPage <- lapply(bandPages, getTabPage)
  bandTabs <- do.call(rbind, tabPage)
  bandTabs$votes <- as.integer(bandTabs$votes)
  
  rateThres <- thresh(bandTabs$stars, bandTabs$votes)
  passThres <- rateThres & as.character(bandTabs$type) == "Chords"
  
  for(i in 1:nrow(bandTabs)) {
    if (passThres[i]) {
      chord <- try(getTab(bandTabs[i, ]$tabLinks))
      if (class(chord) == "try-error") {
        bson <- mongo.bson.from.list(c(bandName, 
                                       as.list(bandTabs[i, ]), 
                                       error = TRUE))
        mongo.insert(con, collection, bson)
      } else {
        dataList <- c(bandName, as.list(bandTabs[i, ]), chord)
        bson <- mongo.bson.from.list(dataList)
        mongo.insert(con, collection, bson)
      }
    } else {
      bson <- mongo.bson.from.list(c(bandName, as.list(bandTabs[i, ])))
      mongo.insert(con, collection, bson)
    }  
  }
}


getTab <- function(tabdt) {
  response <- GET(tabdt)
  doc <- htmlParse(content(response, as = "text"))
  raw1 <- getNodeSet(doc, "//*[@id='page-wrapper']/div/div/div/div/div[@class='t_comments']//span[@class='t_cbl']/text() | //pre[@class = 'js-tab-content']")
  raw1 <- lapply(raw1, function(x) paste(capture.output(x), collapse="\n"))
  names(raw1) <- c("comments", "raw")
  comments <- as.integer(gsub("[^0-9]", "", raw1$comments))
  ifelse(is.na(comments), raw1$comments <- 0, raw1$comments <- comments)
  
  raw2 <- xpathApply(doc, "//*[@id='page-wrapper']/div/div/div/div/div[@class='stats']//text()",xmlValue, trim=TRUE)
  raw2 <- numberParse(raw2)
  
  raw <- c(raw1, raw2)
}

numberParse <- function(raw2) {
  views <- raw2[sapply(raw2, function(x) grepl('view', x))][[1]]
  weekViews <- raw2[sapply(raw2, function(x) grepl('week', x))][[1]]
  
  views <- gsub("[[:punct:]]|[[:blank:]]|[[:alpha:]]", "", views)
  weekViews <- gsub("[[:punct:]]|[[:blank:]]|[[:alpha:]]", "", weekViews)
  
  list(views = as.integer(views), weekViews = as.integer(weekViews))
}

UltimateGuitarScrape <- function() {
  url <- "https://www.ultimate-guitar.com"
  mongo <- mongo.create()
  collection <- "guitar.collection"
  
  letterList <- getLetterList(url)
  letterPages <- sapply(letterList, getLetterPages)
  
  bandList <- lapply(unlist(letterPages), getBand) #bands is dataframe with partial url and number of tabs
  bandList <- do.call(rbind, bandList)
  rownames(bandList) <- NULL
  
  bandList[ ,1] <- sapply(bandList[ ,1], function(x) paste0(url, x))
  bandList <- bandList[order(bandList$TabCount, decreasing = T), ]
  bandDict <- list()
  
  for (i in 1:nrow(bandList)) {
    bandDict[[i]] <- getPages(bandList[i, ])
  }
  
  tabList <- lapply(bandDict, 
                    tabPageWrapper, 
                    con = mongo, 
                    collection = collection)
  
  cursor <- mongo.find(mongo, collection, '{"raw" : {"$exists" : 1}}')
  
  #remove spaces from chords and have the chords easily accessible by 
  #string or list.
  while (mongo.cursor.next(cursor)){
    rawTab <- mongo.cursor.value(cursor)
    listTab <- mongo.bson.to.list(rawTab)
    chords <- xpathSApply(htmlParse(listTab$raw), "//span", xmlValue)
    chords <- gsub(" ", "", chords)
    listTab$chords <- chords
    listTab$chordString <- paste(chords, collapse = " ")
    newRawTab <- mongo.bson.from.list(listTab)
    mongo.update(mongo, collection, rawTab, newRawTab)
  }
  mongo.cursor.destroy(cursor)
}


  
  
