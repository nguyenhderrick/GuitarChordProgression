

# find all errors and try to recapture them
error <- mongo.find.all(mongo, collection, '{"error": {"$exists":1}}')

sapply(error, function(x) try(getTab(x$tabLinks)))

#check if there are new duplicates of the collection
sapply(error, function(x) length(
  mongo.find.all(mongo,collection, sprintf('{"tabLinks": "%s"}', x$tabLinks))))

# removing all duplicates
allDat <- mongo.find.all(mongo, collection, '{"`_id`" : {"$exists" : 1}}')
tabLinks <- sapply(allDat, function(x) x$tabLinks)
removeThese <- duplicated(tabLinks)

idRemove <- function(x) mongo.remove(mongo, "test.people", 
                                     list(`_id` = x$`_id`))

sapply(allDat[removeThese], idRemove)

#remove spaces from chords and have the chords easily accessible by 
#string or list.
#find raw data without chord data to update collection
cursor <- mongo.find(mongo, collection, '{ $and: [{"chords" : {"$exists" : 0}},
                     {"raw" : {"$exists" : 1}}')

while (mongo.cursor.next(cursor)) {
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

#find all collections with chords
mongo <- mongo.create()
collection <- "guitar.collection"
allChords <- mongo.find.all(mongo, collection, '{"chords" : {"$exists" : 1}}')
#collect chordString
chordString <- lapply(allChords, function(x) x$chordString)




