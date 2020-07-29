
# I need the RawData for this method and ListOfTags

# I will first make a metadata with how much point i have and how much i clean

ListOfTags <- unique(RawData$TAG)
str(ListOfTags)
countpointday <- list()
countpointday2 <- list()
dataforlenght <- data.frame(matrix(ncol = 3, nrow = 0))
dataforlenght<- dataforlenght[nrow(dataforlenght) +1,]
x <- c("TAG","date","lenght")
colnames(dataforlenght) <- x


for (i in ListOfTags) {
  d <- subset(RawData, TAG == i)
  DatesOfTags <- as.character(unique(d$date))
  countpointday <- list()
  for (b in DatesOfTags) {
    #if(b >= length(DatesOfTags)) break()
    dd <- subset(d, date == b)
    daypoints <- nrow(dd)
    dataforlenght$lenght <- daypoints
    dataforlenght$date <- unique(dd$date)
    dataforlenght$TAG <- i
    print(dataforlenght)
    countpointday[[b]] <- dataforlenght
    
  }
  tt <- do.call(rbind.data.frame, countpointday)
  countpointday2[[i]] <- tt
  print(countpointday2)
} 

pointlenght <- do.call(rbind.data.frame, countpointday2)
pointlenght$month <- month(pointlenght$date)

DatesOfTagss <- as.character(unique(pointlenght$date))

for (d in DatesOfTagss) {
  ff <- subset(pointlenght, date == d)
  fdf <- ggplot(data=ff, aes(x = date, y = lenght, fill = TAG )) +
    geom_bar(stat="identity", position=position_dodge())  +
    theme_minimal() + ggtitle("Life span of all battery")
  print(fdf)
}

ggplot(data=pointlenght, aes(x = date, y = lenght, fill = TAG )) +
  geom_bar(stat="identity", position="stack")  +
  theme_minimal() + ggtitle("Locations quantities of tags per month") +
  facet_wrap(~month, scale="free") 

write.csv(pointlenght, file = paste('DailyLocations', 'csv', sep = '.'))

