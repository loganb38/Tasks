setwd('/Users/lcb/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
Data
options(max.print=1000000)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data [2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
#Data[257,]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
nrow(berenMilk)
#322 rows in 'berenMilk'. Each row represents a time when Beren was fed milk from a bottle.
Feeds <- which(Data[, 'event'] == 'bottle')
Feeds
Feeds <- which(Data$event == 'bottle')
Feeds
head(Feeds)
Data[Feeds,]
#Each of these three ways of finding the bottle events are the same. I convinced myself they were all the same by realizing that they are all different methods of identifying the same column to R.
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
head(beren2)
head(beren3)
#Age column was added in "beren2", data was sorted by age, with the first even happening when Beren was 2 days old, in "beren3".
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)