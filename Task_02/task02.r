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
#Beginning of Task 02b; I worked/studied with Rebecca LaRochelle for parts of this Task.
#Question 1
#Hypothesis 1 is inappropriate for this data because the term 'amount' is not defined. 'Amount' could mean the volume of food he eats, the number of times he eats during the day, etc. The language is too ambiguous.
#Hypothesis 2 is inappropriate for this data because again, the language is too ambiguous. It doesn't define the type of relationship between how much Beren naps and how much he drinks. Also, the terms 'how much' tell us nothing. 'How much' could mean number of times in a day, or time per occurence, etc. Also, what is Beren drinking? Milk? Water? The hypothesis doesn't say.
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
#Units for average milk value are ounces
#You used the value column to tell R that you want to use the values from that column (conveniently named 'Values') to formulate your average. You already told R that you wanted the 'bottle' data in the previous step, and named it 'Feeds'. The value column is the column where the numbers of ounces of milk per feeds in located.
#The set of square brackets with "Feeds" inside of it tells R to use the data from the "which(beren3$event == "bottle)" command that you assigned to the variable "Feeds" to draw your average from, which we assigned to the variable "avgMilk".
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
head(avgFeed)
avgFeed[1:3]
avgFeed[4:33]
length(avgFeed)
nrow(avgFeed)
ncol(avgFeed)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
varFeed
totalFeed
numFeeds
?cor
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenCor
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
berenANOVA
?aov
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
?par
#"las=1" means the axis labels will always be horizontal
#"mar=c(5,5,1,1)" means you set the margins of the graph to be 5 lines on the bottom, 5 lines on the left, 1 line on the top, and 1 line on the right.
#"mgp=c(2, 0.5, 0)" means you changed the margin for the axis title to be 2 lines above the graph, the margin for the axis labels to be 0.5 line from the graph, and the margin line for the axis line to be 0 lines from the graph (touching it). 
#"tck=-0.01" means you set the size of the tick marks of the graph to 0.01. The negative sign means the tick marks will be outside of the actual graph space.
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("rO2b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#Question 2: The graph is impossible to interpret because it does not give any indication as to what "ounces of milk" or "age in days" is referring to. "Ounces of milk" could be referring to ounces of milk drank by Beren, ounces of milk drank by me, or ounces of milk I threw out. The graph doesn't tell us. Also, "age in days" could be referring to Beren's age, my goldfish's age, or the age of anything else. Without knowing the exact thing those ambiguously titled axis labels are referring to, we cannot reliably interpret the graph. 
#One more reason this graph can't be interpreted is because if you look at the data, Beren drinks a lot less milk in the later days. I think this is because at that point, he has began to eat more real food, and drink less milk. If a person was going by just that graph with no further context, then they would assume Beren just quit drinking as much milk as he had been, which is true, but only tells half the story. This could lead to ill-informed interpretations being made. One way to solve this problem would be to drop the mlk data from after he began to eat real food.    
source("http://jonsmitchell.com/code/plotFxn02b.R")
#Self Quiz
unique(beren3$event)
colnames(beren3)
head(beren3)
beren3[1:50,]
#My Hypothesis: There is an overall positive correlation between Beren's head circumference in cm and his age in days.
berenHeadGrowth <- which(beren3$event == "trait_head_circum")
beren3[berenHeadGrowth,]
cor.test(beren3$value[berenHeadGrowth], beren3$age[berenHeadGrowth])
#Based on the 'cor' value of 0.8759189, I believe it can be reasonably concluded that there is an overall positive correlation between Beren's head circumference in cm and his age in days, confirming my original hypothesis.
#Extra Credit I
which(beren3$event == "nap")
beren4 <- which(beren3$event == "nap")
berenNaps <- beren3[beren4,]
berenNaps
berenNaps$duration_hours = berenNaps$end_hour - berenNaps$start_hour
berenNaps
berenNaps$duration_minutes = berenNaps$end_minute - berenNaps$start_minute
berenNaps$duration_hours <- berenNaps$duration_hours*60
berenNaps$Total_Mins <- berenNaps$duration_hours + berenNaps$duration_minutes
Total_Mins <- berenNaps$Total_Mins
age <- berenNaps$age
NapTimePerDay <- tapply(berenNaps$value[Total_Mins], berenNaps$age[Total_Mins], sum)
Naps <- which(berenNaps$event == "nap")
NapTimePerDay <- tapply(berenNaps$value[Total_Mins], berenNaps$age[Naps], sum)
NapTimePerDay <- tapply(berenNaps$Total_Mins[Naps], berenNaps$age[Naps], sum)
#CORRECT ONE!!! ^^^
NapTimePerDay
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
TimeSleptvsDay <- plot(as.numeric(names(NapTimePerDay)), NapTimePerDay, type="b", pch=16, xlab="Age in Days", ylab="Time Napped per Day in Minutes")
pdf("r02b-TimeNappedVsDay.pdf", height=4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(NapTimePerDay)), NapTimePerDay, type="b", pch=16, xlab="Age in Days", ylab="Time Napped per Day in Minutes")
abline(h=mean(NapTimePerDay), lty=2, col='red')
dev.off()
berenNaps$StartTime <- paste(berenNaps$start_hour, berenNaps$start_minute)
berenNaps
cor.test(berenNaps$start_hour[Naps], berenNaps$Total_Mins[Naps])
#The correlation coefficient value was -0.1079752, meaning that there is a slightly negative correlation between nap starting time and the nap's total duration.
#I tried to get the specific nap starting time, and I actually did, but they were not numeric values. I could not find a way (by googling or looking at my packets/notes) to change the column I made of the starting hour and minute combined to numeric values. Because of this, I was not able to put those values through the cor.test function, so I did what I thought would be the next best thing, and used only the nap start hour values for the correlation.
#I still was able to get the main task (nap durations for each day) done!




