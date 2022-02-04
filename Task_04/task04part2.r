setwd("~/Desktop/Evolution/Tasks/Task_04")
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
head(results)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
plotChis(counts)
#When the chi squared is high, the bars are extremely uneven. When the chi squared is low, the bars are more even. 
#The plotChis() function shows you that the chi square value relates to the evenness of the data, and can indicate how much each field in the data differ from one another. Again, the higher the chi square value, the higher the variance between different fields in a data set, and vice versa.
Avg <- mean(Chisqs)
Avg
#The average Chi-squared is 60.99081. I interpret that as meaning throughout the many plots, the average chi squared value comes out to be 60.99081. Throughout all of the plots I made, the top graph always has the same chi square value, as does the 2nd, 3rd, and 4th. Each "sub-graph" in the whole plot exhibits the same chi square value no matter how many times I run the function.
#The average chi-squared (60.99081) is much higher than the critical value in the packet (11.70).
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#The chi-squared does differ by background.
propSig <- length(which(Chisqs > 11.70))/length(Chisqs)
percSig <- round(100 * propSig)
percSig
#Yes, the percent of trials that had a significant p-value does surprise me. It seems very high.
#I do not believe the only thing driving that very high number is natural selection.
pdf("biggraph.pdf", height=10, width=5)

par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck= -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las=1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck= -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for(i in backgrounds) {
  Data <- Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1
}
abline(v=11.70, lty=2, lwd=2, col='black')
#I do not see any meaningful differences between backgrounds.
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
propSim <- length(which(Simulation > 11.70))/length(Simulation)
percSim <- round(100*propSim)
percSim
#The percentage of the time the selection-free simulation finds a "significant" result is 89%.
#The selection-free simulation can produce final counts so different from the initial counts because while there is no selection being factored into the simulation, genetic drift is still taking place.
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0, 0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0, 0, 1, 0.25))
#As far as I can tell, the mixture is quite similar to the student-generated data.
#When taking how the mixture was generated into consideration (strong selection), most student groups show evidence of strong selection.
#If I had to describe the relative strength of different evolutionary processes across all the groups in all the labs across all of the years, I would say that genetic drift plays the biggest factor in the chi squared results being larger than the critical value, but (extreme) selection can be blamed for the experiments where the chi square value is extremely high, like in the 300s.
#The evolutionary processes at work in the lab as done by humans are selection and genetic drift.
#The evolutionary process at work in the lab as simulated by the computer are also selection and genetic drift, but the simulation lacks the student bias that causes such extreme selection to occur in some cases in the student experiment. The students know what is supposed to happen in the experiment, so their consciousness in some cases causes them to pick the "right" toothpicks to prove selection occurs. This is still selection, just a different kind.
#Regarding the relative strength of the evolutionary processes the BIOL-112 students are simulating, the graphs tell us that at the lower chi squared values (<100), genetic drift is primarily driving the results because of how similar that section of each plot is to the simulation where no selection takes place. Genetic drift will only differentiate the data to a certain point though. In the trials where the chi squared values are at ~200 and beyond, selection has taken over and become the strongest factor. The differences in the student trial plots and the selection-free simulation plot proves this.
#In my opinion, comparing the student numbers to the simulated numbers tells you way more about what processes are occurring here than comparing them to a single critical value. 
#Introducing mutation would probably increase the chi squared values for each experiment because of the increased genetic diversity that it would cause (as long as it was a new color not already present, which is what I am assuming).










