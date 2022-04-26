setwd("~/Desktop/Evolution/Tasks/Task_11")
#I collaborated with Rebecca LaRochelle and Google for portions of this assignment.
#PART 1
?rnorm
x <- rnorm(100, mean=5, sd=2)
#SD=2 because 2 squared = variance of 4
summary(x)
?runif
y <- ((x*5)+2)+runif(100, 0, 0.1)
plot(x,y)
abline(lm(y~x))
coef(lm(y~x))
#coef() function states y intercept is 2.047642 and slope is 5.000811.
#This is because I set y to equal 5x+2. This is the slope-intercept form equation y=mx+b where m=slope and b=y-intercept.
#I'm assuming the values are not exactly 2 and exactly 5 because of the random numbers I added at the end.
#FOR LOOP
z <- c()
for(i in 1:100) {
  z[i] <- runif(1, 0, 1)
  y <- ((x*z[i])+2)+runif(100, 0, 0.1)
  mb <- coef(lm(y~z[1:100]))
}
mb
plot(z, z[1:100])
#The slope and y-intercept seem to be the same as the previous plot, despite x being multiplied by the random numbers.
#The actual x and y values of each point are less, but the slope and direction of the line look nearly identical to the first plot.
#PART 2: MONTY HALL PROBLEM
n <- 10000
prize <- sample(c("A", "B", "C"), size=n, replace=TRUE)
doorOpened <- ifelse(prize=="A", sample(c("B", "C"), size=n, replace=TRUE), ifelse(prize=="B", "C", "B"))
doorUnopened <- ifelse(doorOpened=="B", "C", "B")
NotSwitchingWinChance <- sum(prize=="A")/n
SwitchingWinChance <- sum(prize==doorUnopened)/n
?barplot
WinChance <- c(NotSwitchingWinChance,SwitchingWinChance)
barplot(WinChance, names.arg = c("Not Switching Doors", "Switching Doors"), ylab="Chance of Winning", main="Monty Hall Odds of Winning Grand Prize", col="cornflowerblue")
#PART 3: MEME
install.packages("meme")
library(meme)
install.packages("jpeg")
library(jpeg)
m <- readJPEG("ScumbagSteve.jpeg")
m2 <- meme(m, "Makes Plot in R", "Saves it Using Export instead of pdf( )")
plot(m2, size=2)

