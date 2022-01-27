#For Task 03, I collaborated with Rebecca LaRochelle for some parts, and looked on the subreddit for help a few times.
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
Sample1
Sample2
#To the naked eye, the samples do not seem that different. When looking at the boxplots, however, you can see that the values are slightly higher on average in Sample1. This is because of the difference in the two populations each sample was taken from. Population1, which is where Sample1 was taken from, had a normal distribution mean of 5, while population2, which is where Sample2 was taken from, had a normal distribution mean of 4. 
#I also noticed that Sample2 had a slightly wider range than Sample1, but this is probably due to the random chance of the 50 numbers R happened to pull out of the 1,000,000 for each population. They are relatively the same range because the standard deviation was the same for both population1 and population2 (which Sample1 and Sample2 were taken from, respectively), at 5.
boxplot(Sample1, Sample2)
#Part III
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandma)
nrow(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
MatGrandpa
PatGrandma
PatGrandpa
Alan <- makeBaby(PatGrandma, PatGrandpa)
#Brenda
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
#The percentage of genes shared between Brenda and Focus should be 0.50, or 50%.
ToMom <- length(grep("mom", Focus)) / length(Focus)
#Yes, they match my expectation!
#The percentage of genes shared between Focus and his maternal grandparents should be 0.25, or 25%.
ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus)
#This did not match my expectation!
#Focus is not equally related to each maternal grandparent. 
ToDadMom <- length(grep("grandma_da", Focus)) / length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus)) / length(Focus)
#Focus is also not equally related to each paternal grandparent. This is not what I expected. I thought since Focus inherited exactly 50% of genes from each parent, he would inherit exactly 25% from each grandparent, but that was not the case. I assume this is due to random chance, and percentage of genes inherited from grandparents will always be somewhat near 25%.
#The average relatedness of Focus to all four grandparents is 25%, so I assume the fact that there are 4 total grandparents means that there can be some variance in the percentage of genes inherited from each grandparent individually, but the overall inherited genes from each SET (maternal and paternal) of grandparents will be exactly 25% each.
Sibling_01 <- makeBaby(Brenda, Alan)
#I expect Focus to share roughly 50% of his DNA with his sibling.
ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)
#Focus shared 48.245% of his DNA with his sibling. This is kind of what I expected, but I honestly thought it would be closer to 50%. i imagine this is again due to random chance, and if Focus gets another sibling, the percentage of DNA he shares with it will be different, but never straying too far away from 50%.
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan))) / length(Focus))
#The amount of genes Focus shares with each of the 1,000 siblings actually varies way more than I thought it would. However, the average percentage of genes shared throughout the entire 1,000 siblings is right around 50%, at 50.3%. The quantile values for each percentage group also make sense, and are in line with what I would have predicted them to be. The 50th percentile (most common) is 0.504650, meaning that the most common shared gene percentages amongst the 1,000 siblings were right around 50%, and as the shared gene percentage values deviate further and further away from 50%, they get less and less common in the 1,000 sibling sample.
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#This plot resembles the common bell curve, which makes sense.
#A range of values is observed in these analyses because outliers will always exist due to random chance and the nature (some would argue pitfalls) of sexual reproduction. This has to do with the mechanisms of sexual reproduction, mainly recombination during meiosis, which occurs randomly for each different offspring. These functions in R do a really good job of modelling that randomness in my opinion.
HWE <- function(p) {
  aa <- p^2
  ab <- 2 * p * (1-p)
  bb <- (1-p) ^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#Yes, I believe I can read and understand this plot. As the frequency of the allele 'a' in members of a population increases, the percentage of population members with that allele in their genotype (aa) increases (exponentially). Also, as the number of people in a population with allele 'a' decreases, the percentage of people in the population with 'aa' genotypes decreases exponentially.
#Time is not shown on this plot, nor is geographic space.
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#Yes, the frequency of the aa genotype in my population matches the expectation from Hardy-Weinberg.
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#More points have been added further up the aa curve. These points are also more scattered out, especially near the top of the curve. This is due to more aa genotype people being added to this population via the function. Naturally, as more aa genotype people are added to the population, the frequency of the 'a' allele will also increase, especially since there were already 500 members with the aa genotype present from the previous step. 
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(Ne=50, nrep=5, pause=0.01)
x <- genetic.drift(Ne=1000, nrep=5, pause=0.01)
x <- genetic.drift(Ne=600, nrep=5, pause=0.01)
#As Ne is increased, the 5 population lines become more straight, and do not deviate far from the middle (0.5) over time up to 100. As Ne is decreased, the 5 population lines become less straight, and tend to deviate towards either 0.0 or 1.0 on the graph over time. This tells me that with more individuals in a population, it probably takes longer for genetic drift to occur, and vice versa.
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt ~ Samples + 0)
abline(Line2)
summary(Line2)
Line2$coef
#Line2 seems to take away the intercept value (0, 0) as a data point and adjust the slope for the data accordingly. For these data, Line2 would make more sense because without the (0, 0) data point (which is non-existent in the data) affecting the slope, the value is more accurate to the true data.
#As the population size increases, the points tend to stray further and further away from the line. To me, this means that as the population size increases, there is more (unequal) variation between the path the line models and the actual data. Because of this, the line becomes less descriptive of the data pattern.
#Extra Credit
library(MASS)
robustLine <- rlm(tExt ~ Samples)
abline(robustLine)
summary(robustLine)
robustLine$coef
#The robust line takes out the inflation that the outlier data points (in the top-right corner) cause in the slope. The slope of the robust line is lower than the slope of the regular line, which leads to it better fitting the data located lower on the plot where there are more points, which in turn makes the robust line a better representation of the data overall.  

