#I collaborated with Rebecca LaRochelle for parts of this assignment.
setwd("~/Desktop/Evolution/Tasks/Task_05")
#II.
library(learnPopGen)
?coalescent.plot
Plot1 <- coalescent.plot(n=10, ngen=20)
pdf("CoalescentPlot1.pdf", height=10, width=5)
#^This pdf function would not work, so I used the "Export" tool to save each graph as a pdf.
Plot2 <- coalescent.plot(n=50, ngen=20)
Plot3 <- coalescent.plot(n=100, ngen=200)
#^This one took quite a while...Ha
Plot4 <- coalescent.plot(n=10, ngen=20)
Plot5 <- coalescent.plot(n=10, ngen=20)
#Question 1: Plot1 began with 10 different alleles, Plot2 began with 50 different alleles, and Plot3 began with 100 different alleles. I modified the number of alleles by changing the number following 'n=' in the function.
#Question 2: As more alleles are added, it takes much longer for one allele to go to fixation, but for a 10 allele simulation, it takes about 8 generations on average for one of them to go into fixation.
mean(8, 6, 10)
#Question 3: The average number of offspring each haploid individual has is right around 2. This is accounting for the individuals that had zero offspring, and the individuals that had four offspring (the highest number I noticed.) Most seemed to have either one or two offspring in my simulations.
#The number of offspring varies in that it increases as one allele begins to dominate and move toward fixation. From what I can tell, as one allele begins to move toward fixation, individuals with that allele start having more offspring on average.
#Question 4: Fitness does not play a factor in these simulations. Throughout each simulation, the color that reaches fixation is random, so no allele is more fit than the others. "Fitness doesn't see each allele, just the sum of 2 copies of an allele"
#Question 5: Yes, in these simulations, the most recent common ancestor for the focal locus is typically alive in generation 0. For example, if the orange allele reaches fixation, any member of the all-orange population in generation 20 can be traced back to the original orange in generation 0.
#III
install.packages("rehh", dep=T)
Yes
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
library(phytools)
library(coala)
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) + feat_mutation(10) + feat_recombination(10) + sumstat_trees() + sumstat_nucleotide_div()
stats <- simulate(model, nsim=1)
stats
Diversity <- stats$pi
Diversity
#In the Diversity object, all of the numbers are not the same. This is because of mutation, recombination, or a combination of both of those features we added to the simulation. The numbers are different because different loci exhibit differing degrees of sameness to the same loci of other individuals.
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#Question 6: The number of tips does not match the number of individuals simulated because in the original function, we set the ploidy number to 2 (diploid), which means each individual receives 2 copies of each allele, so in this case 5 individuals would exhibit 10 alleles at one locus.
Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
Age1
Age2 <- max(nodeHeights(t2))
Age2
#The most recent common ancestor for t2 is 1.218863 back. The most recent common ancestor for t1 is 1.220087 back. The ages of the common ancestors for each SNP are not exactly the same, but very close.
par(mfrow=c(1, 2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
#Question 7: Assuming we're still talking about the age of each common ancestor, looking only at the two plots, to the naked eye they do match.
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for(n in 1:ntrees) {
    if(locus==1 && n==1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else{
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model <- coal_model(sample_size=5, loci_number=15, loci_length=500, ploidy=2) + feat_mutation(10) + feat_recombination(10) + sumstat_trees() + sumstat_nucleotide_div()
model2 <- coal_model(sample_size=5, loci_number=15, loci_length=500, ploidy=2) + feat_mutation(10) + feat_recombination(5) + sumstat_trees() + sumstat_nucleotide_div()
stats2 <- simulate(model2, nsim=1)
Nloci2 <- length(stats2$trees)
for(locus in 1:Nloci2) {
  ntrees <- length(stats2$trees[[locus]])
  for(n in 1:ntrees) {
    if(locus==1 && n==1) {
      outPhy2 <- read.tree(text=stats2$trees[[locus]][n])
    }
    else{
      outPhy2 <- ape:::c.phylo(outPhy2, read.tree(text=stats2$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy2)
#In model2, I changed the recombination rate from 10 to 5.
#I predicted this would reduce the spread and number of lines in the plot, which means the genetic diversity would decrease. If the genetic diversity decreases, that would basically mean the population would be more inbred, and on average the alleles would share a common ancestor more recently than if recombination rate was higher.
#I think my prediction was correct!
model3 <- coal_model(10, 50) +
  feat_mutation(par_prior("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats3 <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats3, function(x) mean(x$pi))
theta <- sapply(stats3, function(x) x$pars[["theta"]])
plot(mean_pi, theta, cex=1, col="red", main="mean_pi Plotted Against theta", xlab="mean_pi", ylab="theta")
abline(lm(mean_pi ~ theta))
lm(mean_pi ~ theta)
#IV. Extra Credit
activate_msms(jar=NULL, java=NULL, priority=200, download=TRUE)
model4 <- coal_model(c(13,20),5) + feat_size_change(0.25, population=2, time="0") + feat_selection(strength_A=1000, population="all", time = 0, locus_group = "all") + feat_migration(0.5, 1, 2) + feat_migration(1.0, 2, 1) + feat_growth(8, time=0) + feat_growth(3, time=1) 
stats4 <- simulate(model4, nsim = 40)
#My model seemed to run with no errors, but when I tried to run the actual simulation, I received errors that I think have something to do with Java.
#I tried using the "activate_msms" function command suggested in the subreddit, but I still had no luck.
#Without running the simulation though, I still believe I can draw some decent conclusions about how the two populations compare in nucleotide diversity.
#I think a population with more immigration than emigration (with the same degrees of selection) will show higher nucleotide diversity due to new individuals, likely with different alleles, joining the population.
#A population with higher emigration than immigration would have lower nucleotide diversity with the same degrees of selection.
#These are general conclusions, but I would need to actually run the model to see how the other factors (varying rats of selection, starting population size, varying rates of migration, etc) interact with each other and lead to changes in nucleotide diversity.



