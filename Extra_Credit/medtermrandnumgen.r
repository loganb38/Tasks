#This is a random number generator I made to help me study for my Medical Terminology class.
#I divide my flash cards by unit in sets of 50, number the flash cards 1 to 50, then tell R to generate a set of random numbers between 1 and 50.
#This gives me a list of the numbers 1-50 sorted randomly every time, which I then go through from beginning to end.
#This has actually worked really well, and much better than just studying them in order like I always have.
setwd("~/Desktop/Evolution/Tasks/Extra_Credit")
sample(1:50, 50, replace=TRUE)