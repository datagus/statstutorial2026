# Statistics Tutorial Day 2, summer semester 2026

#Note: Use the tiny arrow on your left side to collapse/expand the different sections of the script

###################################################

####----1. Working Directory----####
getwd()

#this is how you set your working directory on a Mac,
#you need to adjust it to the location of the txt or csv file on YOUR computer!
#This basically tells R where to find the data:
setwd("/Users/carlo/Desktop/01_Projects/02_Statistics Tutorial SoSe 2025/data")

#with a Windows computer it looks slightly different:
#remember when you copy the path from Windows you have to change the /
setwd("C:/")

####----2. Basic Data Types in R----####

## character or string: anything inside single or double quotes
a <- 'apple'

## numeric: whole numbers and decimal numbers
b <- 1.618

## integers: positive and negative whole numbers
#             with `L` attached at the end
c <- 3L
d <- -6L

# check what type of object `b` is
class(b) # numeric


####----3. Normal Distribution----####

#data to calculate the mean and median
#enter the values from the board below into the vector candy (separate by comma)
#for example name<-c(1,2,3)

candy<-c(1,	2,	3,	5,	3,	2,	4,	2,	5,	6,	8,	3,	2, 10, 8, 2, 5, 10)

str(candy) #the structure of the data
summary(candy) #descriptive information about the data
boxplot(candy) #boxplot
hist(candy) #histogram

#check further documentation of the function hist()
?hist

#check the graphical parameters to customize your figures
?par

#what happens if you add bigger numbers to the vector?
candy2<-c(1,	2,	3,	5,	3,	2,	4,	2,	5,	6,	8,	3,	2, 49, 79, 2, 4, 5, 10, 6, 4)
hist(candy2)

#let's see how we can make this more "normal"
#why logarithm?
hist(log(candy2))

####----4. Load survey data----####
survey<-read.csv("survey26cleaned.csv",header=T, sep=",")

#attach survey (why?)

#detach survey (what happens if we "detach"?)

#let's inspect the structure of your data

#let's see the descriptive statistics of your data

#let's see the data in another window

#let's plot a histogram of the variable happy1 (remember the data is not attached)

#what if I put hist(happy1)  why the error?


####----5. Chi-Square-Test (chisq.test) for Independence----####
#Does the preference for having pineapple on a pizza is related to the preference of an operative system?

# H0 : The two variables are (stochastically) independent
# H1 : The two variables are related

#create a table with both variables

#run the X2 test (chi-square)


# if the p-value bigger is than 0.05 : we cannot reject the H0
# ==> the two variables are not related (independent)


#let's only check for Microsoft and Apple
#this is how you sub-select for Apple (does not create new variable!)
Pineapple_Pizza[OS=="Apple"]
#this is how you sub select for several factor levels
Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ]


#for the chisq.test you need to do this for both variables
#(if you want a shorter command structure create new variables instead, see below)
table(Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ],OS[OS == "Apple" | OS == "Microsoft" ])
chisq.test(table(Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ],OS[OS == "Apple" | OS == "Microsoft" ]))

# or
Pineapple <- Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ]
OperativeSystem <- OS[OS == "Apple" | OS == "Microsoft" ]

#run the table with the counts

#run again the chisquare test

# do we reject the null hypothesis?

#Practice at home: Test other variables from the survey that you can test with a Chi-Square-Test

####----6. Student t-test (Test for equality of means) - Independent----####

# Do students in the different two major programs(uwi and gess) have different heights?

# H0 : means are equal
# H1 : means are not equal

# 4 key decisions of t-test: independence, normality, equal sample size and variance

#Assumption 1 - independence of the data: No student is enrolled in the same major program

#Assumption 2 - normality of the data: Visualizing it and testing it
#let's separate the groups and save them into two different variables
#for example uwi <- height[major=="UWI"]

#let's create a histogram for each variable


#let's apply the shapiro test
# H0 : The sample is normally distributed
# H1 : The sample is not normally distributed

#what do the p-values tell us?


#Assumption 3 - Equal sample size

#the sample size is not the same, ups. Statiscal power is reduced, but not significantly to crash the t-test

#Assumption 4 - Equal variances
#let's visualize the variances with a boxplot

#let's run a F-test
# H0 : Ratio of variance is equal to 1 (they have the same variance)
# H1 : Ratio of variance is NOT equal to 1


#what does the p value tell us?

#Running the t-test
#is the difference we observe in our sample large enough to convince us
#that a real difference exists in the population, or could it have occurred by chance alone?"

# H0 : means are equal
# H1 : means are not equal


#what does the p-value tell us? How do we interpret the result?

#HOMEWORK
####----7. Paired t-test----####
# Paired Samples - when we apply the 2 treatments to the same samples,
# giving us paired combinations of samples for the 2 treatments

# Assumptions for the Paired Sample T-Test:

# (1) Differences between paired values follow a Normal distribution
# (2) Data is Continuous
# (3) We have paired or dependent samples
# (4) Simple Random Sampling - each unit has an equal probability of being selected

# Setting up our Hypotheses (Two-Tailed):

# H0: The pairwise difference between means is 0 / Paired Population Means are equal
# H1: The pairwise difference between means is not 0 / Paired Population Means are not equal

### Example: Weight loss after raising the younglings?

# Read data from .txt file
weight<-read.table("t.test.paired.txt",header=T)

# Attach object to search path
attach(weight)

# Structure of the object
str(weight)

# Boxplots for visual comparison
boxplot(before,after)

# Performing a two-tail, repeated T-Test
weight.t <- t.test(before,after, paired=T)

# Structure of output object
str(weight.t)

# Round up p-values
round(weight.t$p.value,digits=2)

# p-value is below 0.05: We reject the H0
# means for before and after treatment are not equal
