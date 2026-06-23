##############################################################
# Multivariate statistics in R: ordination & clustering
# Revisiting the datasets from this semester
# From a multivariate perspective
##############################################################

# vegan: main package for community ecology ordination (DCA, RDA, CCA, envfit, ...)
# Created initially by Jari Oksanen
library(vegan)
# cluster: provides agglomerative clustering tools such as agnes() or divisive clustering like diana()
library(cluster)

#--------------------------------------------------------------
# Example using Dune meadow data
#--------------------------------------------------------------

#Load  classic data from Jongman
# dune: vegetation cover data for 20 dune meadow sites (sites x species)
# dune.env: environmental/management variables for the same 20 sites
data(dune);data(dune.env)

#inspect the data
str(dune)          # structure: 20 sites (rows) x 30 species (columns), cover scores
?dune              # opens the help file describing the dataset and its variables
View(dune)         # opens a spreadsheet-like viewer of the data matrix

#Make a DCA of the dataset
# Detrended Correspondence Analysis: an indirect/unconstrained ordination method,
# well suited to species data with non-linear (unimodal) responses along gradients
modeldca<-decorana(dune)

#How much does the model explain
# printing the model gives eigenvalues per axis and gradient lengths (in SD units);
# higher eigenvalues / longer axes = more variation captured by that axis
modeldca

#How does it look?
# default plot shows both sites and species together in ordination space (axis 1 vs 2)
plot(modeldca)

#choose two species distant to each other and find a description/ picture
# i.e. pick two species that sit far apart on the plot and look up their
# ecology/habitat preferences - this helps you interpret what the axes represent

#Plot only the sites
# cleaner view of how the 20 sites spread out along the ordination axes,
# without species labels cluttering the picture
plot(modeldca,display=c("sites"))
# and now species without sites - mind the change in axes scaling
plot(modeldca,display=c("species"))
# now both
plot(modeldca,display=c("both"))

#Make a cluster analysis
# agglomerative hierarchical clustering with Ward's method (minimizes within-cluster
# variance at each merge step); agnes() computes the dissimilarities internally
modelclust<-agnes(dune,method="ward")

# Compare with the DCA, use the blue arrows in the plot window
# plotting an agnes() object gives you a dendrogram;
# use the on-screen arrows to move between them. Compare the resulting clusters
# to the DCA: sites grouped together here should also lie close together there
plot(modelclust, which=2)

#--------------------------------------------------------------
# Example using the Swiss data
#--------------------------------------------------------------

#Here is a PCA of the swiss data that we used before
# swiss: socio-economic and fertility data for 47 French-speaking Swiss provinces (~1888)
data(swiss)

#inspect data....
?swiss             # variable descriptions: Fertility, Agriculture, Examination,
                   # Education, Catholic, Infant.Mortality

# Principal Component Analysis - a linear ordination method, appropriate here since
# swiss variables are continuous and roughly linearly related (unlike the DCA above)
# scale=T standardizes all variables to unit variance first, since they are measured
# on very different scales/units
modelpca<-prcomp(swiss, scale=T)

# proportion of total variance explained by each principal component
summary(modelpca)

# scree-type plot of variance explained per component
# Compare the plot to the Proportion of Variance in the plot
plot(modelpca)

# allow biplot labels/arrows to extend slightly outside the plot region
par(xpd=T)

# biplot: shows observations (provinces) as points and variable loadings as arrows
# in the same PCA space, so you can interpret both together
biplot(modelpca)

# lm as an example how to select non-redundant variables
# attach() exposes swiss's columns directly by name (no swiss$ prefix needed) -
# convenient, but can clash with other objects in your workspace; detach() when done
attach(swiss)

# linear regression: Does Examination performance relate to Education, 
# infant mortality, and fertility?
# Filter predictor variables based on PCA.
modelswiss <- lm(Examination ~ Education+ Infant.Mortality+ Fertility)


library(car)
vif(modelswiss)
# inspect coefficients, their significance, and overall model fit (R-squared, F-test)
summary(modelswiss)
# Infant mortality is not significant
modelswiss2 <- lm(Examination ~ Education+ Fertility)
summary(modelswiss2)
AIC(modelswiss);AIC(modelswiss2)
# According to AIC, both models are too similiar and would need to be averaged
# This is way beyond the difficulty of this module, but people were asking about this
library(MuMIn)
mods <- list(modelswiss, modelswiss2)
model.sel(mods)
ms <- model.sel(modelswiss, modelswiss2)
avg <- model.avg(ms)
summary(avg)

#--------------------------------------------------------------
# Example using Varechem data
#--------------------------------------------------------------

#Now a DCA with environmental data fitted
# varespec: lichen/vegetation cover data from a reindeer grazing study (sites x species)
# varechem: corresponding soil chemistry/environmental variables for the same sites
data(varechem);data(varespec)

#inspect data
modeldca2<-decorana(varespec)
plot(modeldca2)

# envfit: correlates external environmental variables with the ordination axis scores;
# perm=1000 runs a permutation test to assess the significance of each variable
modelfit<-envfit(modeldca2,varechem,perm=1000)

# adds arrows for the (significant) environmental variables onto the existing DCA plot -
# arrow direction = gradient direction, arrow length = strength of the correlation
plot(modelfit)

# Alternatively, one can make a PCA of the environmenatl matrix
varepca<-prcomp(varechem, scale = T)
summary(varepca)
biplot(varepca)

#--------------------------------------------------------------
# Example using Mtcars data
#--------------------------------------------------------------

# mtcars: classic dataset of car design/performance variables (mpg, cyl, hp, wt, ...)
data(mtcars)

# PCA on mtcars; scale=T is important here since mpg, hp, wt etc. are on very
# different units and would otherwise dominate the result unfairly
modelpca2<-prcomp(mtcars,scale=T)

# biplot of cars (points) and variables (arrows) in PCA space
biplot(modelpca2)

#--------------------------------------------------------------
# Example using Airbnb listings
#--------------------------------------------------------------

# set the working directory to the folder containing airbnb.csv
setwd("/Users/henrikvonwehrden/Desktop/R/temp/")

# read the csv file; header=T = first row holds column names, sep="," = comma-separated
air<-read.csv("airbnb.csv",header=T, sep=",")

# attach so columns can be referenced directly by name
attach(air)

# inspect structure - check which columns are numeric vs. character/factor,
# since PCA below needs purely numeric input
str(air)

# keep only the numeric columns suitable for PCA: columns 1-3, 5-15, and 18
# (this drops e.g. ID/text/categorical columns that prcomp can't handle)
airsub<-air[,c(2:3,5:15,18)]

# PCA on the selected airbnb variables; center=T and scale=T put every variable
# (price, reviews, availability, etc.) on a comparable footing before the PCA
airmodel<-prcomp(airsub,scale=T,center=T)

# biplot showing listings (points) and variable loadings (arrows) together
biplot(airmodel)

biplot(airmodel, xlabs = rep("", nrow(airmodel$x)))

# Try to interpret the plot



## Centering the plot
loadings <- airmodel$rotation[, 1:2]

# Compute the scaling factor biplot() uses internally
n <- nrow(airmodel$x)
lam <- airmodel$sdev[1:2] * sqrt(n)
loadings_scaled <- t(t(loadings) * lam)

plot(loadings_scaled, type = "n",
     xlab = "PC1", ylab = "PC2",
     asp = 1)
abline(h = 0, v = 0, lty = 2, col = "grey80")
arrows(0, 0, loadings_scaled[, 1], loadings_scaled[, 2],
       col = "red", length = 0.1)
text(loadings_scaled, labels = rownames(loadings_scaled),
     col = "red", cex = 0.8, pos = 3)
