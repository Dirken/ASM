###########################################################
# Advanced Statistical Modelling: Linear Models assignments
# Ricard Meyerhofer Parra - 2019Q2
##########################################################

library(mice)

dataset <- read.csv("IMDB.csv", stringsAsFactors = TRUE, sep = ";")

###########################################################
#Exploring dataset
###########################################################

#Are there missing values? There are no missing values.
rowsWithNA <- !complete.cases(dataset); 
md.pattern(dataset)

#Are there outliers?
summary(dataset)


library(FactoMineR)
pca <- FactoMineR::PCA(dataset, quali.sup = c(1,12))
