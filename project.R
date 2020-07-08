# DSC 424, Final Project
# Umair Chaanda

library(MASS)
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(ggplot2)
library(psych)
library(dplyr)


##################################################################
#                     DATA EXPLORATION
##################################################################

# Load the test dataset
housingTest = read.csv("housing_test.csv")
head(housingTest) # Shows the first 6 rows
nrow(housingTest) # Report number of rows in dataset


# Load the train dataset
housingTrain = read.csv("housing_train.csv")
head(housingTrain)          # Shows the first 6 rows
nrow(housingTrain)          # Report number of rows in dataset 
ncol(housingTrain)          # Report number of columns in dataset
str(housingTrain)           # List the "structure" of the dataset including variable names and types
sapply(housingTrain, class) # check type of variables"


# Pull the names of numeric vaiables
housingNumeric = names(dplyr::select_if(housingTrain,is.numeric))
housingNumeric


# Pull out just the numeric fields
houseNum = Filter(is.numeric, housingTrain)
head(houseNum)

# remove variables Id and SalePrice

##############################################################
# True numerical variables
##############################################################

#1  LotFrontage: Linear feet of street connected to property
#2  LotArea: Lot size in square feet
#3  MasVnrArea: Masonry veneer area in square feet
#4  BsmtFinSF1: Type 1 finished square feet
#5  BsmtFinSF2: Type 2 finished square feet
#6  BsmtUnfSF: Unfinished square feet of basement area
#7  TotalBsmtSF: Total square feet of basement area
#8  1stFlrSF: First Floor square feet
#9  2ndFlrSF: Second floor square feet
#10 LowQualFinSF: Low quality finished square feet (all floors)
#11 GrLivArea: Above grade (ground) living area square feet
#12 GarageArea: Size of garage in square feet
#13 WoodDeckSF: Wood deck area in square feet
#14 OpenPorchSF: Open porch area in square feet
#15 EnclosedPorch: Enclosed porch area in square feet
#16 3SsnPorch: Three season porch area in square feet
#17 ScreenPorch: Screen porch area in square feet
#18 PoolArea: Pool area in square feet

houseNum2 = houseNum%>%select(LotArea, MasVnrArea, BsmtFinSF1, BsmtFinSF2, 
                              BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF , 
                              LowQualFinSF, GrLivArea, GarageArea, WoodDeckSF,
                              OpenPorchSF, EnclosedPorch, X3SsnPorch , ScreenPorch,
                              PoolArea)
head(houseNum2)


##############################################################
# Checking and replacing NA's
##############################################################
sum(is.na(houseNum2)) # 8 NA's

# replace missing values with mean
for(i in 1:ncol(houseNum2)){
  houseNum2[is.na(houseNum2[,i]), i] = mean(houseNum2[,i], na.rm = TRUE)
}

sum(is.na(houseNum2)) # Zero NA's
head(houseNum2)


##########################################################
# Testing the correlation matrix for True numericals
##########################################################
# Compute the correlation matrix to see if there is significant 
# correlation to exploit
corTests = cor(houseNum2) 

# Visualize correlation matrix
cor.housing = cor(houseNum2, use="complete.obs") 
corrplot(cor.housing, method="circle", order="AOE") 


houseCorrTest = corr.test(houseNum2, adjust="none")
Mhouse = houseCorrTest$p
MTesthouse = ifelse(Mhouse < .01, T, F) # if Mhouse value < .01 then TRUE else FALSE
colSums(MTesthouse) - 1


######################################################################
# Compute "prcomp" with scaling/correlation matrix
# and determine number of components
######################################################################
prHousing = prcomp(houseNum2, scale=T)
plot(prHousing)           # The scree plot
abline(1, 0, col="red")   # Put in a line at var=1
summary(prHousing)         # Get a summary including variances
print(prHousing, digits=4, format="fg", flag="#")


######################################################################
# use "principal" to compute the Principal Factor Analysis 
# with prHousing number of components, and with varimax factor rotation
######################################################################
# prcomp is what we use to select our components
# principal is used if we need to rotate the components

principalHousing = principal(houseNum2, rotate="varimax", nfactors=7)
# factors determined from prHousing scree-plot
print(principalHousing$loadings, cutoff=.4, sort=T)

# produce a PCA_Plot_Psych plot of the contributions
source("PCA_Plot.R")
PCA_Plot_Psyc(principalTests)
PCA_Plot_Psyc_Secondary(principalTests)     # plot PC3 and PC4


######################################################################
# PRICIPAL FACTOR ANALYSIS with variables removed
######################################################################

housingReduced = houseNum2[, -c(15)] # remove X3SsnPorch
head(housingReduced)

# Recompute the "prcomp" again to determine number of components with variables removed
prHousing2 = prcomp(housingReduced, scale=T)
plot(prHousing2)                     # The scree plot
abline(1, 0, col="red")             # Put in a line at var=1
summary(prHousing2)                  # Get a summary including variances 

# compute the Principal Factor Analysis with 4 factors
principalHousing2 = principal(housingReduced, rotate="varimax", nfactors=5)
print(principalHousing2$loadings, cutoff=.4, sort=T)



##############################################################
##############################################################
##############################################################
##############################################################

# these variables have maximum zero values - remove them

# BsmtFinSF2
# LowQualFinSF
# EnclosedPorch
# 3SsnPorch
# ScreenPorch
# PoolArea

houseNum3 = houseNum%>%select(LotArea, MasVnrArea, BsmtFinSF1,  
                              BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, 
                              GrLivArea, GarageArea, WoodDeckSF, OpenPorchSF)
head(houseNum3)


##############################################################
# Checking and replacing NA's
##############################################################
sum(is.na(houseNum3)) 
# 8 NA's

# replace missing values with mean
for(i in 1:ncol(houseNum3)){
  houseNum3[is.na(houseNum3[,i]), i] = mean(houseNum3[,i], na.rm = TRUE)
}

sum(is.na(houseNum3)) # Zero NA's


##########################################################
# Testing the correlation matrix for reduced data
##########################################################
# Compute the correlation matrix to see if there is significant 
# correlation to exploit
corTests3 = cor(houseNum3) 

# Visualize correlation matrix
cor.housing3 = cor(houseNum3, use="complete.obs") 
corrplot(cor.housing3, method="circle", order="AOE") 


houseCorrTest3 = corr.test(houseNum3, adjust="none")
Mhouse3 = houseCorrTest3$p
MTesthouse3 = ifelse(Mhouse3 < .01, T, F) # if Mhouse3 value < .01 then TRUE else FALSE
colSums(MTesthouse3) - 1


######################################################################
# Compute "prcomp" with scaling/correlation matrix
# and determine number of components
######################################################################
prHousing3 = prcomp(houseNum3, scale=T)
plot(prHousing3)           # The scree plot
abline(1, 0, col="red")   # Put in a line at var=1
summary(prHousing3)         # Get a summary including variances
print(prHousing3, digits=4, format="fg", flag="#")


######################################################################
# use "principal" to compute the Principal Factor Analysis 
# with prHousing number of components, and with varimax factor rotation
######################################################################
# prcomp is what we use to select our components
# principal is used if we need to rotate the components
principalHousing3 = principal(houseNum3, rotate="varimax", nfactors=4)
# factors determined from prHousing scree-plot
print(principalHousing3$loadings, cutoff=.4, sort=T)

# produce a PCA_Plot_Psych plot of the contributions
source("PCA_Plot.R")
PCA_Plot_Psyc(principalHousing3)
PCA_Plot_Psyc_Secondary(principalHousing3)     # plot PC3 and PC4


##################################################################################
# And finally, COMMON FACTOR ANALYSIS and compare the two loadings
##################################################################################

factanalHousing = factanal(houseNum3, 4)
print(factanalHousing$loadings, cutoff=.4, sort=T)



