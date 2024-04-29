####################################################################
####################################################################
###                   Case Econometrics                          ###
### Bachelor of Science in Economics and Business Engineering    ###
###         Academic year 2023-24 Ghent University               ###
####################################################################
####################################################################


rm(list = ls())   # Clear workspace 

####################################################################
## Set input file directory: change the path to your own pc directory
## !! Note: EconometricsUGent package and dataset are saved in this 
## input file directory
####################################################################

## Rstudio installed on your pc
## For Windows
setwd("C:\\Users\\Milan\\OneDrive - UGent\\3de bach HIR\\Econometrics\\Rcode")

## For Mac
## setwd("/users/ymeersch/Desktop")

## RStudio through Athena, path corresponds to the desktop or other folder on your H-drive
## For Windows
## setwd("\\Desktop")

## For Mac
## setwd("/Desktop")

####################################################################
## Set output file directory: change the path to your own pc directory
## The output tables are created in this directory
####################################################################

## Rstudio installed on your pc
## For Windows
output="C:\\Users\\Milan\\OneDrive - UGent\\3de bach HIR\\Econometrics\\Rcode"

## For Mac
## output="/users/ymeersch/Desktop"

## RStudio through Athena, path corresponds to the desktop or other folder on your H-drive 
## For Windows
## output = "\\Desktop"

## For Mac
## output = "/Desktop"

####################################################################
## Install required packages: this only needs to be done the first time
## before you use these packages
####################################################################
if(!require(pastecs)){install.packages("pastecs")}
if(!require(psych)){install.packages("psych")}
if(!require(moments)){install.packages("moments")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(sandwich)){install.packages("sandwich")}
if(!require(AER)){install.packages("AER")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(nlme)){install.packages("nlme")}
if(!require(orcutt)){install.packages("orcutt")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(fastDummies)){install.packages("fastDummies")}
if(!require(datawizard)){install.packages("datawizard")}

####################################################################
## To install the EconometricsUGent-package, execute the following steps:
## 1. Copy-paste the EconometricsUGent_1.0.tar.gz file to your desktop
## 2. Change the part "C:\\users\\ymeersch\\Desktop" in the code below
##    so that it corresponds with your pc directory to the desktop
####################################################################

## Rstudio installed on your pc
## For Windows
install.packages("C:\\Users\\Milan\\OneDrive - UGent\\3de bach HIR\\Econometrics\\EconometricsUGent_1.0.tar.gz", source = TRUE, repos = NULL)

## For Mac
## install.packages("/users/ymeersch/Desktop/EconometricsUGent_1.0.tar.gz", type="source", repos = NULL)

## RStudio through Athena
## For Windows
## install.packages("H://Desktop//EconometricsUGent_1.0.tar.gz", type="source", repos = NULL, lib = output) 

## For Mac
## install.packages("H:/Desktop/EconometricsUGent_1.0.tar.gz", type="source",repos = NULL, lib = output) 


####################################################################
## Load required packages: this needs to be done every time you
## close an R-session
####################################################################
library(pastecs)     ## Descriptive Statistics
library(psych)       ## Correlation plot
library(moments)     ## Test for Normality
library(lmtest)      ## Test for heteroscedasticity
library(sandwich)    ## Newey-West HAC estimators
library(AER)         ## 2SLS
library(stargazer)   ## Stargazer
library(orcutt)      ## Cochrane-Orcutt EGLS
library(nlme)        ## Linear models with autocorrelated error terms
library(openxlsx)    ## To read an excel xlsx data file
library(fastDummies) ## Create dummies based on different categories in a variable
library(datawizard)  ## Data transformations (e.g. demeaning by group)
library(EconometricsUGent)  ## Additional functions


############################################
## Load data from csv file
############################################
myDataRaw <- read.table("C:\\Users\\Milan\\OneDrive - UGent\\3de bach HIR\\Econometrics\\Rcode\\Data.csv", header = TRUE, sep = ",")
head(myDataRaw)

####################################################################
## Import data set
####################################################################

data <- read.csv("C:\\Users\\Milan\\OneDrive - UGent\\3de bach HIR\\Econometrics\\Rcode\\Data.csv")

model <- lm(dy ~ D + dy_L1, data = myDataRaw)
summary(model) 
stargazerRegression(model, fileDirectory = output, fileName = "FirstRegression")
library(ggplot2) 

if (!require(corrplot)) {   
  install.packages("corrplot")   
  library(corrplot) 
  } 
# Descriptive Statistics 
desc_stats <- stat.desc(myDataRaw) 
desc_stats 
# Covariance Matrix 
cov_matrix <- cov(myDataRaw) 
cov_matrix 
# Correlation Matrix 
cor_matrix <- cor(myDataRaw) 
cor_matrix 
# Plot histograms for all variables 
par(mfrow=c(2,2)) # Change layout to 2x2 
for (i in 1:ncol(myDataRaw)) {   
  hist(myDataRaw[,i], main=colnames(myDataRaw)[i], xlab="", col="lightblue") 
  } 
# Correlation Plot 
corrplot(cor_matrix, method="color") 
# Scatterplot Matrix 
pairs(myDataRaw) 
# You can customize and save these plots as needed 
# For example, saving a correlation plot 
png(file = "correlation_plot.png", width = 800, height = 800) 
corrplot(cor_matrix, method="color") 
#Multicorrelation
vif (model)
stargazer(vif(model), fileDirectory = output, filename = "VIF")
#Hetereoscedasticity 
#Informal residual model
residuals <- resid(model)
variable <- myDataRaw$D 
#Plot residuals against the selected variable 
plot(variable, residuals,main = "Residuals vs. Variable Value",xlab = "D",ylab = "Residuals",col = "blue",pch = 20) 
# Add a horizontal line at y = 0 for reference 
abline(h = 0, col = "red", lty = 2)
variable <- myDataRaw$dy_L1 
# Plot residuals against the selected variable 
plot(variable, residuals,main = "Residuals vs. Variable Value",xlab = "dy_L1",ylab = "Residuals",col = "blue",pch = 20) 
# Add a horizontal line at y = 0 for reference 
abline(h = 0, col = "red", lty = 2)
#bp test
bp_test_result <- bptest(model)
print(bp_test_result)
bp_test_result2 <- bptest(model, studentize = FALSE)
print(bp_test_result2)
#WHITE
myDataRaw$residuals_squared <- residuals^2 
white_model <- lm(residuals_squared ~ D + dy_L1 + I(D^2) + I(dy_L1^2) + D * dy_L1, data = myDataRaw)
white_model
#Autocorrelation
##Grapical method

res <- model$residuals
par(mar = c(2,2,0,2)) 

plot(100*res, type = "l", ylim = c(-100,100))
abline(h = seq(-100, 100, 20), col = "blue", lty = 2)
plot(myDataRaw$Year,res)


