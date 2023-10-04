real_estate <- read.csv("/Users/karenwang/PycharmProjects/r-multivariate-analysis/real_estate_lect4.csv")
attach(real_estate)
names(real_estate)
View(real_estate)

install.packages("car")

## Analyzing significance

reg1=lm(price~bedrooms+area+property_type)
summary(reg1)

# While the t-values increase, the p-value should decrease and small.
# F-statistics: 3 means 3 predictors, 91 DF means 95 observations - 4 (such as b0, b1, b2, b3) 
# F-statistics p-value not significance as a whole. It should not be high probablity.
# Difference between significance (P-value) and how well the model make prediction (R squared)
# Adjusted R-squared: We compare it with two models. It helps us know which one should we include in our model, and which one should we leave out?

##adjusted r-squared

#Let's start with a simple model, with bedrooms as only preditor (reg2)
reg2=lm(price~bedrooms)
summary(reg2)
#r2=21.22%, adj r2=20.37%

#let's add an extra variable to reg2 (area)
reg3=lm(price~bedrooms+area)
summary(reg3)
#r2=23.54%, adj r2=21.88%
#r2 always increases, but we do not know if the included predictor is effective, so we need to check adjusted R-squared.

yolo_reg=lm(price~bedrooms+ID)
summary(yolo_reg)
#r2=21.43%, adj r2=19.72%
#R2 increases a tiny bit, the adj r2 decrease. So we can conclude that we will drop the ID variable since ID is not so valuable.
#Now we know that what makes a model significance, and what predictors makes the model powerful
#If we care about vaccine, we care about significance not prediction. However, in real estate market, we care about prediction not significance.

#We must be careful that our predictors are not biasing the coefficients, and standard errors

#################################################
### The four main issues in linear regression####
#################################################

#Issue 1: Non-linearity of predictors
library(car)
reg4=lm(price~area+year+bathrooms+bedrooms)
residualPlot(reg4) # In reality, we want the residual as close to 0 as possible. The graph shows that it is not quite linear. as there are some negative from 8000 to 1000, and some positive in that range too.
residualPlots(reg4)
# area is the best. year is the worst
# We do not want to see dot or stars - they are bad. year higher than 0.1 is bad.

## variable year is not linear, Tukey test shows that entire model is not linear

#Solution: let's rerun the model without "year" (in reg5)

reg5=lm(price~area+bathrooms+bedrooms)
residualPlots(reg5)

### Issue 2: Heteroskedasticity

# Step 1: Detect heteroskedasticity visually (funnel test)
reg6=lm(price~area+year+bedrooms)
residualPlot(reg6, quadratic=FALSE) # In the residual plat, we see a funnel shape that means bad. The variance might increase or decrease along the horizontal axis.
# Note: When variance of errors is not constant, linear regresion will give biased standard errors.
# Step 2: Detect heteroskedasticity numerically (Non-constant variance test)
ncvTest(reg6) # p-value < 0.05. It is not good, we get the wrong p-value
# Step 3: Correct for heteroskedasticity
install.packages("lmtest")
install.packages("plm")
require(lmtest)
require(plm)

summary(reg6)
coeftest(reg6, vcoc=vcovHC(reg6, type="HC1"))

### Issue 3: Outliers #######

## Step 1: Identify outliers visually (studentized residual plot)

reg7=lm(price~area+year+bedrooms)
qqPlot(reg7, envelope=list(style="none"))

## Step 2: Bonferroni outlier test

outlierTest(reg7)

## Step 3: Return regression without outliers
real_estate2=real_estate[-c(29,74),] #c means concatenate. Remove 29 and 74. If it is [-c(29,74),-c(1)] it means 1 column 

reg8=lm(price~area+year+bedrooms, data=real_estate2)
summary(reg8)

### Issue 4: Collinearity 
# two predictors that are highly correlated.

install.packages("psych")
require(psych)

## Step 1: Detecting collinear variables through correlation matrix

quantvars=real_estate[, c(3, 6, 7, 8, 9, 10, 12)]
corr_matrix=cor(quantvars)
round(corr_matrix, 2)

## Step 2: Variance Inflation Factor (VIF) test 
# Is VIF > 4, that means there is a sign of collinearity

reg9=lm(price~bedrooms+bathrooms+area+area_with_garden+Levels+year)
vif(reg9) # We see that area, area_with_garden are highly correlated.

reg10=lm(price~bedrooms+bathrooms+area+Levels+year) # We remove area_with_garden
vif(reg10) # By removing the area_with_garden, the coeffcient will be balanced out (the positive / negative).













