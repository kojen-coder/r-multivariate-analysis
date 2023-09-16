real_estate=read.csv("/Users/karenwang/PycharmProjects/r-multivariate-analysis/real_estate.csv")
attach(real_estate)

names(real_estate) # Show names of variables
View(real_estate) # view the dataset in a spreadsheet form

############################
### visualizing variable ###
############################

## visualizing variable independently
# Summary statistics
summary(price)
summary(bedrooms)
# Note: When the 1st quantile is 290. This means that if you have 290.000, there are 25% of the houses you can buy.

## Box plot
boxplot(price)
boxplot(bedrooms)

## histogram
hist(price)
hist(bedrooms)


# visualizing relationships between variables 
plot(bedrooms, price)  # for scatter plot, you first put x then y: plot(x, y)

###################################
### Running a linear regression ###
###################################

lm.fit=lm(price~bedrooms) # store it as lm.fit
lm.fit
abline(lm.fit, col="red") # adds the regression line to the graph

## Making predictions
b0=coef(lm.fit)[1] # Get the first element from coef : intercept
b1=coef(lm.fit)[2] # Get the second element from coef: slope

b0+b1*2  ## prediction for 2-bedroom home
b0+b1*3  ## prediction for 3-bedroom home
b0+b1*4  ## prediction for 4-bedroom home

## How much can we trust this prediction?
# Standard error: to find standard error, you type summary()
summary(lm.fit)

## confidence interval
confint(lm.fit, 'bedrooms', level = 0.90) # 90% confidence interval
confint(lm.fit, 'bedrooms', level = 0.95) # 95% confidence interval
confint(lm.fit, 'bedrooms', level = 0.99) # 99% confidence interval
# Note: Try to describe the data to a manager of what confidence interval mean here

## visualizing confidence intervals
install.packages("visreg") ## install package (only to run once per device)
require(visreg)
visreg(lm.fit, alpha=0.05) ## 95% confidence interval (alpha=1-0.95)

par(mfrow=c(2,3)) # matrix of plots (2 rows, 3 columns)
visreg(lm.fit, alpha=0.40) ## 60% confidence interval
visreg(lm.fit, alpha=0.30) ## 70% confidence interval
visreg(lm.fit, alpha=0.20) ## 80% confidence interval
visreg(lm.fit, alpha=0.10) ## 90% confidence interval
visreg(lm.fit, alpha=0.05) ## 95% confidence interval
visreg(lm.fit, alpha=0.01) ## 99% confidence interval
par(mfrow=c(1,1))  # reset the environment for plots

### To get r-squared, you type summary()
summary(lm.fit)




