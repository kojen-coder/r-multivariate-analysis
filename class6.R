real_estate <- read.csv("/Users/karenwang/PycharmProjects/r-multivariate-analysis/real_estate_lect6.csv")
attach(real_estate)
View(real_estate)

install.packages("caTools")
require(caTools)
require(splines)
require(methods)
library(ggplot2)

# Since we do not have new data, we use train & test split - this is a resampling technique. We will cover some resampling methods.
plot=ggplot(real_estate, aes(y=price, x=area))
scatter=geom_point()
plot+scatter
# It looks like quadratic

#############################################
############ Validation-set test ############
#############################################
reg1=lm(price~area)
reg2=lm(price~poly(area,2))
reg3=lm(price~poly(area,10))
summary(reg1)
summary(reg2)
summary(reg3)

# Step 1: divide data into two random sets
sample=sample.split(real_estate$price, SplitRatio=0.5) # Sample has true and false
train_set=subset(real_estate, sample==TRUE) # make the "true" sample into train set
test_set=subset(real_estate, sample==FALSE) # make the "false" sample into train set

train_points=geom_point(data=train_set, col="red")
test_points=geom_point(data=test_set, col="grey")
plot+train_points+test_points

# Step 2: Fit model on training data
fit=lm(price~area, data=train_set) # model 1: linear
fit=lm(price~poly(area,2), data=train_set) # model 2: quadratic
fit=lm(price~poly(area,10), data=train_set) # model 3: 10-degree

# Steps 3 and 4: Calculate MSE on test data

actual=test_set$price              # y
prediction=predict(fit, test_set)  # y_hat
squared_error=(actual-prediction)^2  #(y-y_hat)^2
mse=mean(squared_error)
mse

# Note: so far, quadratic performs better among other models as it has the lowest MSE and ME. 
# However, because we have 0.5 ratio of the train and test set, we lost a lot of observations that can be in the train set instead of test set.
# LOOCV One of the observation will be left on -> to be test set


##############################################################
############### Leave-one-out cross-validation  ##############
##############################################################
library(boot)
# Model 1: Linear
fit=glm(price~area, data=real_estate)  # glm is for generalized linear model
mse=cv.glm(real_estate, fit)$delta[1]  # delta[1] means it is for MSE, and delta[2] is MAE
mse

# Model 2: Linear
fit=glm(price~poly(area,2), data=real_estate)  
mse=cv.glm(real_estate, fit)$delta[1] 
mse

# Model 3: 10-degree
fit=glm(price~poly(area,10), data=real_estate)  
mse=cv.glm(real_estate, fit)$delta[1] 
mse

####Automate best model search through loops

mse=rep(NA,5)
for (i in 1:5) {
  fit=glm(price~poly(area,i), data=real_estate)
  mse[i]=cv.glm(real_estate, fit)$delta[1]
}
mse
plot(mse)
lines(mse, col="red")
which.min(mse)
min(mse)


mse=rep(NA,10)
for (i in 1:10) {
  fit=glm(price~poly(area,i), data=real_estate)
  mse[i]=cv.glm(real_estate, fit, K=20)$delta[1]
}
plot(mse)
lines(mse, col="red")
which.min(mse)
min(mse)
# Try K=10 then K=20 to compare the which.min(mse)
# What k should we choose? How can we optimize it?


# LOOCV: very reliable but very slow. K-fold CV: very fast but less reliable
# In this example, we can select from 2 to 90. When k=n, which is 90 in this case, it is LOOCV.

fit=glm(price~area, data=real_estate)
mse=cv.glm(real_estate, fit, K=10)$delta[1]
mse

fit=glm(price~poly(area,2), data=real_estate)
mse=cv.glm(real_estate, fit, K=10)$delta[1]
mse

fit=glm(price~poly(area,10), data=real_estate)
mse=cv.glm(real_estate, fit, K=10)$delta[1]
mse








