real_estate <- read.csv("~/Desktop/MGSC 661 Multivariate Analysis/real_estate.csv")
attach(real_estate)
names(real_estate)
View(real_estate)
# Since there are 95 observations (rows), we will make prediction on each row, and calculate the error (and then calculate the sum of squared errors to find the best b0, b1 ...b95)

## Run regression
mreg=lm(price~bedrooms+area+year)
summary(mreg)
# Note: the older house seems have a higher price compared to newer house. We might need another predictor to determine that, such as neighborhood.

#### Why not running separate simple linear regression?

# Reason #1: We cannot make joint predictions
# Separate regressions
sreg1=lm(price~bedrooms)
sreg2=lm(price~area)
sreg3=lm(price~year)
# Summary of regressions
summary(sreg1)
summary(sreg2)
summary(sreg3)
# Predictions of sreg1, sreg2, and sreg3
b0=coef(sreg1)[1]
b1=coef(sreg1)[2]
b0+b1*7  # 7 bedrooms
  
b0=coef(sreg2)[1]
b1=coef(sreg2)[2]
b0+b1*2500  # 2500 area

b0=coef(sreg3)[1]
b1=coef(sreg3)[2]
b0+b1*2008  # year of 2008 

### Joint prediction using mreg
b0=coef(mreg)[1]
b1=coef(mreg)[2]
b2=coef(mreg)[3]
b3=coef(mreg)[4]

# Prediction for 7-bedroom home, with area=2500, year=2008
b0+b1*7+b2*2500+b3*2008
#Note: We do not run separate simple linear regressions as it will inflate the predictor. It is called omitted variable bias.

####Categorical/Dummy/Factor variables#####

#Step1: Declare variables as factor variables
real_estate$property_type=as.factor(real_estate$property_type)
attach(real_estate) 
# Note: you need to attach again as you have changed the dataset. Taking a screenshot of the dataset.


#Step 2: Explore the categories
levels(property_type) #shows you the categories in the variables
table(property_type) #shows you number of observations in each category


#Step3: run regression with dummy
mreg2=lm(price~bedrooms+property_type)
summary(mreg2)


#Step 4: Plot regression lines
plot(bedrooms, price, col=ifelse(property_type=='house', 'red', 'blue'))  # meet the condition -> True (Red), False (Blue)

b0=coef(mreg2)[1]
b1=coef(mreg2)[2]
b2=coef(mreg2)[3]

abline(b0+b2, b1, col='red') # intercept=b0+b2, slope=b1
abline(b0, b1, col='blue')  # intercept=b0, slope=b1

# Create legend
legend("topright", pch=1, col=c( "red", "blue" ), c( "house", "apartment"))

# There are 4 categories in the season. So we will create 3 variables.
# sales = b0+b1(Spring)+b2(Summer)+b3(Fall) -> 4 equations, including sales=b0 for Winter

### Variables with more than two categories

# Step 1: Declare variable as a dummy
real_estate$neighbourhood=as.factor(real_estate$neighbourhood)
attach(real_estate)
levels(neighbourhood)
# note: by alphabetical order, cote-des-neiges is the excluded category. R will automatically do that for you

### Step 2: Run regression
mreg3=lm(price~bedrooms+neighbourhood)
summary(mreg3)

#Note: to change excluded category, we can use "relevel"

real_estate$neighbourhood=relevel(real_estate$neighbourhood, ref="Lasalle")
attach(real_estate)

mreg4=lm(price~bedrooms+neighbourhood) # regression mreg3 with new excluded category "Lasalle"
summary(mreg4)

### Interaction terms / variables

mreg5=lm(price~bedrooms+property_type+bedrooms*property_type)
summary(mreg5)

b0=coef(mreg5)[1]
b1=coef(mreg5)[2]
b2=coef(mreg5)[3]
b2=coef(mreg5)[4]

plot(bedrooms, price, col=ifelse(property_type=="house", "red", "blue"))
abline(b0+b2, b1+b3, col='red') # intercept=b0+b2, slope=b1
abline(b0, b1, col='blue')  # intercept=b0, slope=b1




