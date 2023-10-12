real_estate <- read.csv("/Users/karenwang/PycharmProjects/r-multivariate-analysis/real_estate_lect5.csv")
attach(real_estate)
names(real_estate)
View(real_estate)
library(car)

###Polynomial regression
plot(year, price)
plot(bedrooms, price)
reg1=lm(price~bedrooms+year)
residualPlots(reg1) # P value is really higher, higher than 0.1 -> it is linear variable.
# If the variable is non-linear, we keep it out.

##Running a polynomial model
reg2=lm(price~bedrooms+poly(year, 2))
reg3=lm(price~bedrooms+poly(year, 3))
reg4=lm(price~bedrooms+poly(year, 4))

reg_a=lm(price~poly(year, 1))
reg_b=lm(price~year) # The both results are different, because of multicolleanrity..
reg_c=lm(price~poly(year, 1, raw=TRUE))
summary(reg_a)
summary(reg_b)
summary(reg_c)

install.packages("ggplot2")
library(ggplot2)
require(methods)
#layer #1: Plot environment
plot = ggplot(real_estate, aes(y=price, x=year))
scatter=geom_point() # I want a scatter plot
line_d2=geom_smooth(method="lm", formula=y~poly(x,2)) # I want a plot line
line_d3=geom_smooth(method="lm", formula=y~poly(x,3), color="salmon")
line_d4=geom_smooth(method="lm", formula=y~poly(x,4), color="forest green") 

plot+scatter+line_d2
plot+scatter+line_d3
plot+scatter+line_d4
plot+scatter+line_d2+line_d3+line_d4 # You can add multiple layers

##Choosing the optimal polynomial degree
#Step 1: Choosing models to test (reg1, reg2, reg3, reg4)

#Step 2: Run anova test
anova(reg1, reg2, reg3, reg4)

detach(real_estate)

# Now we are using a new dataset: MontrealTemp
MontrealTemp <- read.csv("/Users/karenwang/PycharmProjects/r-multivariate-analysis/MontrealTemp.csv")
attach(MontrealTemp)

plot=ggplot(MontrealTemp, aes(y=Temperature, x=Day))
scatter=geom_point(color="grey")
line_1=geom_smooth(method="lm", formula=y~x)
line_2=geom_smooth(method="lm", formula=y~poly(x,2))
line_3=geom_smooth(method="lm", formula=y~poly(x,3))
line_9=geom_smooth(method="lm", formula=y~poly(x,9))


plot+scatter+line_9

###Spline regression
library(splines)

reg6=lm(Temperature~bs(Day, knots=c(210, 400, 560), degree=1)) # 3 knots, and 4 lines
summary(reg6)

spline_1=geom_smooth(method="lm", formula=y~bs(x, knots=c(210, 400, 560), degree=1))
plot+scatter+spline_1
# Slope = Rise/Run. The Estimate difference is the Rise, and the y difference is the Run.

##Making predictions with splines

reg6$fitted[200]
reg6$fitted[300]
reg6$fitted[400]
reg6$fitted[500]

reg7=lm(Temperature~bs(Day, knots=c(210, 400, 560), degree=2))
reg8=lm(Temperature~bs(Day, knots=c(210, 400, 560), degree=3))
reg9=lm(Temperature~bs(Day, knots=c(210, 400, 560), degree=4))

spline_1=geom_smooth(method="lm", formula=y~bs(x, knots=c(210, 400, 560), degree=1))
spline_2=geom_smooth(method="lm", formula=y~bs(x, knots=c(210, 400, 560), degree=2))
spline_3=geom_smooth(method="lm", formula=y~bs(x, knots=c(210, 400, 560), degree=3))
spline_4=geom_smooth(method="lm", formula=y~bs(x, knots=c(210, 400, 560), degree=4))

plot+scatter+spline_1
plot+scatter+spline_2
plot+scatter+spline_3
plot+scatter+spline_4

###Placing the knots uniformly

k1=quantile(Day, 0.2)
k2=quantile(Day, 0.4)
k3=quantile(Day, 0.6)
k4=quantile(Day, 0.8)

reg10=lm(Temperature~bs(Day, knots=c(k1, k2, k3, k4), degree=3))

eq_spline=geom_smooth(method="lm", formula=y~bs(x, knots=c(k1, k2, k3, k4), degree=3))
plot+scatter+eq_spline+geom_vline(xintercept=c(k1, k2, k3, k4), linetype="dotted")

###Local regression

reg11=loess(Temperature~Day, span=0.1)
reg12=loess(Temperature~Day, span=0.3)
reg13=loess(Temperature~Day, span=0.5)

localplot1=geom_smooth(method="loess", span=0.1, color="red")
plot+scatter+localplot1





