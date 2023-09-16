#Lab 1 - Video Games Data Set
#Names: Jessica Quansah and Ko-Jen Wang
#Date: 18th September, 2023

#Loading Data 
video_games_fall_2023 <- read.csv("/Users/karenwang/PycharmProjects/r-multivariate-analysis/video_games_fall_2023.csv")
attach(video_games_fall_2023)

########################
#                      #
#   QUESTION 1         #
#                      #
########################
#Summary Stats/Visuals on Score
summary(score)
boxplot(score, col ="blue")
hist(score, breaks = 20, col="blue")

#Summary Stats/Visuals on sales
summary(sales_global)
boxplot(sales_global, col ="green")
hist(sales_global, breaks = 100, col="green")

#Summary Stats/Visuals on release year
summary(release_year)
boxplot(release_year, col ="orange")
hist(release_year, breaks = 25, col="orange")

#Summary Stats/Visuals on count critic
summary(count_critic)
boxplot(count_critic, col ="purple")
hist(count_critic, breaks = 10, col="purple")

#Scatter plot matrix of 3 variables
unique_genres <- unique(genre) # Dynamically generate colors for each genre
genre_colors <- rainbow(length(unique_genres))
names(genre_colors) <- unique_genres
par(mfrow=c(1,3))
plot(sales_global, score, col=genre_colors[genre])
plot(count_critic, score, col=genre_colors[genre])
plot(release_year, score, col=genre_colors[genre])
par(mfrow=c(1,1))


########################
#                      #
#   QUESTION 2         #
#                      #
########################

#Linear Model for sales_global
lm1=lm(score~sales_global)
lm1

#Plotting Linear model
plot(sales_global, score)
abline(lm1, col="red")

#Obtaining summary stats on Model
summary(lm1)

#95% Confidence Interval
confint(lm1, 'sales_global',level =0.95)
require(visreg)
visreg(lm1, alpha=0.05)

#Linear Model_release_year
lm2=lm(score~release_year)
lm2

#Plotting Linear model
plot(release_year, score)
abline(lm2, col="red")

#Obtaining summary stats on Model
summary(lm2)

#95% Confidence Interval
confint(lm2, 'release_year',level =0.95)
visreg(lm2, alpha=0.05)



#Linear Model_count_critic
lm3=lm(score~count_critic)
lm3

#Plotting Linear model
plot(count_critic, score)
abline(lm3, col="red")

#Obtaining summary stats on Model
summary(lm3)


#95% Confidence Interval
confint(lm3, 'count_critic',level =0.95)
visreg(lm3, alpha=0.05)




########################
#                      #
#   QUESTION 3         #
#                      #
########################
#Predictions using results from model1 
b0=coef(lm1)[1] # Get intercept
b1=coef(lm1)[2]
b0+b1*0.75  ## prediction for 750,000 sales globally

#Predictions from Model 2
b0=coef(lm2)[1] # Get intercept
b1=coef(lm2)[2]
b0+b1*2009  ## prediction for released in 2009

#Predictions for Model 3
b0=coef(lm3)[1] # Get intercept
b1=coef(lm3)[2]
b0+b1*80  ## prediction for reviewed by 80 critics


########################
#                      #
#   QUESTION 4         #
#                      #
########################
mreg1 = lm(score~sales_global+release_year+count_critic)
summary(mreg1)

b0 = coef(mreg1)[1]
b1 = coef(mreg1)[2]
b2 = coef(mreg1)[3]
b3 = coef(mreg1)[4]

#Prediction of score for 750,000 in global sales, released in 2009, and reviewed by 80 critics.
b0+b1*0.75+b2*2009+b3*80


########################
#                      #
#   QUESTION 5         #
#                      #
########################

#Creating nintendo dummy variable 
video_games_fall_2023$Nintendo =as.factor(ifelse(video_games_fall_2023$publisher =="Nintendo", 1,0))
attach(video_games_fall_2023)

#Regression for Nintendo
mreg2 =lm(score~release_year+Nintendo)
summary(mreg2)

#Scatter Plot
plot(release_year, score,pch =0, col=ifelse(publisher=="Nintendo","green","blue"))
b0 = coef(mreg2)[1]
b1 = coef(mreg2)[2]
b2 = coef(mreg2)[3]
abline(b0+b2 , b1, lwd=2, lty="dashed", col ="green")
abline(b0, b1, lwd=2, lty="dashed",col ="blue")
legend("topright", legend = c("Games published by Nintendo", "Games not published by Nintendo", "Nintendo Regression", "Others Regression"),
       pch = c(0, 0, NA, NA), col = c("green", "blue", "green", "blue"),
       lty = c(NA, NA, "dashed", "dashed"), lwd = c(NA, NA, 2, 2))


########################
#                      #
#   QUESTION 6         #
#                      #
########################
video_games_fall_2023$genre=as.factor(video_games_fall_2023$genre)
attach(video_games_fall_2023)


levels(genre)
table(genre)

video_games_fall_2023$genre =relevel(video_games_fall_2023$genre, ref ="Racing")
attach(video_games_fall_2023)
mreg3 = lm(score~ genre)
summary(mreg3)

########################
#                      #
#   QUESTION 7         #
#                      #
########################
#Create Variable
video_games_fall_2023$strategy =as.factor(ifelse(video_games_fall_2023$genre =="Strategy", 1,0))
attach(video_games_fall_2023)

#Create Regression Model
mreg4 =lm(score~Nintendo +strategy+strategy*Nintendo)
summary(mreg4)

#plotting the graph
mreg5 =lm(score~release_year +Nintendo+release_year*Nintendo)
summary(mreg5)
plot(release_year, score,pch =2, col=ifelse(publisher=="Nintendo","green","blue"))
b0 = coef(mreg5)[1]
b1 = coef(mreg5)[2]
b2 = coef(mreg5)[3]
b3 = coef(mreg5)[4]
abline(b0+b2 , b1+b3, lwd=2, lty="dashed", col ="green")
abline(b0, b1, lwd=2, lty="dashed",col ="blue")
legend("bottomleft", legend = c("Games published by Nintendo", "Games not published by Nintendo", "Nintendo Regression", "Others Regression"),
       pch = c(2, 2, NA, NA), col = c("green", "blue", "green", "blue"),
       lty = c(NA, NA, "dashed", "dashed"), lwd = c(NA, NA, 2, 2))

