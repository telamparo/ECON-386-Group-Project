#Exporting data into excel to divide in training/testing sets
install.packages("openxlsx")
library("openxlsx")
write.xlsx(df_7, 'Airbnb-TIDY-data.xlsx') 

Random <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Airbnb-Random-data.csv')
View(Random)
testing <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Testing%20Data.csv', header = TRUE)
View(testing)
training <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Training%20Data.csv', header = TRUE)
View(training)

#Regression Task
M1 <- lm(price~accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating+neighbourhood, training)
summary(M1) #This is the main model we're using to compare all of our individual models to

class(training$neighbourhood)
training$corridor[training$neighbourhood=='Corridor']<-1 #if it's Saturday, code the Sat variable 1
training$corridor[training$neighbourhood!='Corridor']<-0
View(training)
M2 <- lm(price~accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating+corridor, training)
summary(M2)

levels(training$room_type)

#Testing out Regression Models 
M2 <- lm(price~accommodates+bathrooms+bedrooms+beds, training)
summary(M2)
predictions <- predict(M2,testing)
View(predictions)
RMSE=sqrt(sum((predictions-testing$price)^2)/(length(testing$price)-4))
RMSE
# E_IN = 611.8 E_OUT = 619.2654 Difference: 7.4654
M2 <- lm(price~accommodates+bathrooms+bedrooms+beds+review_scores_rating, training)
summary(M2)
predictions <- predict(M2,testing)
View(predictions)
RMSE=sqrt(sum((predictions-testing$price)^2)/(length(testing$price)-5))
RMSE
# E_IN = 611.8 E_OUT = 619.3792 Difference: 7.5792
M2 <- lm(price~accommodates+bathrooms+bedrooms+number_of_reviews+review_scores_rating, training)
summary(M2)
predictions <- predict(M2,testing)
View(predictions)
RMSE=sqrt(sum((predictions-testing$price)^2)/(length(testing$price)-4))
RMSE
# E_IN = 610.8 E_OUT = 618.1663 Difference: 7.3663
M2 <- lm(price~accommodates+bathrooms+bedrooms+review_scores_rating, training)
summary(M2)
predictions <- predict(M2,testing)
View(predictions)
RMSE=sqrt(sum((predictions-testing$price)^2)/(length(testing$price)-4))
RMSE
# E_IN = 611.8 E_OUT = 619.2718 Difference: 7.4718
M2 <- lm(price~accommodates+bedrooms+number_of_reviews+review_scores_rating, training)
summary(M2)
predictions <- predict(M2,testing)
View(predictions)
RMSE=sqrt(sum((predictions-testing$price)^2)/(length(testing$price)-4))
RMSE
# E_IN = 612.5 E_OUT = 619.5536 Difference: 7.0536
hist(M2)
hist(price)
hist(training)
hist(training$price)
hist(testing$price)

##FINAL Model for Regression Task## 
M3 <- lm(price~accommodates+bedrooms+number_of_reviews+review_scores_rating, training)
summary(M3)
M3predict <- predict(M3,testing)
View(predictions)
M3_RMSE=sqrt(sum((M3predict-testing$price)^2)/(length(testing$price)-5))
M3_RMSE
summary(M3$residuals)
hist(M3$residuals,breaks = 30, prob = TRUE)
curve(dnorm(x,mean = 0, sd = sd(M3$residuals)), col = "darkblue", lwd = 2, add = TRUE)
RMSE_IN <- sqrt(sum(M3$residuals^2)/length(M3$residuals))
RMSE_OUT <- sqrt(sum((predict(M3, testing)-testing$price)^2)/length(testing))
RMSE_IN
RMSE_OUT
confint(M3)
# **Model for Classification Task**
training$price<-factor(training$price)
M3.1<- glm(price ~ ., data = training, family = "binomial")
summary(M3.1)
M_LOG<-glm(price ~ accommodates + bedrooms + number_of_reviews + review_scores_rating, data = training, family = "binomial")
summary(M_LOG)
#***FINAL model for classification task*** 
#Training Dataset
df0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Training%20Data.csv', header = TRUE)
View(df0)
dim(df0)

testing <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Testing%20Data.csv', header = TRUE)
View(testing)
dim(testing)

summary(df0)

df1 <- na.omit(df0)
sum(is.na(df1)) 
dim(df1)
View(df1)


hist(df1$accommodates)
hist(df1$bathrooms)
hist(df1$review_scores_rating)
hist(df1$number_of_reviews)
hist(df1$bedrooms)
hist(df1$host_response_rate)
hist(df1$beds)
hist(df1$price)
hist(df1$number_of_reviews)
hist(df1$review_scores_rating)

#testing the relationship between price and other quantitative variables
cor(df1$price, df1$host_response_rate)
cor(df1$price, df1$accommodates)
cor(df1$price, df1$bathrooms)
cor(df1$price, df1$bedrooms)
cor(df1$price, df1$beds)
cor(df1$price, df1$number_of_reviews)
cor(df1$price, df1$review_scores_rating)

summary(lm(price~neighbourhood, df1))
summary(lm(price~property_type, df1))
summary(lm(price~room_type, df1))
summary(lm(price~zipcode, df1))

class(df1$room_type)
df1$privateroom[df1$room_type=='Private room']<-1
df1$privateroom[df1$room_type!='Private room']<-0
View(df1$privateroom)

df1$sharedroom[df1$room_type=='Shared room']<-1
df1$sharedroom[df1$room_type!='Shared room']<-0
View(df1$sharedroom)

class(df1$neighbourhood)
df1$corridor[df1$neighbourhood=='Corridor']<-1
df1$corridor[df1$neighbourhood!='Corridor']<-0
View(df1$corridor)

df1$grantville[df1$neighbourhood=='Grantville']<-1
df1$grantville[df1$neighbourhood!='Grantville']<-0
View(df1$grantville)

df1$lajolla[df1$neighbourhood=='La Jolla']<-1
df1$lajolla[df1$neighbourhood!='La Jolla']<-0
View(df1$lajolla)

df1$missionbeach[df1$neighbourhood=='Mission Beach']<-1
df1$missionbeach[df1$neighbourhood!='Mission Beach']<-0
View(df1$missionbeach)

testing$privateroom[testing$room_type=='Private room']<-1
testing$privateroom[testing$room_type!='Private room']<-0
View(testing$privateroom)

testing$sharedroom[testing$room_type=='Shared room']<-1
testing$sharedroom[testing$room_type!='Shared room']<-0
View(testing$sharedroom)

testing$corridor[testing$neighbourhood=='Corridor']<-1
testing$corridor[testing$neighbourhood!='Corridor']<-0
View(testing$corridor)

testing$missionbeach[testing$neighbourhood=='Mission Beach']<-1
testing$missionbeach[testing$neighbourhood!='Mission Beach']<-0
View(testing$missionbeach)

testing$grantville[testing$neighbourhood=='Grantville']<-1
testing$grantville[testing$neighbourhood!='Grantville']<-0
View(testing$grantville)

testing$lajolla[testing$neighbourhood=='La Jolla']<-1
testing$lajolla[testing$neighbourhood!='La Jolla']<-0
View(testing$lajolla)

M3 <- lm(price~0+accommodates+bedrooms+number_of_reviews+review_scores_rating, df1)
summary(M3)
confint (M3)

Predict3 <- predict(M3, testing)
View(Predict3)

#Logostic Regression predicting Property Type

df1$typehouse[df1$property_type=='House'] <- 1
df1$typehouse[df1$property_type!='House'] <- 0
summary(df1$typehouse)
View(df1$typehouse)

testing$typehouse[testing$property_type=='House'] <- 1
testing$typehouse[testing$property_type!='House'] <- 0
summary(testing$typehouse)
View(testing$typehouse)


Model3 <- glm(typehouse~ accommodates+bedrooms+number_of_reviews+review_scores_rating, df1, family = 'binomial')
summary(Model3)


df1$privateroom[df1$room_type=='Private room'] <- 1
df1$privateroom[df1$room_type!='Private room'] <- 0
View(df1$privateroom)

testing$privateroom[testing$room_type=='Private room'] <- 1
testing$privateroom[testing$room_type!='Private room'] <- 0
View(testing$privateroom)

Model3 <- glm(typehouse~privateroom+accommodates+bedrooms+number_of_reviews+review_scores_rating, df1, family = 'binomial')
summary(Model3)

View(predict(Model3, testing, type="response")) #building the predicion on Model3

install.packages("caret")
library(caret)
install.packages("e1071")

confusionMatrix(table(predict(Model3, df1, type="response") >= 0.5, df1$typehouse == 1))
InSampleError <- 1-0.7546
InSampleError

confusionMatrix(table(predict(Model3, testing, type = "response") >= 0.5, testing$typehouse == 1))

OOSError <- 1-.7417
OOSError


