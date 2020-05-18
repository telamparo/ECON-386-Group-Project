#Training Dataset
df0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Training%20Data.csv', header = TRUE)
View(df0)
dim(df0)

testing <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Testing%20Data.csv', header = TRUE)
View(testing)
dim(testing)

summary(df0)

df1 <- na.omit(df0)
sum(is.na(df1)) #to check if there is any NAs in the dataset
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

#testing the relationship between Price and other quantitative var
cor(df1$price, df1$host_response_rate)
cor(df1$price, df1$accommodates)
cor(df1$price, df1$bathrooms)
cor(df1$price, df1$bedrooms)
cor(df1$price, df1$beds)
cor(df1$price, df1$number_of_reviews)
cor(df1$price, df1$review_scores_rating)

M101 <- lm(price~0+host_response_rate+accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating, df1)
summary(M101)
confint(M101)

M102 <- lm(price~accommodates+bathrooms+number_of_reviews, df1)
summary(M102)
confint(M102)

M103 <- lm(price~0+accommodates+bathrooms+number_of_reviews, df1)
summary(M103)
confint(M103)

M104 <- lm(price~0+log(accommodates)+log(number_of_reviews), df1)
summary(M104)
confint(M104)

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

M105 <- lm(price~0+accommodates+bathrooms+number_of_reviews+missionbeach+corridor, df1)
summary(M105)

M106 <- lm(price~0+accommodates+bathrooms+number_of_reviews+grantville+lajolla, df1)
summary(M106)

confint(M106)

prediction101 <- predict(M101, testing)
View(prediction101)

prediction102 <- predict(M102, testing)
View(prediction102)

prediction103 <- predict(M103, testing)
View(prediction103)

prediction104 <- predict(M104, testing)
View(prediction104)

prediction105 <- predict(M105, testing)
View(prediction105)

prediction106 <- predict(M106, testing)
View(prediction106)

##CALCULATE ROOT MEAN SQUARE PREDICTION ERROR ON TEST DATA: THE OUT-OF-SAMPLE ERROR MEASURE
RMSE101=sqrt(sum((prediction101-testing$price)^2)/(length(testing$price)-5))
RMSE101

RMSE102=sqrt(sum((prediction102-testing$price)^2)/(length(testing$price)-5))
RMSE102

RMSE103=sqrt(sum((prediction103-testing$price)^2)/(length(testing$price)-5))
RMSE103

RMSE104=sqrt(sum((prediction104-testing$price)^2)/(length(testing$price)-5))
RMSE104 

RMSE105=sqrt(sum((prediction105-testing$price)^2)/(length(testing$price)-5))
RMSE105

RMSE106=sqrt(sum((prediction106-testing$price)^2)/(length(testing$price)-5))
RMSE106  #lowest (E_out - E_in), also lowest E_out (615.03 - 610.4)

generalization_error <- 615.59-610.4
generalization_error

#Logostic Regression predicting Property Type

df1$typehouse[df1$property_type=='House'] <- 1
df1$typehouse[df1$property_type!='House'] <- 0
summary(df1$typehouse)
View(df1$typehouse)

testing$typehouse[testing$property_type=='House'] <- 1
testing$typehouse[testing$property_type!='House'] <- 0
summary(testing$typehouse)
View(testing$typehouse)

LM101 <- glm(typehouse~host_response_rate+neighbourhood+zipcode+room_type+accommodates+bathrooms+bedrooms+beds+price+number_of_reviews+review_scores_rating , df1, family = "binomial")
summary(LM101)

LM102 <- glm(typehouse~room_type+bathrooms+host_response_rate, df1, family = "binomial")
summary(LM102) 

df1$privateroom[df1$room_type=='Private room'] <- 1
df1$privateroom[df1$room_type!='Private room'] <- 0
View(df1$privateroom)

testing$privateroom[testing$room_type=='Private room'] <- 1
testing$privateroom[testing$room_type!='Private room'] <- 0
View(testing$privateroom)


#the best model I came up with
LM103 <- glm(typehouse~privateroom+bathrooms+host_response_rate+bedrooms, df1, family = "binomial")
summary(LM103) 

View(predict(LM103, testing, type="response")) #building the predicion on LM103

install.packages("caret")
library(caret)
install.packages("e1071")

confusionMatrix(table(predict(LM103, df1, type="response") >= 0.5, df1$typehouse == 1)) 

confusionMatrix(table(predict(LM103, testing, type = "response") >= 0.5, testing$typehouse == 1))

##the below code is irrelevant to the project
df1$catbathrooms <- factor(df1$bathrooms) 
class(df1$catbathrooms)

testing$catbathrooms <- factor(testing$bathrooms) 
class(testing$catbathrooms)

LM104 <- glm(property_type~hotelroom+catbathrooms+host_response_rate, df1, family = "binomial")
summary(LM104) #changing the bathrooms var to categorical, the results show that the var is insignificant

class(df1$host_response_rate)
df1$cathost_response_rate <- factor(df1$host_response_rate)
class(df1$cathost_response_rate)

LM105 <- glm(property_type~hotelroom+bathrooms+cathost_response_rate, df1, family = "binomial")
summary(LM105) #changing the host_reponse_rate to ccategorocal, the results show that the var is insignificant
