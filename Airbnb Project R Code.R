##ECON 386 GROUP PROJECT- SAN DIEGO AIRBNBS##

#TIDYING THE DATA
df_0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/ECON%20386%20-%20Airbnb%20data%20copy%202.csv', header = TRUE) 
View(df_0)
dim(df_0)
sum(is.na(df_0$square_feet)) 
df_1 <- df_0
df_1$square_feet = NULL 
dim(df_1) 
df_2 <- df_1
df_2$name <- df_2$host_is_superhost <- df_2$host_identity_verified <- df_2$is_location_exact <- NULL
dim(df_2)
View(df_2)
df_3 <- df_2
df_3$number_of_reviews_ltm <- df_3$reviews_per_month <- NULL
View(df_3)
is.na(df_3)
df_4 <- df_3
df_4 <- subset(df_3, host_response_rate!='NA' & review_scores_rating!='NA') 
View(df_4)
is.na(df_4) 
df_5 <- subset(df_4, host_response_rate!='N/A') 
df_6 <- subset(df_5, neighbourhood!='')
View(df_6)
df_6[df_6==""] <- NA
is.na(df_6)
df_7 <- na.omit(df_6)
is.na(df_7)
View(df_7)

#EXPLORATORY ANALYSIS
View(df_7)
dim(df_7)
cor(df_7[,7:13])
cov(df_7[, 7:13])
summary(df_7)

#TRAINING/TESTING PARTITION
install.packages("openxlsx")
library("openxlsx")
write.xlsx(df_7, 'Airbnb-TIDY-data.xlsx') 
Random <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Airbnb-Random-data.csv')
View(Random)
testing <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Testing%20Data.csv', header = TRUE)
View(testing)
training <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Training%20Data.csv', header = TRUE)
View(training)
cov(training[,7:13])
cor(training[,7:13])


#REGRESSION TASK
#MODEL 1 
M1 <- lm(price~accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating, training)
summary(M1)
confint(M1)
M1predict <-predict(M1, testing)
View(M1predict) 
M1_RMSE=sqrt(sum((M1predict-testing$price)^2)/(length(testing$price)-7))
M1_RMSE

#MODEL 2 
M2 <- lm(price~0+accommodates+bathrooms+number_of_reviews, training)
summary(M2)
confint(M2)
M2predict <- predict(M2, testing)
View(M2predict)
M2_RMSE=sqrt(sum((M2predict-testing$price)^2)/(length(testing$price)-3))
M2_RMSE

#MODEL 3
M3 <- lm(price~0+accommodates+bedrooms+number_of_reviews+review_scores_rating, training)
summary(M3)
M3predict <- predict(M3,testing)
View(predictions)
M3_RMSE=sqrt(sum((M3predict-testing$price)^2)/(length(testing$price)-4))
M3_RMSE

#MODEL 4
training$grantville[training$neighbourhood=='Grantville']<-1
training$grantville[training$neighbourhood!='Grantville']<-0
View(training$grantville)
training$lajolla[training$neighbourhood=='La Jolla']<-1
training$lajolla[training$neighbourhood!='La Jolla']<-0
View(training$lajolla)
M4 <- lm(price~0+accommodates+bathrooms+number_of_reviews+grantville+lajolla, training)
summary(M4)
testing$grantville[testing$neighbourhood=='Grantville']<-1
testing$grantville[testing$neighbourhood!='Grantville']<-0
View(testing$grantville)
testing$lajolla[testing$neighbourhood=='La Jolla']<-1
testing$lajolla[testing$neighbourhood!='La Jolla']<-0
View(testing$lajolla)
M4predict <- predict(M4, testing)
View(M4predict)
M4_RMSE=sqrt(sum((M4predict-testing$price)^2)/(length(testing$price)-5))
M4_RMSE 

#MODEL 5
training$House[training$property_type=='House']<-1
training$House[training$property_type!='House']<-0
View(training)
M5 <- lm(price~0+number_of_reviews+accommodates+bathrooms+House, training)
summary(M5)
testing$House[testing$property_type=='House']<-1
testing$House[testing$property_type!='House']<-0
View(testing)
M5predict<-predict(M5, testing)
View(M5predict)
M5_RMSE=sqrt(sum((M5predict-testing$price)^2)/(length(testing$price)-4))
M5_RMSE

#MODEL 6
training$Privateroom[training$room_type=='Private room']<-1
training$Privateroom[training$room_type!='Private room']<-0
View(training$Privateroom)
training$Sharedroom[training$room_type=='Shared room']<-1
training$Sharedroom[training$room_type!='Shared room']<-0
View(training$Sharedroom)
M6<- lm(price~0+Privateroom+Sharedroom+accommodates+bathrooms+number_of_reviews, training)
summary(M6) 
testing$Privateroom[testing$room_type=='Private room']<-1
testing$Privateroom[testing$room_type!='Private room']<-0
View(testing$Privateroom)
testing$Sharedroom[testing$room_type=='Shared room']<-1
testing$Sharedroom[testing$room_type!='Shared room']<-0
View(testing$Sharedroom)
M6predict <- predict(M6, testing)
View(M6predict)
M6_RMSE=sqrt(sum((M6predict-testing$price)^2)/(length(testing$price)-5))
M6_RMSE

#CLASSIFICATION TASK
training$house[training$property_type=='House'] <- 1
training$house[training$property_type!='House'] <- 0
View(training)
testing$house[testing$property_type=='House'] <- 1
testing$house[testing$property_type!='House'] <- 0
View(testing)

#MODEL 1
M2.1 <- glm(house~bathrooms+bedrooms+price+accommodates+review_scores_rating, data = training, family = "binomial")
summary(M2.1)

M2.1CI<-cbind(M2.1$coefficients, confint(M2.1))
exp(M2.1CI)

confusionMatrix(table(predict(M2.1, data = training, type="response") >= 0.5, training$house == 1)) 
confusionMatrix(table(predict(M2.1, data = testing, type="response") >= 0.5, testing$house == 1)) 


#MODEL 2
training$entirespace[training$room_type=='Entire home/apt'] <- 1
training$entirespace[training$room_type!='Entire home/apt'] <- 0
View(training)
testing$entirespace[testing$room_type=='Entire home/apt'] <- 1
testing$entirespace[testing$room_type!='Entire home/apt'] <- 0
View(testing)

M2.2 <- glm(house~entirespace+accommodates+bathrooms+bedrooms+number_of_reviews+review_scores_rating+host_response_rate, data = training, family = "binomial")
summary(M2.2)

M2.2CI<-cbind(M2.2$coefficients, confint(M2.2))
exp(M2.2CI)

confusionMatrix(table(predict(M2.2, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.2, testing, type="response") >= 0.5,testing$house == 1)) 

#MODEL 3
training$privateroom[training$room_type=='Private room'] <- 1
training$privateroom[training$room_type!='Private room'] <- 0
testing$privateroom[testing$room_type=='Private room'] <- 1
testing$privateroom[testing$room_type!='Private room'] <- 0

M2.3 <- glm(house~privateroom+accommodates+bedrooms+number_of_reviews+review_scores_rating, data = training, family = 'binomial')
summary(M2.3)

M2.3CI<-cbind(M2.3$coefficients, confint(M2.3))
exp(M2.3CI)

confusionMatrix(table(predict(M2.3, training, type="response") >= 0.5, training$house == 1))
confusionMatrix(table(predict(M2.3, testing, type = "response") >= 0.5, testing$house == 1))


#MODEL 4
M2.4 <- glm(house~privateroom+bathrooms+host_response_rate+bedrooms, data = training, family = "binomial")
summary(M2.4) 

M2.4CI<-cbind(M2.4$coefficients, confint(M2.4))
exp(M2.4CI)

confusionMatrix(table(predict(M2.4, data = training, type="response") >= 0.5, training$house == 1)) 
confusionMatrix(table(predict(M2.4, data = testing, type = "response") >= 0.5, testing$house == 1))

#MODEL 5
M2.5 <- glm(house~review_scores_rating+number_of_reviews+bedrooms+price, data = training, family = "binomial")
summary(M2.5)

M2.5CI<-cbind(M2.5$coefficients, confint(M2.5))
exp(M2.5CI)

confusionMatrix(table(predict(M2.5, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.5, testing, type="response") >= 0.5,testing$house == 1))


#MODEL 6
M2.6 <- glm(house~accommodates+price+bathrooms+bedrooms, data = training, family = "binomial")
summary(M2.6)

M2.6CI<-cbind(M2.6$coefficients, confint(M2.6))
exp(M2.6CI)

confusionMatrix(table(predict(M2.6, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.6, testing, type="response") >= 0.5,testing$house == 1))

