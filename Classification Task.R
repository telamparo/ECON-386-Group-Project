#Classification task

Random <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Airbnb-Random-data.csv')
View(Random)
testing <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Testing%20Data.csv', header = TRUE)
View(testing)
training <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Training%20Data.csv', header = TRUE)
View(training)
levels(training$property_type)

training$house[training$property_type=='House'] <- 1
training$house[training$property_type!='House'] <- 0
View(training)

training$entirespace[training$room_type=='Entire home/apt'] <- 1
training$entirespace[training$room_type!='Entire home/apt'] <- 0
View(training)

M2.1 <- glm(house~zipcode+accommodates+price, data = training, family = "binomial")
summary(M2.1)
confusionMatrix(table(predict(M2.1, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.1, testing, type="response") >= 0.5,testing$house == 1))
#dont use zip code


M2.2 <- glm(house~entirespace+bathrooms+bedrooms+beds+accommodates, data = training, family = "binomial")
summary(M2.2)
confusionMatrix(table(predict(M2.2, training, type="response") >= 0.5,training$house == 1))
testing$house[testing$property_type=='House'] <- 1
testing$house[testing$property_type!='House'] <- 0
testing$entirespace[testing$room_type=='Entire home/apt'] <- 1
testing$entirespace[testing$room_type!='Entire home/apt'] <- 0
View(testing)
confusionMatrix(table(predict(M2.2, testing, type="response") >= 0.5,testing$house == 1))

M2.3 <- glm(house~price+entirespace+accommodates, data = training, family = "binomial")
summary(M2.3)
confusionMatrix(table(predict(M2.3, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.3, testing, type="response") >= 0.5,testing$house == 1))

M2.4 <- glm(house~entirespace+price+bedrooms+bathrooms+accommodates, data = training, family = "binomial")
summary(M2.4)
confusionMatrix(table(predict(M2.4, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.4, testing, type="response") >= 0.5,testing$house == 1))

M2.5 <- glm(house~entirespace+accommodates+bathrooms+bedrooms+number_of_reviews+review_scores_rating+host_response_rate, data = training, family = "binomial")
summary(M2.5)
confusionMatrix(table(predict(M2.5, training, type="response") >= 0.5,training$house == 1)) #Ein = 0.2482
confusionMatrix(table(predict(M2.5, testing, type="response") >= 0.5,testing$house == 1)) #Eout = 0.2667


M2.6 <- glm(house~entirespace+bathrooms+bedrooms+review_scores_rating, data = training, family = "binomial")
summary(M2.6)
confusionMatrix(table(predict(M2.6, training, type="response") >= 0.5,training$house == 1))
confusionMatrix(table(predict(M2.6, testing, type="response") >= 0.5,testing$house == 1))

