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


M2.5 <- glm(house~bathrooms+bedrooms+price+accommodates+review_scores_rating, data = training, family = "binomial")
summary(M2.5) 
confusionMatrix(table(predict(M2.5, training, type="response") >= 0.5,training$house == 1))
testing$house[testing$property_type=='House'] <- 1
testing$house[testing$property_type!='House'] <- 0
testing$entirespace[testing$room_type=='Entire home/apt'] <- 1
testing$entirespace[testing$room_type!='Entire home/apt'] <- 0
confusionMatrix(table(predict(M2.5, testing, type="response") >= 0.5,testing$house == 1))

confint(M2.5)
M2.5CI<-cbind(M2.5$coefficients, confint(M2.5))
exp(M2.5CI)



