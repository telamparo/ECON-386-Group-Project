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