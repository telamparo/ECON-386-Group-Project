M1 <- lm(price~accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating+neighbourhood, training)
summary(M1) #This is the main model we're using to compare all of our individual models to

M2 <- lm(price~accommodates+bathrooms+bedrooms+number_of_reviews+review_scores_rating+neighbourhood, training)
summary(M2) #This is the main model we're using to compare all of our individual models to

M3 <- lm(price~accommodates+bathrooms+number_of_reviews+review_scores_rating+neighbourhood, training)
summary(M3) #This is the main model we're using to compare all of our individual models to

M4 <- lm(price~accommodates+bathrooms+number_of_reviews+neighbourhood, training)
summary(M4) #This is the main model we're using to compare all of our individual models to

M5 <- lm(price~accommodates+bathrooms+number_of_reviews, training)
summary(M5) #This is the main model we're using to compare all of our individual models to
M5

M6 <- lm(price~accommodates, training)
summary(M6) #This is the main model we're using to compare all of our individual models to
M6

M7 <- lm(price~accommodates+bathrooms+log(number_of_reviews)+neighbourhood, training)
summary(M7) #This is the main model we're using to compare all of our individual models to

M8 <- lm(price~accommodates+bathrooms+log(number_of_reviews), training)
summary(M8) #This is the main model we're using to compare all of our individual models to
M8

M9 <- lm(price~bathrooms, training)
summary(M9) #This is the main model we're using to compare all of our individual models to
M9

M10 <- lm(price~accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating, training)
summary(M1) #This is the main model we're using to compare all of our individual models to

cov(training[,7:11])
cor(training[,7:11])

RMSE_IN<-sqrt(sum(M5$residuals^2)/length(M5$residuals))  #compute in-sample error by RMSE on the fitted values
RMSE_OUT<-sqrt(sum((predict(M5, testing)-testing$price)^2)/length(testing)) #estimate out-of-sample error by comparing the predicted values on the test set to the actual values in the test set
RMSE_IN  #report E_IN
RMSE_OUT #report E_OUT

