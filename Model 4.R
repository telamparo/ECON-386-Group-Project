cov(training[,7:11])
cor(training[,7:11])

M10 <- lm(price~room_type, training)
summary(M10) #This is the main model we're using to compare all of our individual models to
M10

training$Privateroom[training$room_type=='Private room']<-1
training$Privateroom[training$room_type!='Private room']<-0
View(training$Privateroom)

training$Sharedroom[training$room_type=='Shared room']<-1
training$Sharedroom[training$room_type!='Shared room']<-0
View(testing$Sharedroom)

M11 <- lm(price~Privateroom+Sharedroom+accommodates+bathrooms+number_of_reviews, training)
summary(M11) #This is the main model we're using to compare all of our individual models to
M11

testing$Privateroom[testing$room_type=='Private room']<-1
testing$Privateroom[testing$room_type!='Private room']<-0
View(testing$Privateroom)

testing$Sharedroom[testing$room_type=='Shared room']<-1
testing$Sharedroom[testing$room_type!='Shared room']<-0
View(testing$Sharedroom)

M11predict <- predict(M11, testing)
View(M11predict)
M11_RMSE=sqrt(sum((M11predict-testing$price)^2)/(length(testing$price)-5))
M11_RMSE

RMSE_IN<-sqrt(sum(M11$residuals^2)/length(M11$residuals))  #compute in-sample error by RMSE on the fitted values
RMSE_OUT<-sqrt(sum((predict(M11, testing)-testing$price)^2)/length(testing)) #estimate out-of-sample error by comparing the predicted values on the test set to the actual values in the test set
RMSE_IN  #report E_IN
RMSE_OUT #report E_OUT

