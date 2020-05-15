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
RMSE101=sqrt(sum((prediction101-testing$price)^2)/(length(testing$price)))
RMSE101

RMSE102=sqrt(sum((prediction102-testing$price)^2)/(length(testing$price)))
RMSE102

RMSE103=sqrt(sum((prediction103-testing$price)^2)/(length(testing$price)))
RMSE103

RMSE104=sqrt(sum((prediction104-testing$price)^2)/(length(testing$price)))
RMSE104 

RMSE105=sqrt(sum((prediction105-testing$price)^2)/(length(testing$price)))
RMSE105

RMSE106=sqrt(sum((prediction106-testing$price)^2)/(length(testing$price)))
RMSE106  #lowest (E_out - E_in), also lowest E_out (615.03 - 610.4)
