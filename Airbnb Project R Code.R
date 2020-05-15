##ECON 386 GROUP PROJECT##

#TIDYING THE DATA
df_0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/ECON%20386%20-%20Airbnb%20data%20copy%202.csv', header = TRUE) 
View(df_0)
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
dim(df_7)
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
M1predict <-predict(M1, testing)
View(M1predict) 
M1_RMSE=sqrt(sum((M1predict-testing$price)^2)/(length(testing$price)-7))
M1_RMSE

#MODEL 2 
M2 <- lm(price~0+accommodates+bathrooms+number_of_reviews, training)
summary(M2)
M2predict <- predict(M2, testing)
View(M2predict)
M2_RMSE=sqrt(sum((M2predict-testing$price)^2)/(length(testing$price)-3))
M2_RMSE

#MODEL 3
M3 <- lm(price~accommodates+bedrooms+number_of_reviews+review_scores_rating, training)
summary(M3)
M3predict <- predict(M3,testing)
View(predictions)
M3_RMSE=sqrt(sum((M3predict-testing$price)^2)/(length(testing$price)-5))
M3_RMSE

#MODEL 4
training$grantville[training$neighbourhood=='Grantville']<-1
training$grantville[training$neighbourhood!='Grantville']<-0
View(training$grantville)
training$lajolla[training$neighbourhood=='La Jolla']<-1
training$lajolla[training$neighbourhood!='La Jolla']<-0
View(testing$lajolla)
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
M5 <- lm(price~ accommodates+bathrooms+beds, training)
summary(M5)
M5predict <- predict(M5, testing)
View(M5predict)
M5_RSME=sqrt(sum((M5predict-testing$price)^2)/(length(testing$price)-4))
M5_RSME


