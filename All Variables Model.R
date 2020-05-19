df_0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/ECON%20386%20-%20Airbnb%20data%20copy%202.csv', header = TRUE) 
View(df_0)
sum(is.na(df_0$square_feet)) 
df_1 <- df_0  ##this is without square feet
df_1$square_feet = NULL 
dim(df_1)
install.packages("openxlsx")
Yes
library("openxlsx")
write.xlsx(df_1, 'Airbnb-TIDY-data.xlsx')   ##changed this to df_1 instead of df_7
M1 <- lm(price~accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating, training)
summary(M1)
M1predict <-predict(M1, testing)
View(M1predict) 
M1_RMSE=sqrt(sum((M1predict-testing$price)^2)/(length(testing$price)-7))
M1_RMSE
confint(M1)
