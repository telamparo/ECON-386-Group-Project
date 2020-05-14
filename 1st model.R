#Training Dataset
df0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/Training%20Data.csv', header = TRUE)
View(df0)
dim(df0)

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

M101 <- lm(price~host_response_rate+accommodates+bathrooms+bedrooms+beds+number_of_reviews+review_scores_rating, df1)
summary(M101)

M102 <- lm(price~accommodates+bathrooms+number_of_reviews, df1)
summary(M102)
