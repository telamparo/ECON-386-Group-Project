#ECON 386 GROUP PROJECT: TIDYING THE DATA

df_0 <- read.csv('https://raw.githubusercontent.com/telamparo/ECON-386-Group-Project/master/ECON%20386%20-%20Airbnb%20data%20copy%202.csv', header = TRUE) 
View(df_0)
sum(is.na(df_0$square_feet)) #13569 of observations have NA value for this variable, so we'll take this out for now
df_1 <- df_0
df_1$square_feet = NULL #delete square_feet b/c too many NA values
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
df_4 <- subset(df_3, host_response_rate!='NA' & review_scores_rating!='NA') #I think this deleted all 0 values for # of reviews, is this an issue
View(df_4)
is.na(df_4) #can see this has decreased
df_5 <- subset(df_4, host_response_rate!='N/A') #b/c some of NA values listed as N/A instead for this variable
View(df_5)
df_6 <- subset(df_5, neighbourhood!='')
View(df_6)
df_6[df_6==""] <- NA
is.na(df_6)
df_7 <- na.omit(df_6)
is.na(df_7)
View(df_7)
dim(df_7)



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
