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
is.na(df_5)
