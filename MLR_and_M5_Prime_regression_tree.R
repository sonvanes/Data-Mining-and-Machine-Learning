##################### Board Game #####################
board.data1 <- read.csv('D:/PG/Data Mining/Projects/Final Report/games.csv')
str(board.data1)

########################################################################################################
########################################################################################################
############################## DATA PREPROCESSING ######################################################
########################################################################################################
########################################################################################################

###### remvoing unwanted columns from the dataset ######
board.data1 <- board.data1[, -c(3,4,7, 13,15,18,19,20)]
### checking for the NA's valaues present
sum(is.na(board.data1)) ### 15 NA's are present hence decided to remove
board.data1 <- na.omit(board.data1)
##### combining two columns minplaytime and maxplaytime into avgplaytime #######
board.data1["avgplaytime"] <- (board.data1$minplaytime + board.data1$maxplaytime)/2
####### similarly total_wanters and total_wishers combined to total_interesters #####
board.data1["total_interesters"] <- board.data1$total_wanters+board.data1$total_wishers
str(board.data)
board.data1 <- board.data1[,-c(5,6,11,12)]
###### there are some 0 values present in the dataset, those are converted into NA and then removed
board.data1[board.data1 == 0] <- NA
board.data1 <- na.omit(board.data1)
str(board.data1)
View(board.data)

write.csv(file = "D:/PG/Data Mining/Projects/Final Report/game_final_data.csv", board.data)
########################################################################################################

########################## multiple linear regression ################################################# 
library(corrplot)
library(caTools)
library(rstatix)
library(ggpubr)
library(caret)

board.data <- read.csv("D:/PG/Data Mining/Projects/Final Report/Cleaned dataset/game_final_data.csv")

# correlation checking
correlation1 <- cor(board.data[,-2])
cor(board.data1[-c(1,2)])
corrplot(correlation1, method = 'circle')
str(board.data$total_owners)

# checking normality
hist(board.data$average_rating, probability = T, main = "Histogram of Norm")
lines(density(board.data$average_rating), col=2)

################# outliers ###############################################
boxplot(board.data[,-c(1,2)]) # from the diagram it can be seen that there are some outliers present
# handling outliers using winsorizing method

# users_rated
summary(board.data[,6])
upper_limit <- 128 + 1.5*IQR(board.data[,6])
upper_limit
board.data$users_rated[board.data$users_rated > upper_limit]
board.data$users_rated[board.data$users_rated > upper_limit] <- upper_limit
# total owners
summary(board.data[,8])
upper_limit <- 343 + 1.5*IQR(board.data[,8])
upper_limit
board.data$total_owners[board.data$total_owners > upper_limit]
board.data$total_owners[board.data$total_owners > upper_limit] <- upper_limit
# avgplaytime
summary(board.data[,9])
upper_limit <- 90 + 1.5*IQR(board.data[,9])
upper_limit
board.data$avgplaytime[board.data$avgplaytime > upper_limit]
board.data$avgplaytime[board.data$avgplaytime > upper_limit] <- upper_limit
# total interesters
summary(board.data[, 10])
upper_limit <- 60+1.5*IQR(board.data[,10])
upper_limit
board.data$total_interesters[board.data$total_interesters > upper_limit]
board.data$total_interesters[board.data$total_interesters > upper_limit] <- upper_limit
# maxplayers
summary(board.data[,4])
upper_limit <- 6+1.5*IQR(board.data[,4])
upper_limit
board.data$maxplayers[board.data$maxplayers > upper_limit]
board.data$maxplayers[board.data$maxplayers > upper_limit] <- upper_limit
# minage
summary(board.data[,5])
upper_limit <- 12+1.5*IQR(board.data[,5])
upper_limit
board.data$minage[board.data$minage > upper_limit]
board.data$minage[board.data$minage > upper_limit] <- upper_limit
# minplayers
summary(board.data[,3])
upper_limit <- 12+1.5*IQR(board.data[,3])
upper_limit
board.data$minplayers[board.data$minplayers > upper_limit]
board.data$minplayers[board.data$minplayers > upper_limit] <- upper_limit

# splittin data into train ans test
set.seed(101)
sampel <- sample.split(board.data$average_rating, SplitRatio = 0.7)
train <- subset(board.data, sampel == T)
test <- subset(board.data, sampel==F)
attach(board.data)
### building model ###
mlm <- lm(average_rating ~.-id-type-users_rated, data=train)
summary(mlm)
spar(mfrow=c(2,2))
plot(mlm)
# predictions
pred <- predict(mlm, test)
# checking correlation between predicted and actual values
cor(pred, test$average_rating)
# MAE mean absolute error between actual values and predicted values
MAE(pred, test$average_rating)
# MAE by taking mean of actual values
MAE(mean(train$average_rating), test$average_rating)
# summary of actual and predicted values
summary(test$average_rating)
summary(pred)
# cooks distance
cooksdistance <- round(cooks.distance(mlm), 4)
influential <- as.numeric(names(cooksdistance)[(cooksdistance > 4 * mean(cooksdistance, na.rm = TRUE))]) 
influential

##############################################################################################
##############################################################################################

#### M5 Prime regression model #########
library(rpart)
library(rpart.plot)
str(board.data)
m5_df <- board.data[,-c(11)]
str(m5_df)
## As we checked above this dataset fulfils normality and correlation assumptions we are 
# proceeding forward
### building the m5 Prime model ####
m5_model <- rpart(average_rating ~ .-id-type-users_rated, data = train)
m5_model
summary(m5_model)
# plotting decision tree for the above model
rpart.plot(m5_model, digits = 2, fallen.leaves = T, type = 3, extra = 101)
# predictions for m5 prime model
pred_m5 <- predict(m5_model, test)
# checking correlation between predicted and actual values
cor(pred_m5, test$average_rating)
# MAE mean absolute error between actual values and predicted values
MAE(test$average_rating, pred_m5)
# MAE by taking mean of actual values
MAE(mean(train$average_rating), test$average_rating)
# summary of actual and predicted values
summary(test$average_rating)
summary(pred_m5)
RMSE(pred_m5, test$average_rating)
#############################################################################
###### Compare the results between above two algorithms ##########
#############################################################################

