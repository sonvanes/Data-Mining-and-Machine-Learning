library(caTools)
library(caret)

data.frame<-read.csv("D:/PG/Data Mining/Projects/Final Report/tesco1.csv")
str(data.frame)
data.frame <- tesco[,-c(3,4,5,6,7,8,9,10,27)]
View(data.frame)

#Checking for NA values 
any(is.na(data.frame))

#Places of NA values
which(is.na(data.frame))
str(data.frame)

#1- the customer tapped the card
#0 -the customer did not use the card
#NA- the card was never shown to the customer,therefore they did not tap the card.Hence NA values converted
#to 0.
data.frame[is.na(data.frame)] <-0
# changing datatype of dependent variable to factor
data.frame$content_1 <- as.factor(data.frame$content_1)
str(data.frame)
cor_log <- cor(data.frame[,-c(1,2,17,18)])
corrplot(cor_log, method = 'circle')
boxplot(data.frame[,-c(1,17,18)])
#handling outliers
summary(data.frame[,4])
upper_limit <- 1005 + 1.5*IQR(data.frame[,4])
upper_limit
data.frame$express.total.spend[data.frame$express.total.spend > upper_limit]
data.frame$express.total.spend[data.frame$express.total.spend > upper_limit] <-upper_limit

summary(data.frame[,6])
upper_limit <- 1484.7 + 1.5*IQR(data.frame[,6])
upper_limit
data.frame$metro.total.spend[data.frame$metro.total.spend > upper_limit]
data.frame$metro.total.spend[data.frame$metro.total.spend > upper_limit] <-upper_limit

summary(data.frame[,8])
upper_limit <- 3660.3 + 1.5*IQR(data.frame[,8])
upper_limit
data.frame$superstore.total.spend[data.frame$superstore.total.spend > upper_limit]
data.frame$superstore.total.spend[data.frame$superstore.total.spend > upper_limit] <-upper_limit

summary(data.frame[,10])
upper_limit <- 3652.8 + 1.5*IQR(data.frame[,10])
upper_limit
data.frame$extra.total.spend[data.frame$extra.total.spend > upper_limit]
data.frame$extra.total.spend[data.frame$extra.total.spend > upper_limit] <-upper_limit

summary(data.frame[,12])
upper_limit <- 1356.3 + 1.5*IQR(data.frame[,12])
upper_limit
data.frame$fandf.total.spend[data.frame$fandf.total.spend > upper_limit]
data.frame$fandf.total.spend[data.frame$fandf.total.spend > upper_limit] <-upper_limit

summary(data.frame[,14])
upper_limit <- 739 + 1.5*IQR(data.frame[,14])
upper_limit
data.frame$petrol.total.spend[data.frame$petrol.total.spend > upper_limit]
data.frame$petrol.total.spend[data.frame$petrol.total.spend > upper_limit] <-upper_limit

summary(data.frame[,16])
upper_limit <- 3671 + 1.5*IQR(data.frame[,16])
upper_limit
data.frame$direct.total.spend[data.frame$direct.total.spend > upper_limit]
data.frame$direct.total.spend[data.frame$direct.total.spend > upper_limit] <-upper_limit


# splitting the dataset
sample <- sample.split(data.frame$content_1, SplitRatio = 0.7)
train <- subset(data.frame, sample==T)
test <- subset(data.frame, sample==F)

## building model
logistic_model <- glm(content_1 ~ . -customer.id, family = 'binomial', data = train)
summary(logistic_model)

# prediction
pred_logistic <- predict(logistic_model, test, type = 'response')
pred.results <- ifelse(pred_logistic > 0.7,1,0)
pred.results <- as.factor(pred.results)

#confusion matrix
confusionMatrix(pred.results, test$content_1)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit logistic regression Model
model <- train(content_1 ~.-customer.id, data=data.frame, trControl=train_control, method="glm", family='binomial')
# Summarise Results
print(model)

# prediction
pred_logistic <- predict(model, test, type = 'response')
pred.results <- ifelse(pred_logistic > 0.7,1,0)
pred.results <- as.factor(pred.results)

#confusion matrix
confusionMatrix(pred.results, test$content_1)
