################ Games on steam ###############
steam.data <- read.csv("D:/PG/Data Mining/Projects/Final Report/DMML/steam.csv")
str(steam.data)
attach(steam.data)

########################################################################################################
########################################################################################################
############################## DATA PREPROCESSING ######################################################
########################################################################################################
########################################################################################################

#### Removed some unwanted columns from the dataset #######
steam.data <- steam.data[,-c(1,2,3,5,6,10,11,16 )]
str(steam.data)
################################## Data transformation ###################################

## Column english converted into 'Yes' and 'No' ##
steam.data$english <- factor(steam.data$english, levels = c(1,0), labels = c("Yes", "No"))
##  Column required_age converted into different ages ##
steam.data$required_age <- factor(steam.data$required_age, levels = c(0,3,7,12,16,18), 
                                  labels = c("No Age Limit", "3+", "7+", "12+", "16+", "18+"))
## Column owner: classification in only 4 groups--reducing levels ##
owner <- function(x){
  print(x)
  if(x == '0-20000'){
    return('<20K')
  }
  else if ((x == '0-20000') | (x == '100000-200000') | (x == '20000-50000') | (x == '50000-100000') |(x== '200000-500000')){
    return('20K to 500K')
  }
  else if ((x == '500000-1000000') | (x=='5000000-10000000') | (x == '2000000-5000000') | (x == '1000000-2000000')){
    return('500K to 10M')
  }
  else
    return('10M to 200M')
}

steam.data$owners <- factor(sapply(steam.data$owners, function(x) owner(x)))
## Column platforms- renaming levels to remove semicolon from it
steam.data$platforms <- factor(steam.data$platforms, 
                               levels = c('linux', 'mac', 'mac;linux', 'windows', 'windows;linux','windows;mac', 'windows;mac;linux'),
                               labels = c('linux', 'mac', 'mac linux', 'windows', 'windows linux','windows mac', 'windows mac linux'))
## Column categories- It consist multiple categories, however, we need only three
categories <- function(x){
  if ((length(grep('Multi-player', x))>0) && (length(grep('Single-player', x))>0)){
    var = 'Both'
  }
  else if (length(grep('Multi-player',x))>0){
    var = 'Multi-player'
  }
  else if (length(grep('Single-player',x))>0){
    var = 'Single-player'
  }
  else {
    var = 'Not Mentioned'
  }
  return(var)
}
steam.data$categories <- factor(sapply(steam.data$categories, function(x) categories(x)))

write.csv(file = 'D:/PG/Data Mining/Projects/Final Report/steam_final_data.csv', steam.data)




################################## Decison tree #########################################
library(caTools)
library(C50)
library(caret)
library(OneR)
library(pROC)
df_decision <- steam.data
str(df_decision)

### splitting the dataset
set.seed(101)
sample <- sample.split(df_decision$owners, SplitRatio = 0.70)
train_dt <- subset(df_decision, sample==T)
test_dt <- subset(df_decision, sample == F)
### checking split of oweners
prop.table(table(train_dt$owners))
prop.table(table(test_dt$owners))
## building the model
steam_model <- C5.0(train_dt[,-9], train_dt$owners)
summary(steam_model)
(steam_model)
## predicting model
steam_pred <- predict(steam_model, test_dt)
steam_pred_prob <- predict(steam_model, test_dt, type = 'prob')
#plotting ROC curve
c5_roc <-multiclass.roc(test_dt$owners, steam_pred_prob)
# confusion matrix
confusionMatrix(steam_pred, test_dt$owners)

#####################################################################################

########################################## OneR algorithm ###########################
df_ripper <- steam.data
# using the same above splitted data
steam_ripper <- OneR(owners ~ ., data = train_dt)
summary(steam_ripper)
steam_ripper
### predicting values 
ripper_pred <- predict(steam_ripper, test_dt)
ripper_pred_prob <- predict(steam_ripper, test_dt ,type = 'prob')
# area under curve
ripp_roc <- multiclass.roc(test_dt$owners, ripper_pred_prob)
# confusion matrix
confusionMatrix(ripper_pred, test_dt$owners)
###############################################################################
### Compare two models based on the confusion matrix ##########################
###############################################################################