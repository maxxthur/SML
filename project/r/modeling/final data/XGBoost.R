

library(dplyr)
library(ggplot2)
library(stringr)
library(caret)
library(mice)
library(xgboost)
library(gridExtra)
library(readr)
library(ROCR)
library(tidyverse)
library(leaps)
library(xtable)





rm(list = ls())

test <- read_csv("C:/Users/Vámosi Judit/Desktop/Frisch Marcel/WU master Qantitative Finance/StatisticalMachineLearning/Project_XGBoosting/test (1).csv")

train<-read_csv("C:/Users/Vámosi Judit/Desktop/Frisch Marcel/WU master Qantitative Finance/StatisticalMachineLearning/Project_XGBoosting/train (2).csv")


full <- bind_rows(train,test,.id='id')
full$id <- ifelse(full$id=='1','train','test')




# 1. Create dummy variables

# 1.1) Create dummy variables data sets
encoder <- dummyVars(~HomePlanet + Destination + Deck + Side, 
                     data = full, sep = '.')
dummy <- predict(encoder, newdata = full)

# 1.2) Drop dummy variable columns from original data set
dummy.names <- c('HomePlanet','CryoSleep','Destination','Deck','Side')

full.dummy <- full[!colnames(full) %in% dummy.names]

# 1.3) Join original data set + dummy variable dataset
full.dummy <- cbind(full.dummy,dummy)




# 2. Drop unnecessary columns
full.dummy.model <- full.dummy[,!colnames(full.dummy) %in% c('PassengerId','Name','family',"travel_group",'Age_Group',"Cabin_Number","total_expenditure")]


# 3. Scale continuous variables
scale <- c('Age','RoomService','FoodCourt','ShoppingMall','Spa','VRDeck')
full.dummy.model[,scale] <- sapply(full.dummy.model[,scale],scale)


# 4. Divide train/test data sets
train.full.dummy.model <- full.dummy.model[full.dummy.model$id=='train',-1]
test.full.dummy.model <- full.dummy.model[full.dummy.model$id=='test',-1]


# Set hyperparameter range to train xgboost model
grid = expand.grid(
  nrounds = c(75, 100),
  colsample_bytree = 1,    
  min_child_weight = 1,    
  eta = c(0.01, 0.1, 0.3), 
  gamma = c(0.5, 0.25),    
  subsample = 0.5,         
  max_depth = c(2, 3)      
)

cntrl = caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "final"                                                        
)



train.full.dummy.model$any_expenditure<-NULL

 

x <- as.matrix(train.full.dummy.model[, -8])
y <- ifelse(train.full.dummy.model$Transported == 'TRUE', 1, 0)

train.full.dummy.model$VIP<-as.numeric(train.full.dummy.model$VIP)

train.full.dummy.model$Transported<-as.numeric(train.full.dummy.model$Transported)
# Train XGB model to find optimal hyperparameters
set.seed(123)
train.xgb = caret::train(
  x = train.full.dummy.model[,-8],
  y = train.full.dummy.model[, 8],
  trControl = cntrl,
  tuneGrid = grid,
  method = "xgbTree"
)

train.xgb

# Set optimal hyperparameters
param <- list(  objective           = "binary:logistic",     
                booster             = "gbtree",
                eval_metric         = "error",                  
                eta                 = 0.3, 
                max_depth           = 3, 
                subsample           = 0.5,
                colsample_bytree    = 1,
                gamma               = 0.25,
                min_child_weight = 1
)



# Matrix for XGBoost function

train.mat <- xgboost::xgb.DMatrix(data = x, 
                                  label = y)



# Create model with optimal hyperparameters
set.seed(123)
xgb.fit <- xgb.train(params = param, data = train.mat, nrounds = 75)
xgb.fit



# Predict/evaluate  using train dataset
pred <- predict(xgb.fit, x)






unique_levels_y <- unique(y)
unique_levels_pred <- unique(ifelse(pred >= Cutoff, 1, 0))

print("Unique levels in y:")
print(unique_levels_y)

print("Unique levels in predicted values:")
print(unique_levels_pred)


# Convert to factors with explicit levels
y_factor <- factor(y, levels = unique_levels_y)
pred_factor <- factor(ifelse(pred >= Cutoff, 1, 0), levels = unique_levels_y)

# Use confusionMatrix with the factors
conf_matrix <- caret::confusionMatrix(y_factor, pred_factor)
print(conf_matrix)




test.full.dummy.model$any_expenditure<-NULL
test.full.dummy.model$VIP<- as.numeric(test.full.dummy.model$VIP)
# Predict for test data set
testx <- as.matrix(test.full.dummy.model[, -8])
xgb.test <- predict(xgb.fit, testx)
Transported <- factor(ifelse(xgb.test>=Cutoff,'True','False'),levels=c('True','False'))
Transported <- as.data.frame(Transported)
Transported$PassengerId <- full[full$id=='test','PassengerId']

xgb.test.result <- Transported %>% dplyr::select(PassengerId,Transported)




# Add 'id' column to 'xgb.test.result'
xgb.test.result$id <- seq_len(nrow(xgb.test.result))

# Add 'id' column to 'test.full.dummy.model'
test.full.dummy.model$id <- seq_len(nrow(test.full.dummy.model))

# Merge the two data frames based on 'id'
merged_data <- merge(xgb.test.result, test.full.dummy.model, by.x = "id", by.y = "id", all.x = TRUE)

# Check for uniqueness of 'id' in both data frames
if (length(unique(xgb.test.result$id)) != nrow(xgb.test.result) |
    length(unique(test.full.dummy.model$id)) != nrow(test.full.dummy.model)) {
  stop("Column 'id' is not unique in one or both data frames.")
}

# Convert 'Transported.x' values to uppercase
merged_data$Transported.x <- toupper(merged_data$Transported.x)


# Calculate confusion matrix
conf_matrix_1<- table(merged_data$Transported.x, merged_data$Transported.y)

# Calculate accuracy
accuracy <- sum(merged_data$Transported.x == merged_data$Transported.y, na.rm = TRUE) / nrow(merged_data)

print(paste("Accuracy on the test set:", round(accuracy * 100, 2), "%"))

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix_1)



### Creating Roc curve fortraing dtaset-----

# Install and load necessary packages if not already installed
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}
library(pROC)

# Assuming 'y' is the true class labels and 'pred' is the predicted probabilities
roc_obj <- roc(y, pred)

# Create ROC curve using plot.roc
plot(roc_obj, col = "blue", main = "ROC curve for training dataset", lwd = 2)
abline(h = 1, v = 0, col = "gray", lty = 2)


### Creating Latex table for XGBoost-----

# Create a matrix with the confusion matrix values
conf_matrix <- matrix(c(1906, 1311, 153, 3150), nrow = 2, byrow = TRUE)

# Create a data frame with the confusion matrix
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("0", "1")
rownames(conf_matrix_df) <- c("0", "1")

# Convert the data frame to a LaTeX table
latex_table <- xtable(conf_matrix_df, caption = "Confusion Matrix for the Training dataset", label = "tab:conf_matrix")

# Print the LaTeX code
print(latex_table, include.rownames = TRUE, include.colnames = TRUE, floating = FALSE)


# Create a matrix with the confusion matrix values
conf_matrix_test <- matrix(c(600, 85, 498, 990), nrow = 2, byrow = TRUE)

# Create a data frame with the confusion matrix
conf_matrix_df_test <- as.data.frame(conf_matrix_test)
colnames(conf_matrix_df_test) <- c("0", "1")
rownames(conf_matrix_df_test) <- c("0", "1")

# Convert the data frame to a LaTeX table
latex_table <- xtable(conf_matrix_df_test, caption = "Confusion Matrix for Test Dataset", label = "tab:conf_matrix_test")

# Print the LaTeX code
print(latex_table, include.rownames = TRUE, include.colnames = TRUE, floating = FALSE)






