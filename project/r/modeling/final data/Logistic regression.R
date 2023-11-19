suppressMessages(library("ISLR2"))
suppressMessages(library("ggplot2"))
suppressMessages(library("gridExtra"))
suppressMessages(library("dplyr"))
suppressMessages(library("magrittr"))
suppressMessages(library("readr"))



train <- read_csv("C:/Users/tobia/OneDrive/Desktop/Master QFin/SML/train.csv")
test <- read_csv("C:/Users/tobia/OneDrive/Desktop/Master QFin/SML/test.csv")


unlist(lapply(train, class))
cast_out <- c("PassengerId", "Name", "family", "travel_group")

train <- train[, !(colnames(train) %in% cast_out)]
test <- test[, !(colnames(test) %in% cast_out)]


# Without "PassengerId", "Name", "family", "travel_group
fit_full <- glm(Transported ~ HomePlanet + CryoSleep + Deck + Cabin_Number + Side + Destination + Age + VIP + RoomService + FoodCourt + 
                  ShoppingMall + Spa + VRDeck + Age_Group + total_expenditure + any_expenditure + family_size  + travel_group_size,
                data = train, family = binomial(link = "logit"))

# Backwards elimination
backwards = step(fit_full) # Backwards selection is the default
summary(backwards)
formula(backwards)

# Logistic Regression with variables we got from the backwards elimination
glmfit <- glm(Transported ~ HomePlanet + CryoSleep + Deck + Cabin_Number + 
                Side + Destination + Age + RoomService + FoodCourt + ShoppingMall + 
                Spa + VRDeck + any_expenditure, data = train, family = binomial(link = "logit"))
summary(glmfit)

# # Without Age and Deck because not significant
# glmfit <- glm(Transported ~ HomePlanet + CryoSleep + Cabin_Number + 
#                 Side + Destination + RoomService + FoodCourt + ShoppingMall + 
#                 Spa + VRDeck + any_expenditure, data = train, family = binomial(link = "logit"))
# summary(glmfit)



# Training dataset

# Changing transported to 1 and otherwise 0
train$Transported <- as.integer(ifelse(train$Transported == "TRUE", 1, 0))

preds_train <- predict(glmfit, type = "response")

preds_t = ifelse(preds_train>0.5, 1, 0)

temp_t = table(train$Transported, preds_t)
print(temp_t)

accuracy_t <- (temp_t[1]+temp_t[4])/sum(temp_t)
accuracy_t
sensitivity_t <- temp_t[4]/(temp_t[4]+temp_t[3])
sensitivity_t
specificty_t <- temp_t[1]/(temp_t[1]+temp_t[2])
specificty_t

# library(ROCR)
# pred <- prediction(preds_t, train$Transported)
# perf <- performance(pred, "tpr", "fpr")
# plot(perf, colorize=TRUE)
# 
# unlist(slot(performance(pred, "auc"), "y.values"))


# Test dataset

# Changing transported to 1 and otherwise 0
test$Transported <- as.integer(ifelse(test$Transported == "TRUE", 1, 0))

# Getting the predictions for the test dataset
preds_test = predict(glmfit,test, type = "response")

#Since we get the probabilities with the regression model, we used a 0.5
#threshold.
preds = ifelse(preds_test>0.5, 1, 0)

#To compute the validation set error and false positive ratio we use the
#confusion matrix.
temp = table(test$Transported, preds)
print(temp)

accuracy <- (temp[1]+temp[4])/sum(temp)
accuracy
sensitivity <- temp[4]/(temp[4]+temp[3])
sensitivity
specificty <- temp[1]/(temp[1]+temp[2])
specificty


