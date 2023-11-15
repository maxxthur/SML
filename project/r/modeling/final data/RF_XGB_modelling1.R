#######################################
## Fitting Random Forest
#######################################

# Path
setwd("/Users/jacopoliera/Desktop/University/WU/Quantitative Finance/Year2/SML/Project/Data")

# Libraries
library(randomForest)
library(mlbench)
library(caret)
library(e1071)
library(xgboost)
library(dplyr)


train <- read.csv("train.csv")
test <- read.csv("test.csv")


###################################
## 0.0 Pipeline before rf, xgboost 
###################################

unlist(lapply(train, class))
cast_out <- c("PassengerId", "Deck", "family")

train <- train[, !(colnames(train) %in% cast_out)]
test <- test[, !(colnames(test) %in% cast_out)]

train <- within(train, 
                {
                  # Logicals should be factors really
                  HomePlanet <- as.factor(HomePlanet)
                  CryoSleep <- as.factor(CryoSleep)
                  VIP <- as.factor(VIP)
                  Transported <- as.factor(Transported)
                  any_expenditure <- as.factor(any_expenditure)
                })

test <- within(test, 
                {
                  # Logicals should be factors really
                  HomePlanet <- as.factor(HomePlanet)
                  CryoSleep <- as.factor(CryoSleep)
                  VIP <- as.factor(VIP)
                  Transported <- as.factor(Transported)
                  any_expenditure <- as.factor(any_expenditure)
                })

###################################
## 1.0 Random Forest Model Fitting
###################################

set.seed(1381378)

ytrain <- train[, "Transported"]
Xtrain <- train[, !(colnames(train) %in% c("Transported"))]

ytest <- test[, "Transported"]
Xtest <- test[, !(colnames(test) %in% c("Transported"))]

control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = 'grid')
#create tunegrid
tunegrid <- expand.grid(mtry = c(floor(ncol(train)/4), floor(ncol(train)/3), floor(ncol(train)/2), ncol(train)-2), 
                        splitrule = c("gini", "hellinger"), 
                        min.node.size = c(5, 10, 20, 30))

getModelInfo("ranger")

#train with different ntree parameters
dep <- which(names(train) == "Transported")

fit <- train(y = train$Transported, 
             x = train[, -dep],
             method = 'ranger',
             metric = 'Accuracy',
             tuneGrid = tunegrid,
             trControl = control, 
             ntree = 20)

# Weird
fit$variable.importance

# Visualize Results for the fit
ggplot(fit)

rf_pred <- predict(fit$finalModel, Xtest)

result <- data.frame(confusionMatrix(as.factor(rf_pred$predictions), ytest)$table)

plotTable <- result %>%
  mutate(goodbad = ifelse(result$Prediction == result$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(result$Reference)))

###################################
## 2.0 Xtreme Gradient Boosting
###################################

## One-hot encode factor variables
train_xgb <- model.matrix(~.+0, data = train)
test_xgb <- model.matrix(~.+0, data = test)

## Isolate the predictory
train_xgb_predictors <- train_xgb[, !(colnames(train_xgb) %in% c("Transported"))]
test_xgb_predictors <- test_xgb[, !(colnames(test_xgb) %in% c("Transported"))]

## Target variables
train_xgb_target <- train_xgb[,"Transported"]
test_xgb_target <- test_xgb[,"Transported"]

## check
colnames(train_xgb_predictors) == colnames(test_xgb_predictors)

## set up the cross-validated hyper-parameter search
xgb_grid <-  expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001),
  max_depth = c(2, 3, 4, 6, 8, 10),
  gamma = c(0.1, 0.2, 0.3),
  subsample = 1, 
  colsample_bytree = 1,
  min_child_weight = 1
)

# pack the training control parameters
xgb_trcontrol <-  trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE,
  returnResamp = "all", 
  returnData = FALSE,
  allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
# using CV to evaluate
xgb_train_1 = train(
  x = as.matrix(train_xgb_predictors),
  y = train_xgb_target,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree"
)

ggplot(xgb_train$results, aes(x = as.factor(eta), y = max_depth, size = RMSE, color = RMSE)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none") +
  ggtitle("RMSE for different depths and eta levels")

## Derivating the best tuning parameters for the model. (Direct LGD)
xgb_params <- list(
  objective = "binary:logistic",                                              
  eta = 0.01,                                                                # learning rate
  max.depth = 6,                                                            # max tree depth
  gamma = 0.2,                                                              # penalization factor gamma
  eval_metric = "rmse"                                                       # evaluation/loss metric
)

###################################
##### Prediction on test ##########
###################################

xgb_select <-  xgboost(data = train_xgb_predictors,
                  label = train_xgb_target,
                  params = xgb_params,
                  nrounds = 1000,                                                 # max number of trees to build
                  verbose = TRUE,                                         
                  print_every_n = 1,
                  early_stopping_rounds = 10                                     # stop if no improvement within 10 trees
)

xgb_select$feature_names
colnames(test_xgb_predictors)

y_hat <- predict(xgb_slect, test_xgb_predictors)

############################################
##### XGBoost variable importance ##########
############################################

importance <- xgb.importance(model = xgb_2)
xgb.plot.importance(importance, 
                    rel_to_first = T, 
                    xlab = "Relative importance", 
                    top_n = 10)
