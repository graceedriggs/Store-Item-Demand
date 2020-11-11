
#################
## Predictions ##
#################

#split
store.train <- store %>% filter(!is.na(sales))
store.test <- store %>% filter(is.na(sales))
plot_missing(store.train)


#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=3, 
                        repeats=2)
grid_default <- expand.grid(
  nrounds = 250,
  max_depth = 10,
  eta = 0.3,
  gamma = 15,
  colsample_bytree = .5,
  min_child_weight = 25,
  subsample = 1)

#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
xgb_default <- train(sales ~ ., 
                    data=store.train %>% select(-id), 
                    method='xgbTree', 
                    trControl=control,
                    tuneGrid = grid_default)

names(xgb_default)
plot(xgb_default)
xgb_default$bestTune
predict(xgb_default, newdata=cancer.test)


predictions <- data.frame(id=store.test$id, sales = (predict(xgb_default, newdata=store.test)))

## write to a csv
write.csv(predictions,"/Users/graceedriggs/Documents/STAT 495/Store-Item-Demand/GD_XGB_Store1.csv", row.names = FALSE)
#write_csv(predictions, "submission.csv")

