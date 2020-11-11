

#################
## Predictions ##
#################

#split
store.train <- store %>% filter(!is.na(sales))
store.test <- store %>% filter(is.na(sales))
plot_missing(store.train)

### Time Series
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 0,
                              horizon = 1,
                              fixedWindow = FALSE)

plsFitTime <- train(sales ~ .,
                    data = store.train %>% select(-id),
                    method = "pls",
                    preProc = c("center", "scale"))
plsFitTime
names(plsFitTime)
plot(plsFitTime)
plsFitTime$bestTune


predictions <- data.frame(id=store.test$id, sales = (predict(plsFitTime, newdata=store.test)))

## write to a csv
write.csv(predictions,"/Users/graceedriggs/Documents/STAT 495/Store-Item-Demand/GD_XGB_Store1.csv", row.names = FALSE)
#write_csv(predictions, "submission.csv")
