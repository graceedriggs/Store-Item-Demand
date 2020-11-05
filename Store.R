#libraries
library(tidyverse)
library(caret)
library(DataExplorer)
library(lubridate)
library(pls)
library(forecast)
library(tseries)

#store.train <- read_csv("../input/demand-forecasting-kernels-only/train.csv")
#store.test <- read_csv("../input/demand-forecasting-kernels-only/test.csv")
store.train <- read_csv("/Users/graceedriggs/Documents/Stat 495/Store-Item-Demand/train.csv")
store.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/Store-Item-Demand/test.csv")

store <- bind_rows(store.train, store.test)

#add day of week
store$dayofweek <- weekdays(store$date)
#add month
store$month <- month(store$date)
#add year
store$year <- year(store$date)

plot_missing(store)

#visualize the data
plot(density(store.train$sales))

aggregate(sales ~ store + item + dayofweek + month + year, data = store, mean)
aggregate(sales ~ dayofweek, data = store, mean)
plot(aggregate(sales ~ month, data = store, mean))
aggregate(sales ~ year, data = store, mean)


# plotting time series for month, weekday and date or item 1.
# make sure they have almost equa variance
ggplot(store.train %>% filter(item == 1), 
       mapping=(aes(x = as.factor(month), y = sales)))+
  geom_boxplot()

ggplot(store.train %>% filter(item == 1), 
       mapping=(aes(x = as.factor(dayofweek), y = sales)))+
  geom_boxplot()

ggplot(store.train %>% filter(item == 1), 
       mapping=(aes(x = date, y = sales)))+
  geom_line()

str(store)


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
