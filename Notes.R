## unused


## delete?

timeSlices <- createTimeSlices(1:nrow(store.train %>% select(-id)), 
                               initialWindow = 36, horizon = 12, fixedWindow = TRUE)

trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]



#######################
## ARIMA Predictions ##
#######################


count_ts <- ts(store[, c('sales')])

store$clean_cnt <- tsclean(count_ts)

ggplot() + 
  geom_line(data = store, aes(x = date, y = clean_cnt)) + ylab('Sales Count') 

store$cnt_ma = ma(store$clean_cnt, order=7) # using the clean count with no outliers
store$cnt_ma30 = ma(store$clean_cnt, order=30)


ggplot() + 
  geom_line(data = store, aes(x = date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = store, aes(x = date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = store, aes(x = date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count') 

count_ma = ts(na.omit(store$cnt_ma), frequency=30) 
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp) 

adf.test(count_ma, alternative = "stationary")

## step 5: stationary
count_d1 <- diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

auto.arima(deseasonal_cnt, seasonal=FALSE)

# step 7: evaluate and iterate
fit <- auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals') 

fit2 <- arima(deseasonal_cnt, order=c(1,1,7))
fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=30)
plot(fcast)

hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout <- arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

fit_w_seasonality <- auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast) 

names(seas_fcast)

# figure out how to predict
predictions <- data.frame(id=store.test$id, sales = (predict(seas_fcast, newdata=store.test)))


write_csv(predictions, "submission.csv")


