#Importing libraries.
library(ggplot2)
library(reshape2)
library(forecast)
library(zoo)
library(Metrics)
library(caret)
library(GGally)

#Loading datasets provided by Kaggle. We won't include the 'stores' dataset since we're not attempting to forecasting at the store level.

#The training data. This is a large file. Workstation has 32gb ram.
train = read.csv('C:/Users/smith/Desktop/CKME 136/Grocery Sales Forecasting/train.csv', header = T, sep = ',')
dim(train)
memory.size()
memory.limit()

#The item attributes table.
items = read.csv('C:/Users/smith/Desktop/CKME 136/Grocery Sales Forecasting/items.csv', header = T, sep = ',')

#Transaction counts by location. We aggregate this data to the forecasting level.
transactions = read.csv('C:/Users/smith/Desktop/CKME 136/Grocery Sales Forecasting/transactions.csv', header = T, sep = ',')
transactions_agg_sum <- aggregate(transactions$transactions, by = list(transactions$date), sum)
names(transactions_agg_sum)[1] <- 'date'
names(transactions_agg_sum)[2] <- 'transactions'
rm(transactions)

#Holiday and event details by day.
holidays_events = read.csv('C:/Users/smith/Desktop/CKME 136/Grocery Sales Forecasting/holidays_events.csv', header = T, sep = ',')
#Based on the data description from Kaggle, some days have duplicate entries for holidays since some holidays were transferred to other dates.
#Remove duplicate dates where holidays were transferred/bridged and where different levels of gov't celebrate.
holidays_events = holidays_events[!duplicated(holidays_events$date),]

#Daily oil price.
oil = read.csv('C:/Users/smith/Desktop/CKME 136/Grocery Sales Forecasting/oil.csv', header = T, sep = ',')

#Training data preprocessing.

#Drop Kaggle row ID.
train$id <- NULL

#We'll be aggregating store sales for each SKU.
#Regarding the onpromotion attribute at the aggregate level for each SKU, we'll accept 'True' if it were 'True' at any store on a given day.
#Assuming missing data is 'False'.
train$onpromotion[train$onpromotion == ""] <- "False"
#Aggregate function 'max' only works on numeric values.
train$onpromotion <- as.integer(train$onpromotion)
train$onpromotion[train$onpromotion == 2] <- 0
train$onpromotion[train$onpromotion == 3] <- 1

#Aggregating store level sales by SKU for 'unit_sales' and 'onpromotion'.
train_agg_sum <- aggregate(train$unit_sales, by = list(train$date, train$item_nbr), sum)
#Restoring column names.
names(train_agg_sum)[1] <- 'date'
names(train_agg_sum)[2] <- 'item_nbr'
names(train_agg_sum)[3] <- 'unit_sales'
train_agg_max <- aggregate(train$onpromotion, by = list(train$date, train$item_nbr), max)
names(train_agg_max)[1] <- 'date'
names(train_agg_max)[2] <- 'item_nbr'
names(train_agg_max)[3] <- 'onpromotion'
#Joining aggregated 'unit_sales' and 'onpromotion' dataframes.
rm(train)
train_agg <- merge(x = train_agg_sum, y = train_agg_max, by.x = c('date', 'item_nbr'), all.x = T)
rm(train_agg_sum)
rm(train_agg_max)

#Creating week, year, weekday, and year-week attributes since we're aggregating to the week level and some further preprocessing will be required.
train_agg$date <- as.Date(train_agg$date, format = "%Y-%m-%d")
train_agg$week <- strftime(as.character(train_agg$date), "%U")
unique(train_agg$week)
train_agg$year <- strftime(as.character(train_agg$date), "%Y")
unique(train_agg$year)
train_agg$weekday <- strftime(as.character(train_agg$date), "%A")
unique(train_agg$weekday)
#This will be used for time series forecasting later.
train_agg$yearweek <- as.Date(paste(train_agg$year, train_agg$week, "06", sep = '-'), format = "%Y-%U-%w")
unique(train_agg$week[is.na(train_agg$yearweek)])
unique(train_agg$year[is.na(train_agg$yearweek)])
train_agg$yearweek[is.na(train_agg$yearweek) & train_agg$year == '2013'] <- "2013-01-01"
train_agg$yearweek[is.na(train_agg$yearweek) & train_agg$year == '2014'] <- "2014-01-01"
train_agg$yearweek[is.na(train_agg$yearweek) & train_agg$year == '2015'] <- "2015-01-01"
train_agg$yearweek[is.na(train_agg$yearweek) & train_agg$year == '2016'] <- "2016-01-01"

#We'll now join the other datasets to create a 'spine' for the final dataset and begin imputing on missing values.
#We'll eventually aggregate again to weekly level and rejoin the other tables.
#Converting date back to factor for joining.
train_agg$date <- as.factor(train_agg$date)
data = merge(x = train_agg, y = items, by = 'item_nbr', all.x = T)
data = merge(x = data, y = transactions_agg_sum, by.x = 'date', all.x = T)
data = merge(x = data, y = oil, by.x = 'date', all.x = T)
data = merge(x = data, y = holidays_events, by = 'date', all.x = T)
rm(train_agg)
rm(transactions_agg_sum)
rm(holidays_events)
rm(oil)

#Now to fix missing values using imputation.
#identifying columns with NA's
colSums(is.na(data))

#We'll work on the 'transactions' attribute first.
#Identify weekdays, weeks, and years with NA's.
unique(data[is.na(data$transactions),]$date)
unique(data[is.na(data$transactions),]$weekday)
#Replace missing values with average values of the same weekday for other weeks in the same year.
data[which(is.na(data$transactions) & data$date == "2016-01-01"),]$transactions <- mean(data[which(data$weekday == "Friday" & data$year == "2016"),]$transactions, na.rm = T)
data[which(is.na(data$transactions) & data$date == "2016-01-03"),]$transactions <- mean(data[which(data$weekday == "Sunday" & data$year == "2016"),]$transactions, na.rm = T)

#Now for the 'dcoilwtico' attribute. Replace values for missing days with the value of the previous day.
data$dcoilwtico <- na.locf(data$dcoilwtico, fromLast = T)

#For the holiday attributes, we'll replace each day without a holiday as "No Holiday".
#Since variables are factors, add new level "No Holiday".
levels(data$type) <- c(levels(data$type), "No Holiday")
data$type[is.na(data$type)] <- 'No Holiday'
levels(data$locale) <- c(levels(data$locale), "No Holiday")
data$locale[is.na(data$locale)] <- 'No Holiday'
levels(data$locale_name) <- c(levels(data$locale_name), "No Holiday")
data$locale_name[is.na(data$locale_name)] <- 'No Holiday'
levels(data$description) <- c(levels(data$description), "No Holiday")
data$description[is.na(data$description)] <- 'No Holiday'
levels(data$transferred) <- c(levels(data$transferred), "No Holiday")
data$transferred[is.na(data$transferred)] <- 'No Holiday'
#To keep things simple, we'll create a dummy variable for holiday/no holiday
data$holiday <- 0
#Based on Kaggle data description the following holiday attribute values are actual holidays.
data$holiday[which(data$type == 'Transfer' | data$type == 'Bridge' | data$type == 'Holiday' | data$type == 'Additional' | data$type == 'Event')] <- 1

#Now aggregating to the week level.
#We aggregated daily store level sales to week in two steps because of resource constrains.
data.wk.sales <- aggregate(data$unit_sales, by = list(data$year, data$week, data$yearweek, data$item_nbr), sum)
data.wk.onpromotion <- aggregate(data$onpromotion, by = list(data$year, data$week, data$yearweek, data$item_nbr), max)
names(data.wk.sales) <- c('year', 'week', 'yearweek', 'item_nbr', 'unit_sales')
names(data.wk.onpromotion) <- c('year', 'week', 'yearweek', 'item_nbr', 'onpromotion')
data.wk <- merge(x = data.wk.sales, y = data.wk.onpromotion, by.x = c('year', 'week', 'yearweek', 'item_nbr'), all.x = T)
rm(data.wk.sales)
rm(data.wk.onpromotion)

#Next aggregate and join other attributes to new weekly dataset.
#Starting with transactions.
data.wk.txns <- aggregate(data$transactions, by = list(data$date), mean)
names(data.wk.txns)[1] <- 'date'
names(data.wk.txns)[2] <- 'transactions'
data.wk.txns$date <- as.Date(data.wk.txns$date, format = "%Y-%m-%d")
data.wk.txns$week <- strftime(as.character(data.wk.txns$date), "%U")
data.wk.txns$year <- strftime(as.character(data.wk.txns$date), "%Y")

data.wk.txns <- aggregate(data.wk.txns$transactions, by = list(data.wk.txns$year, data.wk.txns$week), sum)
names(data.wk.txns)[1] <- 'year'
names(data.wk.txns)[2] <- 'week'
names(data.wk.txns)[3] <- 'transactions'

#Next oil.
data.wk.oil <- aggregate(data$dcoilwtico, by = list(data$year, data$week), mean)
names(data.wk.oil)[1] <- 'year'
names(data.wk.oil)[2] <- 'week'
names(data.wk.oil)[3] <- 'oil'
#Next holidays.
data.wk.holiday <-aggregate(data$holiday, by = list(data$year, data$week), max)
names(data.wk.holiday)[1] <- 'year'
names(data.wk.holiday)[2] <- 'week'
names(data.wk.holiday)[3] <- 'holiday'

#Joining data together into dataset for algorithm training/testing.
data.wk.ml = merge(x = data.wk, y = items, by = 'item_nbr', all.x = T)
data.wk.ml = merge(x = data.wk.ml, y = data.wk.txns, by = c("year", "week"), all.x = T)
data.wk.ml = merge(x = data.wk.ml, y = data.wk.oil, by = c('year', 'week') , all.x = T)
data.wk.ml = merge(x = data.wk.ml, y = data.wk.holiday, by = c('year', 'week'), all.x = T)

#Visualization
str(data.wk.ml)
summary(data.wk.ml)

#Initializing ggplot object with data frame.
g <- ggplot(data.wk.ml)

#Examining sales trend over time.
g + geom_col(aes(x = year, y = unit_sales)) + ggtitle("Total Unit Sales")

#Number of weeks in each year.
ggplot(setNames(aggregate(data.wk.ml$week, by = list(data.wk.ml$year), function(x) length(unique(x))), c("year", "Num.weeks"))) + geom_col(aes(x = year, y = Num.weeks)) + labs(title = "Number of Weeks", x = "Year", y = "Weeks") + geom_text(aes(x = year, y = Num.weeks, label = Num.weeks), nudge_y = 2) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

#Unit sales by week.
g + geom_col(aes(x = week, y = unit_sales)) + ggtitle("Weekly Unit Sales")

#Weekly Unit Sales by Year.
ggplot(setNames(aggregate(data.wk.ml$unit_sales, by = list(data.wk.ml$yearweek), sum), c("year.week", "unit_sales"))) + geom_line(aes(x = year.week, y = unit_sales)) + labs(title = "Weekly Unit Sales Trend", x = "Time (weeks)", y = "Unit Sales")

#Weekly transactions by Year.
g + geom_line(aes(x = yearweek, y = transactions)) + labs(title = "Weekly Transactions Trend", x = "Time (weeks)", y = "Transactions")

#Weekly average oil price.
g + geom_line(aes(x = yearweek, y = oil)) + labs(title = "Weekly Average Oil Price Trend", x = "Time (weeks)", y = "Oil Price")

#Scatterplot between oil price and unit sales.
g + geom_point(aes(x = unit_sales, y = oil)) + labs(title = "Oil Price vs. Unit Sales", x = "Unit Sales", y = "Oil Price")

#Scatterplot between oil price and transactions.
g + geom_point(aes(x = transactions, y = oil)) + labs(title = "Weekly Average Oil Price vs. Weekly Total Transactions", x = "Transactions", y = "Oil Price")

#Boxplot of Unit Sales by promotion attribute.
ggplot(data.wk.ml, aes(x = onpromotion, y = unit_sales), group = 1) + geom_boxplot() + labs(title = "Unit Sales: Promotion vs None", x = "Promotion", y = "Unit Sales")

#Item counts by product family.
ggplot(items) + geom_bar(aes(x = family, fill = family), stat = 'count') + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Item Counts by Product Family", x = "Family", y = "Count")

#Unit sales by product family
g + geom_boxplot(aes(x = family, y = unit_sales)) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Unit Sales by Product Family", x = "Family", y = "Unit Sales")

#Average unit sales by product family
ggplot(setNames(aggregate(data.wk.ml$unit_sales, by = list(data.wk.ml$family), mean), c("family", "unit_sales"))) + geom_col(aes(x = family, y = unit_sales)) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Average Unit Sales by Product Family", x = "Family", y = "Average Unit Sales")

#Row Counts by 'perishable'
ggplot(items) + geom_bar(aes(x = perishable), stat = 'count') + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "'Perishable' Counts", x = "Perishable", y = "Count")

#Average Unit Sales by Perishable.
ggplot(setNames(aggregate(data.wk.ml$unit_sales, by = list(data.wk.ml$perishable), mean), c("perishable", "unit_sales"))) + geom_col(aes(x = perishable, y = unit_sales)) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Average Unit Sales by 'perishable'", x = "Perishable", y = "Average Unit Sales")

#Average Unit Sales by Holiday.
ggplot(setNames(aggregate(data.wk.ml$unit_sales, by = list(data.wk.ml$holiday), mean), c("holiday", "unit_sales")))+ geom_col(aes(x = holiday, y = unit_sales)) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Average Unit Sales by 'holiday'", x = "Holiday", y = "Average Unit Sales")

#Scatterplot matrix on numeric data.
ggpairs(data.wk.ml[,c(1,5,6,9,10,11,12)])
pairs(data.wk.ml[,c(5,10,11)])
cor(data.wk.ml[,c(5,10,11)])

#Cast variables.
data.wk.ml$year <- as.factor(data.wk.ml$year)
data.wk.ml$week <- as.factor(data.wk.ml$week)
data.wk.ml$item_nbr <- as.factor(data.wk.ml$item_nbr)
data.wk.ml$onpromotion <- as.factor(data.wk.ml$onpromotion)
data.wk.ml$class <- as.factor(data.wk.ml$class)
data.wk.ml$perishable <- as.factor(data.wk.ml$perishable)
data.wk.ml$holiday <- as.factor(data.wk.ml$holiday)

#Preparing data for time series forecasting.
#Transforming dataset to rows = sales time series and columns SKUs.
data.wk.fcst <- dcast(data.wk.ml, year + week + yearweek ~ item_nbr, value.var  = "unit_sales")

#make 2017 testing data. Split training and testing data.
data.wk.fcst.train <- data.wk.fcst[which(data.wk.fcst$year == '2013' | data.wk.fcst$year == '2014' | data.wk.fcst$year == '2015' | data.wk.fcst$year == '2016'),]
data.wk.fcst.test <- data.wk.fcst[data.wk.fcst$year == '2017',]

#Removing SKUs with any NA's- weeks with 0 sales.
SKU.NA <- colnames(data.wk.fcst[-(1:3)])
NA.CNT <- unname(apply(data.wk.fcst[-(1:3)], 2, function(x) sum(is.na(x))))
SKU.NA <- cbind.data.frame(SKU.NA, NA.CNT)
SKU.NA$NA.CNT <- as.integer(SKU.NA$NA.CNT)
SKU.list <- SKU.NA$SKU.NA[SKU.NA$NA.CNT == 0]

#Creating a random sample of 53 SKUs since Random Forest class cannot handle factors with more than 53 levels. This random selection of the core SKU group will be the basis for our model comparison.
set.seed(5)
SKU.list53 <- sample(SKU.list, 53)

#Limiting datasets to sample SKUs.
data.wk.fcst.train <- data.wk.fcst.train[names(data.wk.fcst.train) %in% SKU.list53]
#data.wk.fcst.train$year <- data.wk.fcst$year[data.wk.fcst$year != '2017']
#data.wk.fcst.train$week <- data.wk.fcst$week[data.wk.fcst$year != '2017']
#data.wk.fcst.train$yearweek <- data.wk.fcst$yearweek[data.wk.fcst$year != '2017']

#include year, week, and yearweek.
data.wk.fcst.test <- data.wk.fcst.test[names(data.wk.fcst.test) %in% SKU.list53]
data.wk.fcst.test$year <- data.wk.fcst$year[data.wk.fcst$year == '2017']
data.wk.fcst.test$week <- data.wk.fcst$week[data.wk.fcst$year == '2017']
data.wk.fcst.test$yearweek <- data.wk.fcst$yearweek[data.wk.fcst$year == '2017']

#Fitting arima model.
fcst.arima <- ts(data.wk.fcst.train, frequency = 53, start = c(2013,1))
fit.arima <- apply (fcst.arima, 2, auto.arima)
pred.arima <- lapply(fit.arima, forecast, 33)
pred.arima <- sapply(pred.arima, "[", 4)
pred.arima <- as.data.frame(pred.arima)

#Unit Sales for selected SKUs.
autoplot(fcst.arima) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Unit Sales by SKU", x = "Time (week)", y = "Unit Sales")

#Arranging and combining results with actuals for scoring. the melt function transforms the pivoted data back to tabular format.
data.wk.fcst.sales <- melt(data.wk.fcst.test, id.vars = c("year", "week", "yearweek"), variable.name = "item_nbr", value.name = "unit_sales")
data.wk.fcst.pred <- data.wk.fcst.sales[,1:3]
data.wk.fcst.arima <- melt(pred.arima, variable.name = "item_nbr", value.name = "unit_sales_pred")
data.wk.fcst.test.arima <- cbind.data.frame(data.wk.fcst.sales, unit_sales_pred = data.wk.fcst.arima[,2])

#Scoring using the root mean squared error (RMSE).
rmse(as.vector(data.wk.fcst.test.arima$unit_sales), as.vector(data.wk.fcst.test.arima$unit_sales_pred))

#Plotting the prediction vs. actuals.
ggplot(data.wk.fcst.test.arima) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - ARIMA - 53 core SKUs", x = "actual", y = "predicted")

#Fitting ets model.
fcst.ets <- ts(data.wk.fcst.train, frequency = 53, start = c(2013,1))
fit.ets <- apply (fcst.ets, 2, ets)
pred.ets <- lapply(fit.ets, forecast, 33)
pred.ets <- sapply(pred.ets, "[", 2)
pred.ets <- as.data.frame(pred.ets)

#Arranging and combining results with actuals for scoring. the melt function transforms the pivoted data back to tabular format.
data.wk.fcst.sales <- melt(data.wk.fcst.test, id.vars = c("year", "week", "yearweek"), variable.name = "item_nbr", value.name = "unit_sales")
data.wk.fcst.pred <- data.wk.fcst.sales[,1:3]
data.wk.fcst.ets <- melt(pred.ets, variable.name = "item_nbr", value.name = "unit_sales_pred")
data.wk.fcst.test.ets <- cbind.data.frame(data.wk.fcst.sales, unit_sales_pred = data.wk.fcst.ets[,2])

rmse(as.vector(data.wk.fcst.test.ets$unit_sales), as.vector(data.wk.fcst.test.ets$unit_sales_pred))

ggplot(data.wk.fcst.test.ets) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - ETS - 53 core SKUs", x = "actual", y = "predicted")

#Mean of current year model (baseline).
data.wk.fcst.train.mean <- as.data.frame(apply(data.wk.fcst.train[160:212,], 2, mean))
data.wk.fcst.train.mean <- t(data.wk.fcst.train.mean)
rownames(data.wk.fcst.train.mean) <- c()
data.wk.fcst.test.mean <- as.data.frame(apply(data.wk.fcst.train.mean, 2, function(x) rep(x, 33)))
data.wk.fcst.test.mean <- melt(data.wk.fcst.test.mean, variable.name = "item_nbr", value.name = "unit_sales_pred")
data.wk.fcst.test.mean <- cbind.data.frame(data.wk.fcst.sales, unit_sales_pred = data.wk.fcst.test.mean[,2])

rmse(as.vector(data.wk.fcst.test.mean$unit_sales), as.vector(data.wk.fcst.test.mean$unit_sales_pred))

ggplot(data.wk.fcst.test.mean) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - MEAN - 53 core SKUs", x = "actual", y = "predicted")

#split train/test.
data.wk.ml.train <- data.wk.ml[which(data.wk.ml$year != '2017' & data.wk.ml$item_nbr %in% SKU.list53),]
data.wk.ml.train$yearweek <- NULL
data.wk.ml.test <- data.wk.ml[which(data.wk.ml$year == '2017'& data.wk.ml$item_nbr %in% SKU.list53),]
data.wk.ml.test$yearweek <- NULL

#To plot boxplot of SKUs.
data.wk.ml.core <- data.wk.ml[data.wk.ml$item_nbr %in% SKU.list53,]
ggplot(data.wk.ml.core) + geom_boxplot(aes(x = item_nbr, y = unit_sales)) + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none") + labs(title = "Unit Sales by SKU", x = "SKU", y = "Unit Sales")

#Recast levels for similarity between training and testing datasets.
data.wk.ml.train$year <- as.integer(data.wk.ml.train$year)
data.wk.ml.test$year <- as.integer(data.wk.ml.test$year)
data.wk.ml.train$onpromotion <- as.factor(data.wk.ml.train$onpromotion)
data.wk.ml.train$perishable <- as.factor(data.wk.ml.train$perishable)
data.wk.ml.train$holiday <- as.factor(data.wk.ml.train$holiday)
data.wk.ml.train$item_nbr <- factor(data.wk.ml.train$item_nbr)
data.wk.ml.train$family <- factor(data.wk.ml.train$family)
data.wk.ml.train$class <- factor(data.wk.ml.train$class)
data.wk.ml.train$week <- factor(data.wk.ml.train$week)

data.wk.ml.test$onpromotion <- as.factor(data.wk.ml.test$onpromotion)
data.wk.ml.test$perishable <- as.factor(data.wk.ml.test$perishable)
data.wk.ml.test$holiday <- as.factor(data.wk.ml.test$holiday)
data.wk.ml.test$item_nbr <- factor(data.wk.ml.test$item_nbr)
data.wk.ml.test$family <- factor(data.wk.ml.test$family)
data.wk.ml.test$class <- factor(data.wk.ml.test$class)
data.wk.ml.test$week <- factor(data.wk.ml.test$week)

levels(data.wk.ml.test$onpromotion) <- levels(data.wk.ml.train$onpromotion)
levels(data.wk.ml.test$perishable) <- levels(data.wk.ml.train$perishable)
levels(data.wk.ml.test$holiday) <- levels(data.wk.ml.train$holiday)
levels(data.wk.ml.test$item_nbr) <- levels(data.wk.ml.train$item_nbr)
levels(data.wk.ml.test$family) <- levels(data.wk.ml.train$family)
levels(data.wk.ml.test$class) <- levels(data.wk.ml.train$class)
levels(data.wk.ml.test$week) <- levels(data.wk.ml.train$week)

#One-hot encoding.
dmy <- dummyVars("~.", data = data.wk.ml.train)
data.wk.ml.train.dmy <- data.frame(predict(dmy, newdata = data.wk.ml.train))
dmy <- dummyVars("~.", data = data.wk.ml.test)
data.wk.ml.test.dmy <- data.frame(predict(dmy, newdata = data.wk.ml.test))

#Fitting and scoring various machine learning algorithms.
#Linear regression.
ml.lm.fit <-train(x = data.wk.ml.train[,-4], y = data.wk.ml.train$unit_sales, method = 'lm', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.lm.pred <- predict.train(object=ml.lm.fit,data.wk.ml.test[,-4],type="raw")
df.ml.lm.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.lm.pred)

#Model diagnostics
summary(ml.lm.fit)
#Plotting residuals
ml.lm.pred.resid <- resid(ml.lm.fit)
df.ml.lm.pred.resid <- cbind.data.frame(data.wk.ml.train, resid = ml.lm.pred.resid)
ggplot(df.ml.lm.pred.resid) + geom_point(aes(x = unit_sales, y = ml.lm.pred.resid)) + geom_abline(intercept = 0, slope = 0) + labs(title = "Linear Regression Residual Plot", x = "actual", y = "predicted")
#Heavy autocorrelation

#Visualizing variable importance
ImpMeasure<-data.frame(varImp(ml.lm.fit)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
rownames(ImpMeasure) <- c()
ggplot(ImpMeasure[order(-ImpMeasure$Overall),][1:20,]) + geom_point(aes(y = Vars, x = Overall))

rmse(as.vector(data.wk.ml.test$unit_sales), as.vector(ml.lm.pred))
ggplot(df.ml.lm.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - LINEAR REGRESSION - 53 core SKUs", x = "actual", y = "predicted")

#Random Forest regressor.
ml.rf.fit <-train(x = data.wk.ml.train[,-4], y = data.wk.ml.train$unit_sales, method = 'rf',  trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.rf.pred <- predict.train(object=ml.rf.fit,data.wk.ml.test[,-4],type="raw")
df.ml.rf.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.rf.pred)
rmse(as.vector(data.wk.ml.test$unit_sales), as.vector(ml.rf.pred))
ggplot(df.ml.rf.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - RANDOM FOREST - 53 core SKUs", x = "actual", y = "predicted")

#Knn regressor.
ml.knn.fit <-train(x = data.wk.ml.train.dmy[,!names(data.wk.ml.train.dmy) == "unit_sales"], y = data.wk.ml.train.dmy$unit_sales, method = 'knn', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.knn.pred <- predict.train(object=ml.knn.fit, data.wk.ml.test.dmy[!names(data.wk.ml.test.dmy) == "unit_sales"],type="raw")
df.ml.knn.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.knn.pred)
rmse(as.vector(data.wk.ml.test$unit_sales), as.vector(ml.knn.pred))
ggplot(df.ml.knn.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - KNN - 53 core SKUs", x = "actual", y = "predicted")

#Gradient boosting machine regressor.
ml.gbm.fit <-train(x = data.wk.ml.train[,-4], y = data.wk.ml.train$unit_sales, method = 'gbm', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.gbm.pred <- predict.train(object=ml.gbm.fit,data.wk.ml.test[,-4],type="raw")
df.ml.gbm.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.gbm.pred)
rmse(as.vector(data.wk.ml.test$unit_sales), as.vector(ml.gbm.pred))
ggplot(df.ml.gbm.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - GBM - 53 core SKUs", x = "actual", y = "predicted")

#Neural network regressor.
ml.nnet.fit <-train(x = data.wk.ml.train.dmy[,!names(data.wk.ml.train.dmy) == "unit_sales"], y = data.wk.ml.train.dmy$unit_sales, method = 'brnn', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.nnet.pred <- predict.train(object=ml.nnet.fit,data.wk.ml.test.dmy[!names(data.wk.ml.test.dmy) == "unit_sales"],type="raw")
df.ml.nnet.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.nnet.pred)
rmse(as.vector(data.wk.ml.test$unit_sales), as.vector(ml.nnet.pred))
ggplot(df.ml.nnet.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - BAYESIAN NEURAL NETWORK - 53 core SKUs", x = "actual", y = "predicted")

#Bagged decision tree regressor.
ml.treebag.fit <-train(x = data.wk.ml.train.dmy[,!names(data.wk.ml.train.dmy) == "unit_sales"], y = data.wk.ml.train.dmy$unit_sales, method = 'treebag', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.treebag.pred <- predict.train(object=ml.treebag.fit,data.wk.ml.test.dmy[!names(data.wk.ml.test.dmy) == "unit_sales"],type="raw")
df.ml.treebag.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.treebag.pred)
rmse(as.vector(data.wk.ml.test$unit_sales), as.vector(ml.treebag.pred))
ggplot(df.ml.treebag.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - BAGGED DECISION TREE - 53 core SKUs", x = "actual", y = "predicted")

#XGBoost regressor.
ml.xgbLinear.fit <-train(x = data.wk.ml.train.dmy[,!names(data.wk.ml.train.dmy) == "unit_sales"], y = data.wk.ml.train.dmy$unit_sales, method = 'xgbLinear', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.xgbLinear.pred <- predict.train(object=ml.xgbLinear.fit, data.wk.ml.test.dmy[!names(data.wk.ml.test.dmy) == "unit_sales"],type="raw")
df.ml.xgbLinear.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.xgbLinear.pred)
rmse(as.vector(data.wk.ml.test.dmy$unit_sales), as.vector(ml.xgbLinear.pred))
ggplot(df.ml.xgbLinear.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - XGBOOST - 53 core SKUs", x = "actual", y = "predicted")

#Support vector machines regressor.
ml.svmLinear.fit <-train(x = data.wk.ml.train.dmy[,!names(data.wk.ml.train.dmy) == "unit_sales"], y = data.wk.ml.train.dmy$unit_sales, method = 'svmLinear', trControl = trainControl(method = "none", number = 1, repeats = 1))
ml.svmLinear.pred <- predict.train(object=ml.svmLinear.fit, data.wk.ml.test.dmy[!names(data.wk.ml.test.dmy) == "unit_sales"],type="raw")
df.ml.svmLinear.pred <- cbind.data.frame(data.wk.fcst.test.mean[1:5], unit_sales_pred = ml.svmLinear.pred)
rmse(as.vector(data.wk.ml.test.dmy$unit_sales), as.vector(ml.svmLinear.pred))
ggplot(df.ml.svmLinear.pred) + geom_point(aes(x = unit_sales, y = unit_sales_pred)) + geom_abline(intercept = 0, slope = 1) + labs(title = "predicted vs actual - SVM - 53 core SKUs", x = "actual", y = "predicted")
