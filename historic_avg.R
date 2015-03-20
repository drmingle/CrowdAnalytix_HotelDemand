## setting working directory
path <- ""
setwd(path)


## loading libraries
library(reshape2)
library(plyr)


## loading data
city1 <- read.csv("CAX_DemandForecasting_City1_Train.csv", stringsAsFactors=F)
city2 <- read.csv("CAX_DemandForecasting_City2_Train.csv", stringsAsFactors=F)
city3 <- read.csv("CAX_DemandForecasting_City3_Train.csv", stringsAsFactors=F)
city4 <- read.csv("CAX_DemandForecasting_City4_Train.csv", stringsAsFactors=F)
city5 <- read.csv("CAX_DemandForecasting_City5_Train.csv", stringsAsFactors=F)

test_public <- read.csv("CAX_DemandForecasting_All_Cities_2013.csv", stringsAsFactors=F)

test <- read.csv("CAX_2014_Forecast.csv", stringsAsFactors=F)


## cleaning data
test_public$Date <- gsub("/", "-", test_public$Date)
test_public$Date <- gsub("-13", "-2013", test_public$Date)

# train data
train <- rbind(city1[,c(2,3,11)], city2[,c(2,3,11)], city3[,c(2,3,11)], city4[,c(2,3,15)], city5[,c(2,3,11)], test_public[,c(2,3,15)])
names(train)[1] <- "City"

# creating calendar features
train$City <- as.integer(substr(train$City,6,6))
train$Date <- as.Date(substr(train$Date,1,10), "%d-%m-%Y")
train$Year <- as.numeric(substr(train$Date,1,4))
train$Month <- as.numeric(substr(train$Date,6,7))
train$Week <- as.numeric(strftime(train$Date, "%W"))
train$Day <- as.numeric(substr(train$Date,9,10))
train$Weekday <- weekdays(train$Date)

train$Quarter <- 0
train$Quarter[train$Week <= 52] <- 4
train$Quarter[train$Week <= 39] <- 3
train$Quarter[train$Week <= 26] <- 2
train$Quarter[train$Week <= 13] <- 1
train$Quarter[train$Week <= 51 & train$Year == 2013] <- 4
train$Quarter[train$Week <= 38 & train$Year == 2013] <- 3
train$Quarter[train$Week <= 25 & train$Year == 2013] <- 2
train$Quarter[train$Week <= 12 & train$Year == 2013] <- 1

# test data
test$Date <- as.Date(test$Date, "%d-%m-%Y")
test$City <- as.integer(substr(test$CITY,6,6))
test$Week <- as.numeric(strftime(test$Date, "%W"))
test$Weekday <- weekdays(test$Date)

test$Quarter <- 0
test$Quarter[test$Week <= 51] <- 4
test$Quarter[test$Week <= 38] <- 3
test$Quarter[test$Week <= 25] <- 2
test$Quarter[test$Week <= 12] <- 1


## previous years' demand on same weekday (Model 1)
py1 <- subset(train, select=c("City", "Date", "AGGREGATE.DEMAND"))
names(py1)[3] <- "one_year_back"
py1$Date <- py1$Date + 364

py2 <- subset(train, select=c("City", "Date", "AGGREGATE.DEMAND"))
names(py2)[3] <- "two_years_back"
py2$Date <- py2$Date + 728

py3 <- subset(train, select=c("City", "Date", "AGGREGATE.DEMAND"))
names(py3)[3] <- "three_years_back"
py3$Date <- py3$Date + 1092

test <- merge(merge(merge(test, py1, all.x=T, by=c("City", "Date")), py2, all.x=T, by=c("City", "Date")), py3, all.x=T, by=c("City", "Date"))

# weighted average with 50%-30%-20% for 2013-2012-2011 respectively
test$pred1 <- 0.5*test$one_year_back + 0.3*test$two_years_back + 0.2*test$three_years_back
test$pred1[test$Date == "2014-12-31"] <- 0.5*train$AGGREGATE.DEMAND[train$Date == "2013-12-31"] + 0.3*train$AGGREGATE.DEMAND[train$Date == "2012-12-31"] + 0.2*train$AGGREGATE.DEMAND[train$Date == "2011-12-31"]


## Splitting previous years' weekly aggregate (Model 2)
train2 <- subset(train, Year >= 2013 | (Week > 0 & Week < 53))
train2$Week[train2$Year < 2013] <- train2$Week[train2$Year < 2013] - 1
train2$count <- 1

# weights of 50%-30%-20% for 2013-2012-2011 respectively
train2$AGGREGATE.DEMAND[train2$Year == 2013] <- 0.5*train2$AGGREGATE.DEMAND[train2$Year == 2013]
train2$AGGREGATE.DEMAND[train2$Year == 2012] <- 0.3*train2$AGGREGATE.DEMAND[train2$Year == 2012]
train2$AGGREGATE.DEMAND[train2$Year == 2011] <- 0.2*train2$AGGREGATE.DEMAND[train2$Year == 2011]

# weekly aggregate
weekly <- dcast(train2, City + Quarter + Week ~ count, sum, value.var = "AGGREGATE.DEMAND")
names(weekly)[4] <- "TotalDemand"

weekday <- dcast(train2, City + Quarter + Weekday ~ count, sum, value.var = "AGGREGATE.DEMAND")
names(weekday)[4] <- "DayTotal"
weektot <- dcast(train2, City + Quarter ~ count, sum, value.var = "AGGREGATE.DEMAND")
names(weektot)[3] <- "CityTotal"

# proportion of weekday in each quarter
weekday <- merge(weekday, weektot, by = c("City", "Quarter"))
weekday$dayratio <- weekday$DayTotal / weekday$CityTotal

d <- merge(weekly, unique(weekday$Weekday))
names(d)[5] <- "Weekday"

# splitting weekly demand into weekday demand
d <- merge(d, weekday, by = c("City", "Quarter", "Weekday"))
d$pred2 <- d$TotalDemand * d$dayratio
d <- subset(d, select = c("City", "Quarter", "Week", "Weekday", "pred2"))

test <- merge(test, d, all.x=T, by=c("City", "Quarter", "Week", "Weekday"))
test$pred2[is.na(test$pred2)] <- test$pred1[is.na(test$pred2)]


# smoothening by weighted average of 80%-10%-10% for same day - previous week - next week predictions
ldf <- lapply(unique(test$City), function(k)
              {
                temp <- test[test$City == k,]
                temp <- temp[order(temp$Date),]
                temp$pred_py <- temp$pred1
                temp$pred_ws <- temp$pred2
                
                for (i in 8:(nrow(temp)-7))
                {
                  temp$pred_py[i] <- 0.8*temp$pred1[i] + 0.1*temp$pred1[i-7] + 0.1*temp$pred1[i+7]
                  temp$pred_ws[i] <- 0.8*temp$pred2[i] + 0.1*temp$pred2[i-7] + 0.1*temp$pred2[i+7]
                }
                
                for (i in 1:7)
                {
                  temp$pred_py[i] <- 0.9*temp$pred1[i] + 0.1*temp$pred1[i+7]
                  temp$pred_ws[i] <- 0.9*temp$pred2[i] + 0.1*temp$pred2[i+7]
                }
                
                for (i in (nrow(temp)-6):nrow(temp))
                {
                  temp$pred_py[i] <- 0.9*temp$pred1[i] + 0.1*temp$pred1[i-7]
                  temp$pred_ws[i] <- 0.9*temp$pred2[i] + 0.1*temp$pred2[i-7]
                }
                
                return(temp)
              })

test <- ldply(ldf, data.frame)


# final prediction is average of Model-1 and Model-2
test$AggregateDemand <- 0.5*test$pred_py + 0.5*test$pred_ws


# output file
submit <- subset(test, select = c("Obs.ID", "CITY", "Date", "AggregateDemand"))
submit$Date <- strftime(strptime(submit$Date, "%Y-%m-%d"), "%d-%m-%Y")

write.csv(submit, "./Forecast2014.csv", row.names=F, quote=T)
