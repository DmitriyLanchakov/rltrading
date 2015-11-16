library(dplyr)
library(ggplot2)
library(tidyr)
library(xts)
options(digits.secs=3)

#_______________________ TICK, BID, ASK DATA ANALYSIS __________________________

fname<-"g:/TRADE/SAZAN/ru.sazan.trader.smartcom.quote/ru.sazan.trader.smartcom.quote/bin/Debug/"
fname<-paste(fname,"Ticks-Si-3.15_FT-0.log", sep="")
tickData<-read.csv(fname, header=FALSE,stringsAsFactors=FALSE)

names(tickData)<-c("datetime", 
                   "datetimebroker",
                   "symbol", 
                   "price", 
                   "volume")

tickData<-xts(x=tickData[,c(-1,-2, -3)],
              order.by=as.POSIXct(strptime(tickData$datetime,"%d/%m/%Y %H:%M:%OS")))

range=""
df<-data.frame(datetime=as.POSIXct(index(tickData[range])),as.data.frame(tickData[range]))

qplot(x=datetime,
      y=price,
      data=df, 
      geom=c("point", "line"),
      colour=volume)

# FORECAST
training = diff(log(tickData$price["T16:17:10/T16:18:49"]))
testing = diff(log(tickData$price["T16:18:49/T16:18:50"]))
tstrain = ts(training$price)
tstest = ts(testing$price, start=nrow(training))

library(forecast);
#fit<-bats(tstrain,use.parallel=FALSE)
#fit<-ets(tstrain)
fit<-auto.arima(tstrain)
fc<-forecast(fit, h=nrow(testing))
plot(fc)
lines(rbind(tstrain,tstest),col="red")

sum(tstest<=fc$upper[,1] & tstest>fc$lower[,1])/length(tstest)

#BREAKOUT

#devtools::install_github("twitter/BreakoutDetection")
library(BreakoutDetection)


res = breakout(df$price[1:1000], min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot



# FORECAST
from<-as.Date("2015-02-12")
to<-as.Date("2015-02-13")
period="1min"

symbols<-c("SiH5 (03.2015)")

for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
symbols<-toupper(symbols)

training = diff(log(get(symbols[1])$Close["2015-02-13 10:10/2015-02-13 11:10"]))
testing = diff(log(get(symbols[1])$Close["2015-02-13 11:10/2015-02-13 11:20"]))

training = get(symbols[1])$Close["2015-02-13 15:10/2015-02-13 16:10"]
testing = get(symbols[1])$Close["2015-02-13 16:10/2015-02-13 16:20"]

tstrain = ts(training)
tstest = ts(testing, start=nrow(training))

library(forecast);
#fit<-bats(tstrain,use.parallel=FALSE)
#fit<-ets(tstrain)
fit<-bats(tstrain)
fc<-forecast(fit, h=nrow(testing))
plot(fc)
lines(rbind(tstrain,tstest),col="red")

sum(tstest<=fc$upper[,1] & tstest>fc$lower[,1])/length(tstest)


