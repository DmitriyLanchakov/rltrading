#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Статистика арбитража Календарю на Брент
#' Данные: робот через Смартком ru.sazan.arbitrage
#'  
#' #' 2016-04-07 | rlukerin@gmail.com
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(data.table)
library(ggplot2)
options(digits.secs=3)

setwd("~/repos/SAZAN/robots/ArbCalndarBrent/")
DT<-read.csv("2016-04-07-0.log", head=F, stringsAsFactors = F, col.names=1:5)
DT<-data.table(DT)
DT<-DT[X2==" Spread Bid/Ask"]
DT[,X3:=as.numeric(X3)]
DT[,X4:=as.numeric(X4)]
summary(DT[,.(X3, X4)])

DT[,X1:=as.POSIXct(strptime(X1,format="%d-%m-%Y %H:%M:%OS"))]

ggplot()+
  geom_point(data=DT,aes(x=X1, y=X3), color="darkgreen")+
  geom_point(data=DT,aes(x=X1, y=X4), color="darkred")+ scale_y_continuous(breaks=seq(-1,1,0.1))

