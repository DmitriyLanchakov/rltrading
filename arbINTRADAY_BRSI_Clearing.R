#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Стоимость Брента до и после вечернего клиринга
#' INTRADAY
#' 2016-08-22 | rlukerin@gmail.com
#' 
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Месяц	Код фьючерса
#' Январь	F
#' Февраль	G
#' Март	H
#' Апрель	J
#' Май	K
#' Июнь	M
#' Июль	N
#' Август	Q
#' Сентябрь	U
#' Октябрь	V
#' Ноябрь	X
#' Декабрь	Z
#' 

library(data.table)
library(ggplot2)
from=as.Date("2016-02-02")
to=as.Date("2016-03-01")
symbList=c("BRH6", "SiH6")
symbDT<-getSymbol.MOEX(from=from, to=to,symbList=symbList)


symbPS<-symbDT[,.SD[code==symbList[2] & format(dat_time,"%H%M")==1405,price][1],by=format(dat_time, "%Y%m%d")][, V2:=shift(V1, type="lag")][]
symbDT[, format:=format(dat_time, "%Y%m%d")]
setkey(symbDT, format)
setkey(symbPS, format)
symbDT<-symbDT[symbPS]
symbDT[, symbPriceStepEv:=V1]
symbDT[, symbPriceStepDay:=V2]

#symbDT[code==symbList[1],price:=price*symbPriceStep/100]
#symbDT[code==symbList[2],price:=price*1]


setkey(symbDT, dat_time)


spreadDT<-symbDT[code==symbList[1]][symbDT[code==symbList[2]]][!is.na(code)]#[as.Date(dat_time)>=to]

spreadDT<-spreadDT[format(dat_time,"%H%M") %in% c(1844:1849, 1901:1905)]

spreadDT<-spreadDT[complete.cases(spreadDT)]

spreadDT[format(dat_time,"%H%M") %in% c(1844:1849),pos:="OPEN"]
spreadDT[format(dat_time,"%H%M") %in% c(1901:1905),pos:="CLOSE"]

posDT<-spreadDT[,.(mean(price),mean(i.price),mean(symbPriceStepDay),mean(symbPriceStepEv)),by=.(format,pos)]
setnames(posDT,c("date","pos","PriceBR","PriceSI","StepDay","StepEv"))
posDT[,flag:=sign(-StepDay+StepEv)]

posDT[complete.cases(posDT)]
posDT[,Amount:=PriceBR*10*flag*ifelse(pos=="OPEN",StepDay/100,-StepEv/100)]
posDT[,Amount:=PriceBR*10*flag*ifelse(pos=="OPEN",StepEv/100,-StepEv/100)]


plDT<-posDT[complete.cases(posDT),.(-sum(Amount),.N),by=date][N>1]
plDT[, profit:=cumsum(V1)]

ggplot(plDT,aes(date, profit, group=1))+
           geom_point()+
           geom_line()

plDT
