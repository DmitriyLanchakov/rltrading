#Option Log reader

#' Codebook for Bids and Asks log
#' --------------------------------------
#' systemDateTime dd/MM/yyyy h:mm:ss.fff
#' Symbol
#' brokerDateTime MM/dd/YYYY hh:mm:ss
#' Row
#' NRows
#' Bid
#' BidSize
#' Ask
#' AskSize

#' Codebook for Ticks log
#' --------------------------------------
#' systemDateTime dd/MM/yyyy h:mm:ss.fff
#' Symbol
#' brokerDateTime MM/dd/YYYY hh:mm:ss
#' Price
#' Qty
#' SellBuy flag

#' Option Symbol Codebook
#' --------------------------------------
#' C – код базового актива, 2 символа,
#' P – цена страйк, переменное количество символов,
#' К – тип расчетов,
#' M – месяц исполнения (а также тип для опциона), 1 символ,
#' Y – год исполнения, 1 символ,
#' W – признак недельного опциона, 1 символ
#' 
#' ---M---
#' Month Call Put
#' Январь	A	 M
#' Февраль	B	N
#' Март	C	O
#' Апрель	D	P
#' Май	E	Q
#' Июнь	F	R
#' Июль	G	S
#' Август	H	T
#' Сентябрь	I	U
#' Октябрь	J	V
#' Ноябрь	K	W
#' Декабрь	L	X
library(data.table)
options(digits.secs=3)
fname<-"g:/TRADE/Data/research/option/"
setwd(fname)

fileList<-dir()
fileList[grepl("Ticks",fileList)]
fileList[grepl("BidAsk",fileList)]


tickDT<-rbindlist(lapply(fileList[grepl("Ticks",fileList)],
                 FUN=fread,
                 sep=",",
                 header=FALSE, 
                 stringsAsFactors=FALSE))

bidaskDT<-rbindlist(lapply(fileList[grepl("BidAsk",fileList)],
                         FUN=fread,
                         sep=",",
                         header=FALSE, 
                         stringsAsFactors=FALSE))

bidaskHeader<-c("systemDateTime",
              "Symbol",
              "brokerDateTime",
              "Row",
              "NRows",
              "Bid",
              "BidSize",
              "Ask",
              "AskSize")

tickHeader<-c("systemDateTime",
              "Symbol",
              "brokerDateTime",
              "Price",
              "Qty",
              "SellBuy")

setnames(tickDT,tickHeader)
setnames(bidaskDT,bidaskHeader)

dtFormat<-"%d/%m/%Y %H:%M:%OS"
tickDT[,"systemDateTime":=as.POSIXct(strptime(systemDateTime,dtFormat))]
bidaskDT[,"systemDateTime":=as.POSIXct(strptime(systemDateTime,dtFormat))]

dtFormat<-"%m/%d/%Y %H:%M:%OS"
tickDT[,"brokerDateTime":=as.POSIXct(strptime(brokerDateTime,dtFormat))]
bidaskDT[,"brokerDateTime":=as.POSIXct(strptime(brokerDateTime,dtFormat))]
bidaskDT<-bidaskDT[Row==0,]

dt1<-tickDT[Symbol=="Si-12.15_FT"]
dt2<-bidaskDT[Symbol=="Si-12.15_FT"]
setkey(dt1, systemDateTime)
setkey(dt2, systemDateTime)
dt3 <- data.table(systemDateTime = sort(unique(c(dt1$systemDateTime, dt2$systemDateTime))))
dtU<-dt1[dt2[dt3, roll=TRUE], roll=TRUE]
library(ggplot2)
qplot(x=systemDateTime,y=Price-(Ask+Bid)/2,data=dtU)

unique(bidaskDT$Symbol)



