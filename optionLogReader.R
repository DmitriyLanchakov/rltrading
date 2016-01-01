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
#' systemDateTime dd-MM-yyyy h:mm:ss.fff
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
#' Март	    C	O
#' Апрель	D	P
#' Май	    E	Q
#' Июнь  	F	R
#' Июль 	G	S
#' Август	H	T
#' Сентябрь	I	U
#' Октябрь	J	V
#' Ноябрь	K	W
#' Декабрь	L	X
library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(NMOF)
library(DEoptim)
library(fOptions)
library(rusquant)
options(digits.secs=3)
fname<-"f:/TRADE/Data/research/option/29122015/"
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

dtFormat<-"%d-%m-%Y %H:%M:%OS"
tickDT[,"systemDateTime":=as.POSIXct(strptime(systemDateTime,dtFormat))]
bidaskDT[,"systemDateTime":=as.POSIXct(strptime(systemDateTime,dtFormat))]

dtFormat<-"%m/%d/%Y %H:%M:%OS"
tickDT[,"brokerDateTime":=as.POSIXct(strptime(brokerDateTime,dtFormat))]
bidaskDT[,"brokerDateTime":=as.POSIXct(strptime(brokerDateTime,dtFormat))]
bidaskDT<-bidaskDT[Row==0,]
BAbidaskDT<-bidaskDT[Symbol=="Si-3.16_FT"]
CHAINbidaskDT<-bidaskDT[Symbol!="Si-3.16_FT"]

ExpDate<-as.POSIXct("2016-01-21")
CHAINbidaskDT[,Expiration:=ExpDate]
CHAINbidaskDT[,Strike:=as.numeric(tstrsplit(Symbol,"*[A-z]{2,2}")[[2]])]
CHAINbidaskDT[,ExpMonthYear:=gsub("[A-z]{2,2}[0-9]{2,}B","",Symbol)]
CHAINbidaskDT[,ExpMonth:=gsub("[0-9]","",ExpMonthYear)]
CHAINbidaskDT[,ExpYear:=gsub("[A-z]","",ExpMonthYear)]
CHAINbidaskDT[,Symbol:="Si-3.16_FT"]
CHAINbidaskDT[,OptType:=ifelse(match(ExpMonth,LETTERS)>12,"PA", "CA")]

CHAINbidaskDT<-CHAINbidaskDT[,.(systemDateTime,Bid,BidSize,Ask,AskSize,Strike,OptType,Expiration)]
CHAINbidaskDT<-CHAINbidaskDT[Bid>0&Ask>0]
CHAINbidaskDT[,spread:=Ask-Bid]
CHAINbidaskDT<-CHAINbidaskDT[spread<100]

BAbidaskDT<-BAbidaskDT[,.(systemDateTime,Symbol,Bid,BidSize,Ask,AskSize)]
BAbidaskDT<-BAbidaskDT[Bid>0&Ask>0]
BAbidaskDT[,spread:=Ask-Bid]
BAbidaskDT<-BAbidaskDT[spread<100]

setkey(BAbidaskDT,systemDateTime)
setkey(CHAINbidaskDT,systemDateTime)

BACHAINDT<-BAbidaskDT[CHAINbidaskDT,roll=T]
setnames(BACHAINDT,c("DateTime", "Symbol","BBid","BBidSize","BAsk", "BAskSize","Bspread",
         "OBid","OBidSize", "OAsk", "OAskSize","Strike","OptType","Expiration","Ospread"))

BACHAINDT[,PriceMid:=(BBid+BAsk)/2,]
BACHAINDT[,PRICE:=(OBid+OAsk)/2,]

BACHAINDT[,tau:=as.numeric(difftime(Expiration,DateTime, "days"))/365]
BACHAINDT[,id:=.I]
BACHAINDT[,GBSIV:=GBSVolatility(price=PRICE, 
                                  TypeFlag = strtrim(tolower(OptType),1), 
                                  S=PriceMid,
                                  X=Strike,
                                  Time=tau,
                                  r=0,
                                  b=0,
                                  maxiter=100), by=id]
nStrikes<-35
StrikeStep<-250
MaxStrike<-floor(BACHAINDT[.N]$PriceMid/StrikeStep)*StrikeStep+nStrikes*StrikeStep
MinStrike<-floor(BACHAINDT[.N]$PriceMid/StrikeStep)*StrikeStep-nStrikes*StrikeStep

ggplot()+
    geom_point(data=BACHAINDT[Strike>=MinStrike & Strike<=MaxStrike & GBSIV<1],aes(x=Strike,y=GBSIV*100,colour=format(DateTime,"%d%H")))+
    geom_line(data=BACHAINDT[Strike>=MinStrike & Strike<=MaxStrike & GBSIV<1],aes(x=Strike,y=GBSIV*100,colour=format(DateTime,"%d%H")))+
    geom_vline(xintercept=BACHAINDT[.N]$PriceMid)+
    scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StrikeStep*2)) + 
    scale_y_continuous(breaks=seq(1,100,0.25))+
    annotate("text", label = "HistPrice IV by hours", x = MinStrike*1.2, y = 13, size = 4, colour = "forestgreen")



