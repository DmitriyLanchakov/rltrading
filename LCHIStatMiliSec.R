library(data.table)
library(ggplot2)
library(bit64)
options(digits.secs=3)

setwd("~/repos/Data/research/LCHI")

#robot_SprStealer 1644%
fname<-"ftp://ftp.moex.com/pub/info/stats_contest/2015/all/2_49083.zip"

#PhilMaN 2nd place 150% 1 443 510,78 rub
fname<-"ftp://ftp.moex.com/pub/info/stats_contest/2015/all/2_48871.zip"

#PartalovAlexey 3rd place 149%
fname<-"ftp://ftp.moex.com/pub/info/stats_contest/2015/all/2_48675.zip"

obDT<-makeBidAskData("OrdLog.RTS-12.15.2015-09-18.bidask-0.log")
setkey(obDT,datetime)


tickDT<-makeTickData("OrdLog.RTS-12.15.2015-09-18.tick-0.log")
setkey(tickDT,datetime)
tickDT<-tickDT[,.SD[1], by=dealid]


download.file(fname, "data.zip")
unzip("data.zip")


tradeDT<-fread("2_49083.csv",sep=";", stringsAsFactors=FALSE)
setnames(tradeDT, c("datetime", "symbol","position", "price"))
dtFormat<-"%Y-%m-%d %H:%M:%OS"
tradeDT[,"datetime":=as.POSIXct(strptime(datetime,dtFormat))]

tradeDT[,.N, by=symbol]
tradeDT[,sum(price*position), by=symbol]
tradeDT[,sum(position), by=symbol]

tradeDT[,dtsec:=datetime]
tradeDT<-tradeDT[symbol=="RIZ5"&  format(datetime,"%Y%m%d" )=="20150918"]
tradeDT<-tradeDT[as.numeric(format(datetime,"%H")) %in% 10:18]


dtFormat<-"%Y-%m-%d %H:%M:%S"
tickDT[, dtsec:=format(datetime,dtFormat)]
tickDT[,dtsec:=as.POSIXct(strptime(dtsec,dtFormat))]
tickDT<-tickDT[as.numeric(format(datetime,"%H")) %in% 10:18]

tickDT[,position:=volume*ifelse(buysell=="buy",1, -1)]
setkey(tickDT,dtsec,price,  position)
setkey(tradeDT, dtsec,price,  position)


tradeDT<-tickDT[tradeDT,][!is.na(dealid)]


ggplot(tradeDT[symbol=="RIZ5" & format(datetime,"%Y%m%d%H" )=="2015091812"])+
    geom_point(aes(x=datetime, y=price, color=as.factor(sign(position))))+
    geom_line(aes(x=datetime, y=price))


