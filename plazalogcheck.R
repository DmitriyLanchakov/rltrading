library(data.table)
setwd("G:/TRADE/Data/research")
load("tba.RData")
options(digits.secs=3)
banames<-c("id", "bidprice0","bidprice1", "bidprice2",
   "bidvolume0","bidvolume1","bidvolume2","askprice0","askprice1","askprice2",
   "askvolume0","askvolume1","askvolume2")
setnames(tickbidaskdt, banames)

tickbidaskdt<-cbind(tickbidaskdt,ticks)
tickbidaskdt<-tickbidaskdt[NonSystem!=TRUE]

dtFormat<-"%d.%m.%Y %H:%M:%OS"
tickbidaskdt[,"datetime":=as.POSIXct(strptime(ExchTime,dtFormat))]

tickbidaskdt[,buysell:=ifelse(Buy==TRUE, "Buy", "Sell")]

tbanames<-c("datetime", "DealPrice","Amount","buysell", "bidprice0","bidprice1", "bidprice2",
            "bidvolume0","bidvolume1","bidvolume2","askprice0","askprice1","askprice2",
            "askvolume0","askvolume1","askvolume2")


dfplaza<-tickbidaskdt[,.SD,.SDcols=tbanames]

dfnames<-c("datetime", "price","volume","buysell", "bidprice0","bidprice1", "bidprice2",
            "bidvolume0","bidvolume1","bidvolume2","askprice0","askprice1","askprice2",
            "askvolume0","askvolume1","askvolume2")

setnames(dfplaza, dfnames)
rm(tickbidaskdt,ticks)
gc()

dfdate<-format(dfplaza$datetime[2], "%Y-%m-%d")
downlimit<-as.POSIXct(paste(dfdate,"15:10:00.000"))
uplimit<-as.POSIXct(paste(dfdate,"18:00:00.000"))
dfplaza<-dfplaza[datetime>downlimit & datetime<uplimit]


#library(ggplot2)
source('G:/TRADE/R/rltrading/sandBox/OrderBook/orderbookggplot.R', echo=TRUE)
orderbookggplot(df[price<askprice0 & price>bidprice0])

df<-dfplaza
rm(dfplaza)


