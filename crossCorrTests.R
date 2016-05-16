#Cross corellation tests
library(data.table)
options(digits.secs=3)
#setwd("f:/TRADE/Data/research/_landy/")
#load("Si-12.152015-09-18.RData")
#load("RTS-12.152015-09-18.RData")
#load("SBRF-12.152015-09-18.RData")
load("f:/TRADE/Data/research/SI/Si-3.16_FT2015-12-21.RData")
symb1<-df
rm(df)
#load("RTS-12.152015-09-18.RData")
#load("SBRF-12.152015-09-18.RData")

load("f:/TRADE/Data/research/USDTOM/USD000UTSTOM2015-12-21.RData")
symb2<-df
rm(df)

symb1[, priceMid:=(askprice0+bidprice0)/2]
symb2[, priceMid:=(askprice0+bidprice0)/2]

# Return Cross Corr
symb1[, symb1Ret:=log(priceMid)-shift(log(priceMid),1, type="lag")]
symb2[, symb2Ret:=log(priceMid)-shift(log(priceMid),1, type="lag")]

symb1[, symb1Ret:=-log(price)+shift(log(price),1, type="lead")]
symb2[, symb2Ret:=-log(price)+shift(log(price),1, type="lead")]

setkey(symb1, datetime)
setkey(symb2, datetime)
symbs<-symb1[symb2]
symbs<-symbs[complete.cases(symbs)]

dfdate<-format(symb1$datetime[2], "%Y-%m-%d")
downlimit<-as.POSIXct(paste(dfdate,"10:05:00.000"))
uplimit<-as.POSIXct(paste(dfdate,"18:00:00.000"))
symbs<-symbs[datetime>downlimit & datetime<uplimit]

ccf(symbs$symb1Ret,symbs$symb2Ret, lag.max=100)

ggplot(data=symbs)+
    geom_line(aes(datetime,i.askprice0/(bidprice0*100) -1), colour="mediumaquamarine", alpha=I(0.5))+
    geom_line(aes(datetime,i.bidprice0/(askprice0*100) -1), colour="lightcoral", alpha=I(0.5))

#geom_point(aes(datetime,price*100), colour="lightcoral", alpha=I(0.5))+
#geom_point(aes(datetime,i.price), colour="mediumaquamarine",alpha=I(0.5))

# Volume disbalance cross corr test

levelF<-2
symb1[,bidCum:=rowSums(.SD),.SDcols=paste("bidvolume",0:levelF,sep="")]
symb1[,askCum:=rowSums(.SD),.SDcols=paste("askvolume",0:levelF,sep="")]
symb1[,logF:=log(bidCum)-log(askCum)]
symb1<-symb1[datetime>downlimit & datetime<uplimit]
symb1<-symb1[complete.cases(symb1)]

ccf(symb1$symb1Ret,symb1$logF, lag.max=40)
#acf(symb1$symb1Ret)
