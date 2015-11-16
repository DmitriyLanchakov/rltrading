library(rusquant)
library(ggplot2)
library(data.table)

#' 1min OHLC test
symbs<-c("MXZ5 (12.2015)","MMZ5 (12.2015)")
from<-as.Date("2015-11-16")
to<-Sys.Date()
period="1min"
for (symb in symbs)
  getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)

 
leftLeg<-data.table(DateTimeSymb=as.POSIXct(index(get(symbs[1]))),
           as.data.frame(get(symbs[1]), stringsAsFactors=FALSE))

rightLeg<-data.table(DateTimeSymb=as.POSIXct(index(get(symbs[2]))),
                    as.data.frame(get(symbs[2]), stringsAsFactors=FALSE))

setkey(leftLeg,DateTimeSymb)
setkey(rightLeg,DateTimeSymb)

leftLeg[rightLeg]

qplot(format(DateTimeSymb, "%m%d"),y=Close-i.Close*100, data=leftLeg[rightLeg], color=Volume,
       geom = c("violin", "jitter"))
qplot(DateTimeSymb,Close-i.Close*100,data=leftLeg[rightLeg],
      color=i.Volume)

summary(leftLeg[rightLeg][,Close-i.Close*100])

#' Tick-Bid-Ask test
options(digits.secs=3)
fname<-"g:/TRADE/Data/research/MIX-MXI/26102015/"
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

dtMXI<-bidaskDT[Symbol=="MXI-12.15_FT"]
dtMXI[,"MidPrice":=(Ask+Bid)/2]

dtMIX<-bidaskDT[Symbol=="MIX-12.15_FT"]
dtMIX[,"MidPrice":=(Ask+Bid)/2]

setkey(dtMXI, systemDateTime)
setkey(dtMIX, systemDateTime)

dtU<-dtMIX[dtMXI, nomatch=0]

dtU[,"Spread":=MidPrice-i.MidPrice*100]
qplot(systemDateTime,Spread,data=dtU)
summary(dtU$Spread)

##Tick Bid Ask MXI

setkey(tickDT, systemDateTime)
setkey(bidaskDT, systemDateTime)

tbaDTMXI<-bidaskDT[Symbol=="MXI-12.15_FT"][tickDT[Symbol=="MXI-12.15_FT"],roll=T]
dfnames<-c("datetime", "price","volume","buysell", "bidprice0","bidprice1", "bidprice2",
           "bidvolume0","bidvolume1","bidvolume2","askprice0","askprice1","askprice2",
           "askvolume0","askvolume1","askvolume2")

tbaDTMXI[,c("datetime", "price","volume","buysell", "bidprice0","bidprice1", "bidprice2",
            "bidvolume0","bidvolume1","bidvolume2","askprice0","askprice1","askprice2",
            "askvolume0","askvolume1","askvolume2"):=
           .(systemDateTime,Price,Qty,SellBuy,Bid,Bid,Bid,BidSize,BidSize,BidSize,
             Ask,Ask,Ask,AskSize,AskSize,AskSize)]

df<-data.frame(tbaDTMXI[,.SD,.SDcols=dfnames])
save(df, file="MXI26102015.RData")

#Sazan SpreadValue tests
options(digits.secs=3)
fname<-"g:/TRADE/Data/research/MIX-MXI/30102015/"
setwd(fname)

fileList<-dir()

spreadDF<-rbindlist(lapply(fileList,
                  FUN=read.csv,
                  col.names = paste("V", 1:20, sep=""),
                  header=FALSE,
                  fill=TRUE,
                  stringsAsFactors=FALSE))
spreadDF<-data.table(spreadDF)
dtFormat<-"%d-%m-%Y %H:%M:%OS"
spreadDF[,"DateTime":=as.POSIXct(strptime(V1,dtFormat))]
spreadDF<-spreadDF[grepl("CalculateSpreadOnOrderBookChange", V2)]

spreadDF[,"V6":=sub("\\.$","",V6)]
spreadDF<-spreadDF[,.(DateTime, as.numeric(V5),as.numeric(V6))]
spreadDF<-spreadDF[order(-DateTime)]
summary(spreadDF)
qplot(DateTime, V3-V2, data=spreadDF[V2>250 | V3<120])