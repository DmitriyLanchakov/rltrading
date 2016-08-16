#########################FUTURE ARBITRAGE СHECK####################################

library(rusquant)
library(scales)
library(ggplot2)
library(grid)
source('G:/BACKUP/GDRIVE/FinUniver/Diploma/Code/getsymbols.mfdru.R')

#Parameters
startDate<-as.Date("2014-05-16")
numdays <-150

#Loading Futures History
getSymbols.mfdru("GZU4 (09.2014)",
                 ,from=startDate,to=startDate+numdays, src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("SRU4 (09.2014)",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("VBU4 (09.2014)",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("LKU4 (09.2014)",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)

#Loading Stock History
getSymbols.mfdru("ГАЗПРОМ ао",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("Сбербанк",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("ВТБ ао",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("ЛУКОЙЛ",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)

#Fut/Stock Ratio
StFutRatio<-c(100,100,100000,10)

#LotNumbers
StLot<-c(10,10,10000,11)


timeWindow <- 'T10:00:00/T18:40:00'

#Costing F= S*exp(r*(Tf-t)) -d(exp(r*(Td-t)))+ Z

FUTSpread<-0 #3.5*0.01/100
MOEXSpread<-0 #3.5*0.01/100
Z<- 0.01/100
#Average REPO rate ~ NO RISK rate
r0<-0.08


FUTPosition<-cbind(`GZU4 (09.2014)`[timeWindow, 'GZU4 (09.2014).Close'],
                   `SRU4 (09.2014)`[timeWindow, 'SRU4 (09.2014).Close'],
                   `VBU4 (09.2014)`[timeWindow, 'VBU4 (09.2014).Close'],
                   `LKU4 (09.2014)`[timeWindow, 'LKU4 (09.2014).Close']
                    )

MOEXPosition<-cbind(`ГАЗПРОМ АО`[timeWindow, 'ГАЗПРОМ АО.Close']*StFutRatio[1],
                   `СБЕРБАНК`[timeWindow, 'СБЕРБАНК.Close']*StFutRatio[2],
                   `ВТБ АО`[timeWindow, 'ВТБ АО.Close']*StFutRatio[3],
                   `ЛУКОЙЛ`[timeWindow, 'ЛУКОЙЛ.Close']*StFutRatio[4])
#Date of expiration
DateExp<-as.POSIXct(strptime("2014-09-15 10:00:00 MSK","%Y-%m-%d %H:%M:%S"))

#Number of days till expiration
TimeToExp<-as.numeric(DateExp -index(MOEXPosition))/365


#Dividends
div<-c(7.2,
       3.2,
       0.00116,
       60)

DateDivFix<-c("27.06.2014 18:40:00 MSK",
                "06.06.2014 18:40:00 MSK",
                "19.06.2014 18:40:00 MSK",
                "26.06.2014 18:40:00 MSK")
DateDivFix<-as.POSIXct(strptime(DateDivFix,"%d.%m.%Y %H:%M:%S"))

DateDivClose<-c("15.07.2014 18:40:00 MSK",
                "11.06.2014 18:40:00 MSK",
                "27.06.2014 18:40:00 MSK",
                "11.07.2014 18:40:00 MSK")
DateDivClose<-as.POSIXct(strptime(DateDivClose,"%d.%m.%Y %H:%M:%S"))

TimeToDivClose<-data.frame(GAZP=as.numeric(DateDivClose[1] -index(MOEXPosition),unit="days"),
                  SBER=as.numeric(DateDivClose[2] -index(MOEXPosition),unit="days"),
                  VTB=as.numeric(DateDivClose[3] -index(MOEXPosition),unit="days"),
                  LKOH=as.numeric(DateDivClose[4] -index(MOEXPosition),unit="days"))

TimeToDivClose<-TimeToDivClose/365
DivPosition<-cbind     (div[1]*exp(r0*TimeToDivClose[,1])*StFutRatio[1],
                       div[2]*exp(r0*TimeToDivClose[,2])*StFutRatio[2],
                       div[3]*exp(r0*TimeToDivClose[,3])*StFutRatio[3],
                       div[4]*exp(r0*TimeToDivClose[,4])*StFutRatio[4])

DivPosition<-xts(DivPosition,index(MOEXPosition))

DivPosition[paste(as.character(DateDivClose[1]),"/"),1]<-0
DivPosition[paste(as.character(DateDivClose[2]),"/"),2]<-0
DivPosition[paste(as.character(DateDivClose[3]),"/"),3]<-0
DivPosition[paste(as.character(DateDivClose[4]),"/"),4]<-0

#Arb spread
ArbSpread<-log(FUTPosition*(1-Z))-log(MOEXPosition*(1-Z)*exp(r0*TimeToExp)-DivPosition)
ArbSpread<-ArbSpread*100

# layout(matrix(1:12, nrow=6)) 
# chartSeries(`ГАЗПРОМ АО`,name="GAZP", layout=NULL)
# chartSeries(`GZU4 (09.2014)`,name="GAZP FUT",layout=NULL)
# chartSeries(`СБЕРБАНК`,name="SBER", layout=NULL)
# chartSeries(`SRU4 (09.2014)`,name="SBER FUT",layout=NULL)
# chartSeries( `ВТБ АО`,name="VTB",layout=NULL)
# chartSeries(`VBU4 (09.2014)`,name="VTB FUT",layout=NULL)



ArbSpread.df<-data.frame(datetime=index(ArbSpread),ArbSpread, row.names=NULL)

ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= GZU4..09.2014..Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(GZU4..09.2014..Close))+stat_density()

pushViewport(viewport(layout = grid.layout(4, 2)))
print(ggSpread+
          geom_hline(aes(yintercept=0),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivClose[1])),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivFix[1])),linetype="dashed")+
          annotate("text",x=DateDivClose[1],y=0.5,label="Dividends Close",  size=4, angle=90, vjust = -0.5)+
          annotate("text",x=DateDivFix[1],y=0.5,label="Dividends Fix",  size=4, angle=90, vjust = -0.5)+
          ylab("GAZPROM FUT/STOCK Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("GAZPROM FUT/STOCK Spread, %"), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= SRU4..09.2014..Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(SRU4..09.2014..Close))+stat_density()
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivClose[2])),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivFix[2])),linetype="dashed")+
          annotate("text",x=DateDivClose[2],y=0,label="Dividends Close",  size=4, angle=90, vjust = -0.5)+
          annotate("text",x=DateDivFix[2],y=0,label="Dividends Fix",  size=4, angle=90, vjust = -0.5)+
          ylab("SBERBANK FUT/STOCK Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("SBERBANK FUT/STOCK Spread, %"), 
      vp = viewport(layout.pos.row = 2, layout.pos.col = 2))


ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= VBU4..09.2014..Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(VBU4..09.2014..Close))+stat_density()
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivClose[3])),linetype="dashed", label="Dividends Close")+
          geom_vline(aes(xintercept=as.numeric(DateDivFix[3])),linetype="dashed",label="Dividends Fix")+
          annotate("text",x=DateDivClose[3],y=0,label="Dividends Close",  size=4, angle=90, vjust = -0.5)+
          annotate("text",x=DateDivFix[3],y=0,label="Dividends Fix",  size=4, angle=90, vjust = -0.5)+
          ylab("VTB FUT/STOCK Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("VTB FUT/STOCK Spread, %"), 
      vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

ggSpread<-ggplot(ArbSpread.df,
                 aes(x=datetime, y= LKU4..09.2014..Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(LKU4..09.2014..Close))+stat_density()
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivClose[4])),linetype="dashed")+
          geom_vline(aes(xintercept=as.numeric(DateDivFix[4])),linetype="dashed")+
          annotate("text",x=DateDivClose[1],y=-0.2,label="Dividends Close",  size=4, angle=90, vjust = -0.5)+
          annotate("text",x=DateDivFix[1],y=-0.2,label="Dividends Fix",  size=4, angle=90, vjust = -0.5)+
          ylab("LUKOIL FUT/STOCK Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("LUKOIL FUT/STOCK Spread, %"), 
      vp = viewport(layout.pos.row = 4, layout.pos.col = 2))

write.csv(summary(ArbSpread),"fut.csv")

