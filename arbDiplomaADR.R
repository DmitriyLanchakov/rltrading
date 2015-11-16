#########################ADR/GDR ARBITRAGE СHECK####################################

library(rusquant)
library(ggplot2)
library(grid)
source('G:/BACKUP/GDRIVE/FinUniver/Diploma/Code/getsymbols.mfdru.R')

#Parameters
startDate<-as.Date("2014-05-01")
numdays <-100

#Loading ADR History
getSymbols.mfdru("GAZP London",
                 ,from=startDate,to=startDate+numdays, src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("SBER London",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("VTB London",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("ROSN London",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)

#Loading MOEX History
getSymbols.mfdru("ГАЗПРОМ ао",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("Сбербанк",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("ВТБ ао",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)
getSymbols.mfdru("Роснефть",
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)

#Stock/ADR Ratio
StADRRatio<-c(2,4,2000,1)

#LotNumbers
MOEXLot<-c(10,10,10000,10)

#Loading USD/RUB rates
getSymbols.mfdru(c("USDRUB_TOM")
                 ,from=startDate,to=startDate+numdays,src="mfd",period="1min",auto.assign=TRUE)

#Costing Pmoex=Padr x USD/RUB  + Spread + Z
# Arb = Pmoex- (Padr x USD/RUB  + Spread + Z)
MOEXSpread<-3.5*0.01/100
LSESpread<-10.9*0.01/100
Z<- 0.01/100

timeWindow <- 'T11:00:00/T18:30:00'

#USD
#USDRUB_TOM[timeWindow, 'USDRUB_TOM.Close']


LSEPosition<-cbind(`GAZP LONDON`[timeWindow, 'GAZP LONDON.Close']/StADRRatio[1]*MOEXLot[1]*USDRUB_TOM[timeWindow,'USDRUB_TOM.Close'],
                   `SBER LONDON`[timeWindow, 'SBER LONDON.Close']/StADRRatio[2]*MOEXLot[2]*USDRUB_TOM[timeWindow,'USDRUB_TOM.Close'],
                   `VTB LONDON`[timeWindow, 'VTB LONDON.Close']/StADRRatio[3]*MOEXLot[3]*USDRUB_TOM[timeWindow,'USDRUB_TOM.Close'],
                   `ROSN LONDON`[timeWindow, 'ROSN LONDON.Close']/StADRRatio[4]*MOEXLot[4]*USDRUB_TOM[timeWindow,'USDRUB_TOM.Close']
                    )


MOEXPosition<-cbind(`ГАЗПРОМ АО`[timeWindow, 'ГАЗПРОМ АО.Close']*MOEXLot[1],
                   `СБЕРБАНК`[timeWindow, 'СБЕРБАНК.Close']*MOEXLot[2],
                   `ВТБ АО`[timeWindow, 'ВТБ АО.Close']*MOEXLot[3],
                   `РОСНЕФТЬ`[timeWindow, 'РОСНЕФТЬ.Close']*MOEXLot[4])

ArbSpread<-log(LSEPosition*(1-LSESpread- Z))-log(MOEXPosition*(1-MOEXSpread- Z))
ArbSpread<-ArbSpread*100

ArbSpread.df<-data.frame(datetime=index(ArbSpread),ArbSpread, row.names=NULL)

ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= GAZP.LONDON.Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(GAZP.LONDON.Close))+stat_density()

pushViewport(viewport(layout = grid.layout(4, 2)))
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+ylab("GAZPROM LSE/MOEX Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("GAZPROM LSE/MOEX Spread, %"), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= SBER.LONDON.Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(SBER.LONDON.Close))+stat_density()
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+ylab("SBERBANK LSE/MOEX Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("SBERBANK LSE/MOEX Spread, %"), 
      vp = viewport(layout.pos.row = 2, layout.pos.col = 2))


ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= VTB.LONDON.Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(VTB.LONDON.Close))+stat_density()
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+ylab("VTB LSE/MOEX Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("VTB LSE/MOEX Spread, %"), 
      vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

ggSpread<-ggplot(ArbSpread.df, 
                 aes(x=datetime, y= ROSN.LONDON.Close))+stat_smooth()
ggDensity<-ggplot(ArbSpread.df, 
                  aes(ROSN.LONDON.Close))+stat_density()
print(ggSpread+geom_hline(aes(yintercept=0),linetype="dashed")+ylab("ROSNEFT LSE/MOEX Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("ROSNEFT LSE/MOEX Spread, %"), 
      vp = viewport(layout.pos.row = 4, layout.pos.col = 2))
#Write summary statistics
write.csv(summary(ArbSpread),"fut.csv")