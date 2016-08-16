######################### EVENT ARBITRAGE СHECK####################################

library(rusquant)
library(scales)
library(ggplot2)
library(grid)
source('G:/BACKUP/GDRIVE/FinUniver/Diploma/Code/getsymbols.mfdru.R')

#Parameters
startDate<-as.Date("2012-11-01")
numdays <-260

#Loading History
getSymbols(c("RSTI",
             "FEES"),
           from=startDate,to=startDate+numdays, src="Finam",period="5min",auto.assign=TRUE)
DateDivFix<-as.POSIXct(strptime(DateDivFix,"%d.%m.%Y %H:%M:%S"))

#Main events
DateEvent<-data.frame(dt=as.POSIXct(strptime(c("22.11.2012",
                                     "23.03.2013",
                                     "03.05.2013",
                                     "27.06.2013"),"%d.%m.%Y")),
            event=c("Указ президента о формаировании РОССЕТИ",
                    "Общее собрание акционеров",
                    "Внесение акций ФСК ЕэС в РОССЕТИ",
                    "Годовое собрание акционеров"),row.names=NULL)

data<-cbind(RSTI, FEES)
#Calculating Arb SPread
ArbSpread<-with(data,(log(RSTI.Close*1000)-log(FEES.Close*10000))*100)
data<-data.frame(datetime=index(ArbSpread),ArbSpread, row.names=NULL)
colnames(data)<-c("datetime", "ArbSpread")

#Showing resuts
ggSpread<-ggplot(data,aes(x=datetime, y= ArbSpread))
ggDensity<-ggplot(data,aes(ArbSpread))+stat_density()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggSpread+
          stat_smooth()+
          geom_hline(xintercept=0,linetype="dashed")+
          geom_vline(xintercept=as.numeric(DateEvent[1:4,"dt"]),linetype="dashed")+
          annotate("text",x=DateEvent[1:4,"dt"],y=0,label=DateEvent[1:4,"event"],  size=5, angle=90, vjust = -0.5)+
          ylab("RSTI/FEES Spread, %")+xlab(" "), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(ggDensity+ylab("Density")+xlab("RSTI/FEES Spread, %"), 
      vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

#Summary statistics
write.csv(summary(ArbSpread),"ma.csv")


