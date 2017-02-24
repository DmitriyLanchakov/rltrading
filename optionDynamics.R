library(ggplot2)
library(data.table)
#library(stringr)
#library(tidyr)
#library(NMOF)
#library(DEoptim)
#library(fOptions)
#library(rusquant)
library(RSQLite)
library(manipulate)

path<-"/home/r0man/"
setwd(path)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")

# Fetch all results
rs <- dbSendQuery(mydb, "SELECT * FROM curopdesk")
resDT<-data.table(dbFetch(rs))
dbClearResult(rs)
dbDisconnect(mydb)

resDT[,LastTradeDate:=as.POSIXct(strptime(LastTradeDate, "%d.%m.%y %H:%M:%S"))]
resDT[,DateTime:=as.POSIXct(DateTime, origin = "1970-01-01")]

MinStrike<- -1
MaxStrike<- 1
StepStrike<-0.01
Symb<-"BR"

resDT[,DateTimeF:=format(DateTime,"%Y%m%d%H%M")]
resDT[,SymbExp:=factor(paste(strtrim(CODE,2), ExpDate))]
#grepl(Symb, CODE) &

symbFilter=levels(resDT$SymbExp)[5:6]
#ifelse(stikerAxis, Strike, log(Strike/StockPriceMid))

manipulate(ggplot(data=resDT[SymbExp %in% symbFilter & DateTimeF==resDT[,.N, by=DateTimeF][dt,DateTimeF]])+# & Strike>=MinStrike &Strike<=MaxStrike])+
               geom_line(aes(x=log(Strike/StockPriceMid),y=IV, colour=SymbExp), linetype = 2, show.legend = T)+
               geom_point(aes(x=log(Strike/StockPriceMid),y=GBSIVBidAsk*100,colour=SymbExp))+
               geom_smooth(aes(x=log(Strike/StockPriceMid),y=GBSIVBidAsk*100,colour=SymbExp))+
               ggtitle(paste(resDT[,.N, by=DateTimeF][dt,DateTimeF]))+
               scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StepStrike),limits=c(-0.1,0.10))+ 
               scale_y_continuous(breaks=seq(1,100,1), limits=c(10,40)),
           dt=slider(min = 1,max=resDT[,.N, by=DateTimeF][,.N],initial = resDT[,.N, by=DateTimeF][,.N]))
           
manipulate(ggplot(data=resDT[SymbExp %in% symbFilter & DateTimeF==resDT[,.N, by=DateTimeF][dt,DateTimeF]])+# & Strike>=MinStrike &Strike<=MaxStrike])+
               geom_line(aes(x=Strike,y=IV, colour=SymbExp), linetype = 2, show.legend = T)+
               geom_point(aes(x=Strike,y=GBSIVBidAsk*100,colour=SymbExp))+
               geom_smooth(aes(x=Strike,y=GBSIVBidAsk*100,colour=SymbExp))+
               ggtitle(paste(resDT[,.N, by=DateTimeF][dt,DateTimeF]))+
               scale_x_continuous(breaks=seq(55000,70000,500),limits=c(55000,70000))+ 
               scale_y_continuous(breaks=seq(1,100,1), limits=c(10,40)),
           dt=slider(min = 1,max=resDT[,.N, by=DateTimeF][,.N],initial = resDT[,.N, by=DateTimeF][,.N]))




library(animation)
oopt <- animation::ani.options(interval = 0.1)

# saveGIF(lapply(1:resDT[,.N, by=DateTimeF][,.N], 
#                FUN=function(dt){
#                    gg<-ggplot(data=resDT[ DateTimeF==resDT[,.N, by=DateTimeF][dt,DateTimeF]])+# & Strike>=MinStrike &Strike<=MaxStrike])+
#                        geom_line(aes(x=log(Strike/StockPriceMid),y=IV, colour=SymbExp),  linetype = 2, show.legend = T)+
#                        geom_point(aes(x=log(Strike/StockPriceMid),y=GBSIVBidAsk*100,colour=SymbExp))+
#                        geom_smooth(aes(x=log(Strike/StockPriceMid),y=GBSIVBidAsk*100,colour=SymbExp))+
#                        ggtitle(paste(resDT[,.N, by=DateTimeF][dt,DateTimeF]))+
#                        scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StepStrike),limits=c(-0.1,0.10))+ 
#                        scale_y_continuous(breaks=seq(1,100,1), limits=c(10,40))
#                    print(gg)}
#                ),
#         interval = 0.05,
#         movie.name = paste("SI-BR-RI","-VolSm.gif", sep=""), ani.width = 1000, ani.height = 600)
# 

saveVideo(lapply(1:resDT[,.N, by=DateTimeF][,.N], 
               FUN=function(dt){
                   gg<-ggplot(data=resDT[ DateTimeF==resDT[,.N, by=DateTimeF][dt,DateTimeF]])+# & Strike>=MinStrike &Strike<=MaxStrike])+
                       geom_line(aes(x=log(Strike/StockPriceMid),y=IV, colour=SymbExp),  linetype = 2, show.legend = T)+
                       geom_point(aes(x=log(Strike/StockPriceMid),y=GBSIVBidAsk*100,colour=SymbExp))+
                       geom_smooth(aes(x=log(Strike/StockPriceMid),y=GBSIVBidAsk*100,colour=SymbExp))+
                       ggtitle(paste(resDT[,.N, by=DateTimeF][dt,DateTimeF]))+
                       scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StepStrike),limits=c(-0.1,0.10))+ 
                       scale_y_continuous(breaks=seq(1,100,1), limits=c(10,40))
                   print(gg)}
), movie.name = paste("SI-BR-RI","-VolSm.mp4", sep=""), ani.width = 1000, ani.height = 600)


