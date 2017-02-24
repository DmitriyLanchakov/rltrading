library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(NMOF)
library(DEoptim)
library(fOptions)
library(rusquant)
library(RSQLite)
######################################################################################
# Option Desk writer
######################################################################################

makeOptDesk<-function(symbol, expDate, period, symb){
    
    link<-paste("http://moex.com/ru/derivatives/optionsdesk-csv.aspx?code=",
                symbol,
                "&sid=1&c1=on&c2=on&c3=on&c4=on&c5=on&c6=on&c7=on&marg=1&delivery=",
                expDate, sep="")
    
    curOpDesk<-read.csv(link, sep=",",header=FALSE, stringsAsFactors=FALSE, skip=1)
    header<-c("CCODE",
              "CTotalValue",
              "CQtyLots",
              "CQtyTrades",
              "COI",
              "CMaxDay",
              "CMinDay",
              "CLastTradePrice",
              "CLastTradeDate",
              "CLastTradeDiff",
              "CBid",
              "CAsk",
              "CCalcPrice",
              "CTheoPrice",
              "Strike",
              "IV",
              "PTheoPrice",
              "PCalcPrice",
              "PBid",
              "PAsk",
              "PLastTradePrice",
              "PLastTradeDate",
              "PLastTradeDiff",
              "PMaxDay",
              "PMinDay",
              "PQtyTrades",
              "POI",
              "PTotalValue",
              "PQtyLots",
              "PCODE")
    curOpDesk<-curOpDesk[,1:30]
    colnames(curOpDesk)<-header
    curOpDesk<-data.table(curOpDesk, DateTime=as.POSIXct(Sys.time())-15*60)
    
    curOpDesk<-rbind(curOpDesk[,.SD, .SDcols=c("CCODE",
                                               "CTotalValue",
                                               "CQtyLots",
                                               "CQtyTrades",
                                               "COI",
                                               "CMaxDay",
                                               "CMinDay",
                                               "CLastTradePrice",
                                               "CLastTradeDate",
                                               "CLastTradeDiff",
                                               "CBid",
                                               "CAsk",
                                               "CCalcPrice",
                                               "CTheoPrice",
                                               "Strike",
                                               "IV",
                                               "DateTime")][,TypeFlag:="CA"],
                     curOpDesk[,.SD, .SDcols=c("PCODE",
                                               "PTotalValue",
                                               "PQtyLots",
                                               "PQtyTrades",
                                               "POI",
                                               "PMaxDay",
                                               "PMinDay",
                                               "PLastTradePrice",
                                               "PLastTradeDate",
                                               "PLastTradeDiff",
                                               "PBid",
                                               "PAsk",
                                               "PCalcPrice",
                                               "PTheoPrice",
                                               "Strike",
                                               "IV",
                                               "DateTime")][,TypeFlag:="PA"],
                     use.names=FALSE,fill=FALSE)
    
    setnames(curOpDesk,c("CODE",
                         "TotalValue",
                         "QtyLots",
                         "QtyTrades",
                         "OI",
                         "MaxDay",
                         "MinDay",
                         "LastTradePrice",
                         "LastTradeDate",
                         "LastTradeDiff",
                         "Bid",
                         "Ask",
                         "CalcPrice",
                         "TheoPrice",
                         "Strike",
                         "IV",
                         "DateTime",
                         "TypeFlag"
    ))
    #data("tickers")
    LastPriceSymb<-data.table(getSymbols(symb, from=curOpDesk$DateTime[1], period=period, src='mfd',adjust=TRUE, auto.assign=FALSE))[.N,]
    curOpDesk[,StockPriceMid:=LastPriceSymb[,(Open+High+Low+Close)/4]]
    curOpDesk[,c("PRICE", "tau"):=.((Bid+Ask)/2,
                                    as.numeric((as.POSIXct(expDate)-DateTime)/365))]
    curOpDesk[,c("PRICEBid", "tau"):=.(Bid,
                                       as.numeric((as.POSIXct(expDate)-DateTime)/365))]
    
    curOpDesk[,c("PRICEAsk", "tau"):=.(Ask,
                                       as.numeric((as.POSIXct(expDate)-DateTime)/365))]
    curOpDesk[,ExpDate:=expDate]
    
    curOpDesk[,id:=.I]
    curOpDesk<-curOpDesk[PRICE<TheoPrice*2 & PRICE>TheoPrice*0.5]
    curOpDesk<-curOpDesk[PRICEBid<TheoPrice*2 & PRICEBid>TheoPrice*0.5]
    curOpDesk<-curOpDesk[PRICEAsk<TheoPrice*2 & PRICEAsk>TheoPrice*0.5]
    
    curOpDesk[,GBSIVBidAsk:=GBSVolatility(price=PRICE, 
                                          TypeFlag = strtrim(tolower(TypeFlag),1), 
                                          S=StockPriceMid,
                                          X=Strike,
                                          Time=as.numeric(tau),
                                          r=0,
                                          b=0,
                                          maxiter=100), by=id]
    
    
    curOpDesk[,GBSIVBid:=GBSVolatility(price=PRICEBid, 
                                       TypeFlag = strtrim(tolower(TypeFlag),1), 
                                       S=StockPriceMid,
                                       X=Strike,
                                       Time=as.numeric(tau),
                                       r=0,
                                       b=0,
                                       maxiter=100), by=id]
    
    curOpDesk[,GBSIVAsk:=GBSVolatility(price=PRICEAsk, 
                                       TypeFlag = strtrim(tolower(TypeFlag),1), 
                                       S=StockPriceMid,
                                       X=Strike,
                                       Time=as.numeric(tau),
                                       r=0,
                                       b=0,
                                       maxiter=100), by=id]
    
    
    curOpDesk[,GBSIVCalc:=GBSVolatility(price=CalcPrice, 
                                        TypeFlag = strtrim(tolower(TypeFlag),1), 
                                        S=StockPriceMid,
                                        X=Strike,
                                        Time=as.numeric(tau),
                                        r=0,
                                        b=0,
                                        maxiter=100), by=id]
    curOpDesk[,GBSIVTheo:=GBSVolatility(price=TheoPrice, 
                                        TypeFlag = strtrim(tolower(TypeFlag),1), 
                                        S=StockPriceMid,
                                        X=Strike,
                                        Time=as.numeric(tau),
                                        r=0,
                                        b=0,
                                        maxiter=100), by=id]
    curOpDesk
    
}

symbol<-"Si-3.17"
expDate<-"2017-02-16"
period="1min"
symb<-"SiH7 (03.2017)"

curOpDesk<-makeOptDesk(symbol, expDate, period, symb)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")
dbWriteTable(mydb, "curOpDesk", curOpDesk,append=TRUE, row.names = FALSE)
dbDisconnect(mydb)


symbol<-"Si-3.17"
expDate<-"2017-03-16"
period="1min"
symb<-"SiH7 (03.2017)"

curOpDesk<-makeOptDesk(symbol, expDate, period, symb)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")
dbWriteTable(mydb, "curOpDesk", curOpDesk,append=TRUE, row.names = FALSE)
dbDisconnect(mydb)



symbol<-"BR-2.17"
expDate<-"2017-01-26"
period="1min"
symb<-"BRG7 (02.2017)"

curOpDesk<-makeOptDesk(symbol, expDate, period, symb)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")
dbWriteTable(mydb, "curOpDesk", curOpDesk,append=TRUE, row.names = FALSE)
dbDisconnect(mydb)


symbol<-"BR-3.17"
expDate<-"2017-02-22"
period="1min"
symb<-"BRH7 (03.2017)"

curOpDesk<-makeOptDesk(symbol, expDate, period, symb)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")
dbWriteTable(mydb, "curOpDesk", curOpDesk,append=TRUE, row.names = FALSE)
dbDisconnect(mydb)



symbol<-"RTS-3.17"
expDate<-"2017-03-16"
period="1min"
symb<-"RIH7 (03.2017)"

curOpDesk<-makeOptDesk(symbol, expDate, period, symb)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")
dbWriteTable(mydb, "curOpDesk", curOpDesk,append=TRUE, row.names = FALSE)
dbDisconnect(mydb)


symbol<-"RTS-3.17"
expDate<-"2017-02-16"
period="1min"
symb<-"RIH7 (03.2017)"

curOpDesk<-makeOptDesk(symbol, expDate, period, symb)
mydb <- dbConnect(RSQLite::SQLite(), "qtdb.sqlite")
dbWriteTable(mydb, "curOpDesk", curOpDesk,append=TRUE, row.names = FALSE)
dbDisconnect(mydb)

