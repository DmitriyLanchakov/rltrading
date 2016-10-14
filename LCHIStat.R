library(blotter)
library(rusquant)
library(data.table)
data("tickers")
options(scipen=10)
options(digits = 1)
if (!exists('.blotter')) .blotter <- new.env()
userPortf<-"user_port"
userAcc<-"user_acc"

hcchart.Posn<-function (Symbol, Dates = NULL,...) 
{
    pname <- userPortf
    Portfolio <- getPortfolio(pname)
    if (missing(Symbol)) 
        Symbol <- ls(Portfolio$symbols)[[2]] else Symbol <- Symbol[1]
    Prices =get(Symbol)
    if (!is.OHLC(Prices)) {
        if (hasArg(prefer)) 
            prefer = eval(match.call(expand.dots = TRUE)$prefer)
        else prefer = NULL
        Prices = getPrice(Prices, prefer = prefer)
    }
    freq = periodicity(Prices)
    switch(freq$scale, seconds = {
        mult = 1
    }, minute = {
        mult = 60
    }, hourly = {
        mult = 3600
    }, daily = {
        mult = 86400
    }, {
        mult = 86400
    })
    if (!isTRUE(freq$frequency * mult == round(freq$frequency, 
                                               0) * mult)) {
        n = round((freq$frequency/mult), 0) * mult
    }
    else {
        n = mult
    }
    tzero = xts(0, order.by = index(Prices[1, ]))
    if (is.null(Dates)) 
        Dates <- paste(first(index(Prices)), last(index(Prices)), 
                       sep = "::")
    Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
    Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
    Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
    Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades > 0)]
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 0)]
    Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
    if (nrow(Position) < 1) 
        stop("no transactions/positions to chart")
    if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position)))) 
        Position <- rbind(xts(0, order.by = first(index(Prices) - 
                                                      1)), Position)
    Positionfill = na.locf(merge(Position, index(Prices)))
    CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
    if (length(CumPL) > 1) 
        CumPL = na.omit(na.locf(merge(CumPL, index(Prices)))) else CumPL = NULL
    
    if (!is.null(CumPL)) {
        CumMax <- cummax(CumPL)
        Drawdown <- -(CumMax - CumPL)
        Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) - 1)), Drawdown)} else {  Drawdown <- NULL  }
    
    if (!is.null(Dates)) 
        Prices = Prices[Dates]
    
    # hightChat implementation
    
    library("highcharter")
    hc<-highchart() %>%
        hc_plotOptions(series=list(dataGrouping=list(enabled=FALSE))) %>%
        hc_title(text = paste(Symbol, "Buys:", nrow(Buys), " / Sells:",nrow(Sells)))%>%
        hc_yAxis_multiples(
            list(title = list(text = NULL), height = "60%", top = "0%"),
            list(title = list(text = NULL), height = "10%",top = "67%"),
            list(title = list(text = NULL), height = "20%",top = "80%")
        ) %>% 
        hc_add_series_ohlc(Prices, yAxis = 0, name=Symbol)
    
    if (nrow(Buys)!=0)
    hc<-hc %>% hc_add_series_xts(Buys,
                          color = "green",
                          type="scatter",
                          #marker = list(symbol = fa_icon_mark("arrow-up")),
                          marker = list(symbol = "triangle"),
                          yAxis = 0, 
                          name="Buy")
    
    if(nrow(Sells)!=0)
    hc<-hc %>%  hc_add_series_xts(Sells,  
                          color = "red",
                          type="scatter",
                          #marker = list(symbol = fa_icon_mark("arrow-down")), 
                          marker = list(symbol = "triangle-down"),
                          yAxis = 0, 
                          name="Sell")
    
    if(nrow(Positionfill[paste(min(index(Prices)),"::",sep="")])!=0)
        hc<-hc %>% hc_add_series_xts(Positionfill[paste(min(index(Prices)),"::",sep="")], 
                          #type="scatter",
                          type = "line", 
                          #type="column",
                          color = "blue",
                          yAxis=1,
                          #marker = list(symbol = "square"),
                          name="Poitionfill")
    # if(nrow(Position[paste(min(index(Prices)),"::",sep="")])!=0)
    #     hc<-hc %>% hc_add_series_xts(Position[paste(min(index(Prices)),"::",sep="")], 
    #                       #type = "scatter", 
    #                       color = "orange",
    #                       type = "line", 
    #                       yAxis=1, 
    #                       #marker = list(symbol = "square"),
    #                       name="Position")
    if(nrow(CumPL[paste(min(index(Prices)),"::",sep="")])!=0)
        hc<-hc %>%hc_add_series_xts(CumPL[paste(min(index(Prices)),"::",sep="")], 
                          color = "darkgreen", 
                          type = "line", 
                          yAxis=2,
                          name="CumPL")
    if(nrow(Drawdown[paste(min(index(Prices)),"::",sep="")])!=0)
        hc<-hc %>% hc_add_series_xts(Drawdown[paste(min(index(Prices)),"::",sep="")], 
                          color = "darkred", 
                          type = "line", 
                          yAxis=2, 
                          name="Drawdown")
    hc %>% 
        #hc_add_theme(hc_theme_flat())
        #hc_add_theme(hc_theme_smpl())
        #Cool hc_add_theme(hc_theme_538())
        hc_add_theme(hc_theme_gridlight())
} 

makePortfolio<-function(yearId, userId,marketId, nickname="unknown", amount=100000){

  #Remove account and portfolio if run previously
  #.blotter<-NULL

  try(rm(list=c(paste("portfolio",userPortf,sep="."),
                paste("account",userAcc,sep=".")),
         pos=.blotter), silent =FALSE)
  
  
  #download user data and trades data
  #userId<-"50175"
  dateId<-"all" # all - all trades, 20141208 - day trades
  #marketId<-2 # 1 - spot, 2 - deriv
  ProfileLink<-"http://investor.moex.com/ru/statistics/2015/portfolio.aspx?traderId="
  ProfileLink<-paste(ProfileLink, userId, sep="")
  TradesLink<-"ftp://ftp.moex.com/pub/info/stats_contest"
  TradesLink<-paste(TradesLink, yearId,dateId,
                    paste(marketId,"_", userId,".zip", sep=""),
                    sep="/")
  download.file(TradesLink, paste(marketId,"_", userId,".zip", sep=""))
  unzip(paste(marketId,"_", userId,".zip", sep=""))
  
  #Read trades data
  userData<-read.csv(paste(marketId,"_", userId,".csv", sep=""),sep=";", header=FALSE)
  
  #Removing temporary files
  file.remove(paste(marketId,"_", userId,".zip", sep=""))
  file.remove(paste(marketId,"_", userId,".csv", sep=""))
  
  
  #Processing data and declare symbol
  #userSymbol<-"SiZ4"
  userSymbols<-levels(factor(userData$V2))
  #Load historical data for the symbol
  #symbol<-"SiZ4 (12.2014)"
  data("tickers")
  #tickers<-loadStockListMfd()
  
  #MOEXSymbols<-loadStockListMoex()
  #MOEXSymbols<-data.table(MOEXSymbols, stringsAsFactors=FALSE)
  
  
  if (marketId==2) 
    symbols<-unlist(sapply(paste(userSymbols, " ", sep=""), 
                           searchSymbol, USE.NAMES=FALSE))
  #else 
  #symbols<-as.character(MOEXSymbols[shortSymbol %in% gsub(" ","",userSymbols)][,shortName])
  
  from<-as.Date(userData[1,1])
  to<-Sys.Date()
  period="1min"
  
  for(s in symbols){
      getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
  }
    
  
  #Initialize stocks
  currency("RUB")
  #symbol<-toupper(symbol)
  symbols<-toupper(symbols)
  for(s in symbols){
      assign(s, get(s),envir=globalenv())
  }
  assign("symbols", symbols,envir=globalenv())
  
  symbols.df<-data.frame(symbols, userSymbols)
  symbol<-symbols[1]
  
  #stock(symbol,currency="RUB",multiplier=1)
  stock(symbols,currency="RUB",multiplier=1)
  
  
  # Initialize the Portfolio
  initDate<-"2010-01-14"
  initEq<-amount
  #initPortf(userPortf,symbols=symbol,initDate=initDate)
  initPortf(userPortf,symbols=symbols,initDate=initDate)
  
  initAcct(userAcc,portfolios=userPortf,initDate=initDate, initEq=initEq)
  
  # look at the transactions data
  #symbol.trades
  
  # Add the transactions to the portfolio
  for(s in symbols){
    us<-as.character(symbols.df[symbols.df[,1]==s,2])
    symbol.trades<-userData[userData$V2==us,]
    symbol.trades<-xts(cbind(symbol.trades$V4,symbol.trades$V3),
                       order.by=as.POSIXct(symbol.trades[,1]))
    colnames(symbol.trades)<-c("TxnPrice","TxnQty")
    blotter:::addTxns(userPortf,s,
                      TxnData=symbol.trades,verbose=FALSE)
    
  }
  
  # update the portfolio stats
  updatePortf(userPortf)
  
  # update the account P&L
  updateAcct(userAcc)
  
  # and look at it
  portfolio = getPortfolio(userPortf)
  account = getAccount(userAcc)
  
 
  #lapply(symbols, FUN=function(s) chart.Posn(userPortf, s, theme=theme))
  tStats <- tradeStats(Portfolios = userPortf, use="trades", inclZeroDays=FALSE)
  #tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
  print(data.frame(t(tStats[,-c(1,2, 23)])))
  data.table(nickname=nickname, userId=userId, marketdId=marketdId, symbol=rownames(tStats),tStats[,-c(1,2)])
  
}

# 2016
# 83841 / 2 - robot_v5
# 83836 / 2 - robot_kingfees
# 83013 / 1 - Shadrin
#userData<-data.table(userData)
#userData[,Amount:=V3*V4]
#userData[,.(sum(V3), .N), by=V2]


# Make stat table
dayResDT<-read.csv("ftp://ftp.moex.com/pub/info/stats_contest/2016/result_day.csv", stringsAsFactors = F, sep=";", 
                   header=T, fileEncoding = "CP1251")
dayResDT<-data.table(dayResDT)
dayResDT[, marketId:=ifelse(contype_name=="Срочный", 2, ifelse(contype_name=="Фондовый", 1,3))]
dayResDT[count_deal>0 & marketId==2][order(-dohod)]

statDT<-dayResDT[count_deal>0 & marketId==2][order(-dohod)][1:100]


timerStart<-Sys.time()
resDT<-rbindlist(lapply(1:statDT[,.N], FUN = function(x) makePortfolio(2016, statDT[x,trader_id], statDT[x,marketId], statDT[x,nik], statDT[x,amount] )))
print(Sys.time()-timerStart)

cols<-c("nickname",
        "symbol",
        "Num.Txns",
        "Num.Trades",
        "Net.Trading.PL",     
        #"Avg.Trade.PL",
        #"Med.Trade.PL",
        #"Largest.Winner",    
        #"Largest.Loser",
        #"Gross.Profits",
        #"Gross.Losses",
        #"Std.Dev.Trade.PL",
        "Percent.Positive",
        "Percent.Negative",
        "Profit.Factor",
        #"Avg.Win.Trade",
        #"Med.Win.Trade",
        #"Avg.Losing.Trade"  ,
        #"Med.Losing.Trade",
        "Max.Drawdown",
        #"Profit.To.Max.Draw" ,
        "Max.Equity"        ,
        "Min.Equity",
        "End.Equity")

r<-resDT[,.SD, .SDcols=cols] [order(-Profit.Factor)] 



x=1


makePortfolio(2016, statDT[x,trader_id], statDT[x,marketId], statDT[x,nik], statDT[x,amount] )
hcchart.Posn(symbols[7])


library(jsonlite)
library(httr)
getInitPos<-function(traderId=83225, startDate="2016-09-16"){
    resp<-POST(url = "investor.moex.com/ru/statistics/2016/portfolio.aspx/GetPortfolioData", 
               encode = "json", 
               body = list('traderId'=paste0(traderId),'date'=paste0(startDate),'tableId'=6 ))
    stop_for_status(resp)
    #str(resp$headers)
    json<-content(resp, "text")
    validate(json)
    
    initPos <- fromJSON(fromJSON(txt = json)$d)
    initPos$start_pos<-getStartPos(initPos$pos)
    initPos
}

getStartPos<-function(ipos){
    sapply(ipos, FUN=function(x){
        res<-as.numeric(strsplit(gsub(")","",gsub(" ","",x)),split="(", fixed=T)[[1]])
        res[is.na(res)]<-0
        res[1]-res[2]
    })
}

getQuotesCount<-function(traderId=83225, onDate="2016-09-16", marketId=0){
    resp<-POST(url = "investor.moex.com/ru/statistics/2016/portfolio.aspx/GetPortfolioData", 
               encode = "json", 
               body = list('traderId'=paste0(traderId),'date'=paste0(onDate),'tableId'=2 ))
    stop_for_status(resp)
    #str(resp$headers)
    json<-content(resp, "text")
    validate(json)
    
    content.df <- fromJSON(fromJSON(txt = json)$d)
    if(!is.null(dim(content.df)))
                content.df[marketId+1,]$quotes
    else 0.0
                

}



res<-getInitPos(statDT[1,trader_id],paste(strptime(statDT[1,date_start], format = "%d.%m.%Y")))



statDT<-dayResDT[count_deal>0 & marketId==2][order(-count_deal)][1:100]
statDT[,id:=1:.N]

statDT[,qt:=as.numeric(getQuotesCount(traderId =trader_id, onDate=Sys.Date())), by=id]

x=2

getInitPos(statDT[order(-qt)][x,trader_id], paste(strptime(statDT[order(-qt)][x,date_start], format = "%d.%m.%Y")))
makePortfolio(2016, statDT[order(-qt)][x,trader_id], statDT[order(-qt)][x,marketId], statDT[order(-qt)][x,nik], statDT[order(-qt)][x,amount] )
hcchart.Posn(symbols[3])


# chart.ME(
#     Portfolio=userPortf,
#     Symbol=symbol,
#     type='MAE',
#     scale='percent'
# )
# 
# chart.ME(
#     Portfolio=userPortf,
#     Symbol=symbol
#     type='MFE',
#     scale='percent'
# )
# 
# 
# #trade statistics

 # 
# #daily statistics
# dStats<-dailyStats(Portfolios = userPortf, use="trades")
# print(data.frame(t(dStats)))
# 
# # Returns (check initial equity first)
# charts.PerformanceSummary(PortfReturns(userAcc))
# portRet<-PortfReturns(Account=userAcc, period="daily")
# print(data.frame(t(portRet)))
 
