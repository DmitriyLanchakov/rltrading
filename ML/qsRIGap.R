library(rusquant)
library(quantstrat)
library(data.table)
#source('F:/TRADE/R/trading/sandBox/rltrading/ML/sigAND.R')
#source('F:/TRADE/R/trading/sandBox/rltrading/ML/sigTimestamp.R')
#environment(sigAND)<- environment(sigCrossover)
#environment(sigTimestamp)<- environment(sigCrossover)

options(digits.secs=0)
######### RI 1min Gap strategy ##############
#' Гипотеза: после открытия сессии на RI (утром) заходить в сторону импульса (если он есть).
#' Стратегия: если 1-я минута > 500 пунктов, встать в ту же сторону на 15 минут.
#' Для иллюстрации, псевдо-код, работающий по открытиям свечей:
#' delta = (Close[1] - Open[1])/Point_Size;
#' if (Time==1001 && delta > 500) Buy (15);
#' if (Time==1001 && delta < -500) Sell (15);
#' 


############################# Preparing DATA ######################################
Sys.setenv(TZ="UTC")
period="1min"
symbol<-c("Ri")
setwd("~/repos/rltrading/ML")

assign(symbol, fread("Ri.txt", stringsAsFactors = FALSE))
get(symbol)[,Time:=as.character(Time)]
get(symbol)[,delay:=get(symbol)[,.(paste(Date,Time),
                                   delay=as.ITime(Time[1],format="%H%M")-as.ITime("1001",format="%H%M")),by=Date]$delay]
assign(symbol,get(symbol)[as.numeric(delay)<60*60])
get(symbol)[,DateTime:=paste(Date, Time)]
get(symbol)[,DateTime:=as.POSIXct(strptime(DateTime, format="%m/%d/%y %H%M"))]
get(symbol)[,DateTime:=DateTime-as.numeric(delay)]
setkey(get(symbol), DateTime)

assign(symbol,
       xts(get(symbol)[,.SD, 
                       .SDcols=c("Open", "High", "Low", "Close", "Volume")], 
           order.by = get(symbol)$DateTime))

assign(symbol,get(symbol)["T10:00/T10:35"])
assign(symbol,get(symbol)["2006/"])


# ##########################Data from MFD #############################
# from<-as.Date("2015-12-16")
# to<-Sys.Date()
# period="1min"
# symbols<-c("SiH6 (03.2016)",
#            "RIH6 (03.2016)")
# 
# from<-as.Date("2015-09-16")
# to<-as.Date("2015-12-15")#Sys.Date()
# period="1min"
# symbols<-c("SiZ5 (12.2015)",
#            "RIZ5 (12.2015)")
# 
# for(s in symbols)
#     getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
# symbols<-toupper(symbols)
# 
# symbol<-symbols[2]
# assign(symbol,get(symbol)["T10:00/T10:35"])

############################# DEFINE QUANTSTRAT VARIABLES ##############################
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()


port          = 'riPort'
acct          = 'riAcct'
strat         = "riStrat"
initEq        = 100000
initDate      = index(get(symbol)[1])
deltaPeriod   = 2  # in minutes  - period to measure delta Delt(Open, Close, k = deltaPeriod)
openLag       = 3  # in minutes / 59 max - how long we wait from Market Opening (10:00) open open postion
openLag       = max(deltaPeriod, openLag)
holdPostion   = 27 # in minutes / 59 max - how long we from Market Opening (10:00) to close position.
deltaThreshold= 0.005 # = (Close-Open)/Open

StopLossFlag  = FALSE
stopLossPercent= 0.25/100
StopTrailingFlag = FALSE
takeProfitPercent = 0.5/100
takeProfitFlag=FALSE
brokerFee     = -2

maxPosition   =1
minPosition   =-1

############################# INITIALIZE ####################################
#Remove account, strategy and portfolio if run previously
rm(list=ls(.strategy), pos=.strategy)
rm(list=ls(.blotter), pos=.blotter)
rm_instruments( keep.currencies = FALSE)
cur<-currency('RUR')

stock(symbol,currency=cur, multiplier=1)

initPortf(port, symbol, initDate=initDate, currency=cur)
initAcct(acct, port, initEq=initEq, initDate=initDate, currency=cur)
initOrders(port, initDate=initDate )
strategy(strat, store=TRUE)
############################# MAX POSITION LOGIC ############################
    addPosLimit(
        portfolio=port,
        symbol=symbol, 
        timestamp=initDate,  
        maxpos=maxPosition,
        minpos=minPosition)
############################# INDICATORS ####################################

# OpenCloseDelta
add.indicator(strat, name='Delt', label='Delt', 
              arguments = list(x1=quote(Op(mktdata)),
                               x2=quote(Cl(mktdata)),
                               k=deltaPeriod),
              storefun=FALSE)
############################# SIGNALS #######################################
add.signal(strat, 
           name='sigThreshold', 
           label= 'dltbuy', 
           arguments = list(column='Delt', 
                            threshold=deltaThreshold, 
                            relationship='gt'),
           storefun=FALSE)

add.signal(strat, 
           name='sigThreshold', 
           label= 'dltsell', 
           arguments = list(column='Delt', 
                            threshold= -deltaThreshold, 
                            relationship='lt'),
           storefun=FALSE)

add.signal(strat,
           name='sigTimestamp',
           label='ttopen',
           arguments=list(timestamp=paste(10, formatC(openLag,width=2, flag="0"), sep=":")),
           storefun=FALSE)

add.signal(strat,
           name='sigAND',
           label='buyopen',
           arguments=list(columns=c("dltbuy", "ttopen")),
           storefun=FALSE)

add.signal(strat,
           name='sigAND',
           label='sellopen',
           arguments=list(columns=c("dltsell", "ttopen")),
           storefun=FALSE)

add.signal(strat,
           name='sigTimestamp',
           label='ttclose',
           arguments=list(timestamp=paste(10, formatC(openLag+holdPostion,width=2, flag="0"), sep=":")),
           storefun=FALSE)

########################## RULES ############################################
######################## LONG RULES #########################################
#Enter Rule
add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='enter', 
         label='EnterLONG',
         arguments=list(sigcol= 'buyopen', 
                        sigval= TRUE, 
                        orderqty=1, 
                        ordertype = 'market', 
                        orderside='long',
                        TxnFees=brokerFee,
                        orderset='ocolong',
                        osFUN= 'osMaxPos'),
         storefun=FALSE)

#Exit Rule
add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='exit', 
         label='ExitLONG',
         arguments=list(sigcol='ttclose', 
                        sigval=TRUE, 
                        orderqty ='all', 
                        ordertype='market',
                        TxnFees=brokerFee,
                        orderset='ocolong',
                        orderside='long'),
         storefun=FALSE)

######################## SHORT RULES #########################################
# Enter Rule
add.rule(strategy  = strat,
         name      = 'ruleSignal',
         arguments = list(sigcol     = 'sellopen',
                          sigval    = TRUE,
                          orderqty  =  -1,
                          ordertype = 'market',
                          orderside = 'short',
                          TxnFees=brokerFee,
                          orderset='ocoshort',
                          osFUN     = 'osMaxPos'),
         type      = 'enter',
         label     = 'EnterSHORT',
         storefun=FALSE)

#Exit Rule
add.rule(
    strategy  = strat,
    name      = 'ruleSignal',
    arguments = list(sigcol     = 'ttclose',
                     sigval     = TRUE,
                     orderqty   = 'all',
                     ordertype  = 'market',
                     TxnFees=brokerFee,
                     orderset='ocoshort',
                     orderside  = 'short'),
    type      = 'exit',
    label     = 'ExitSHORT',
    storefun=FALSE)

############################# APPLY STRATEGY ################################

applyStrategy(strat, port, prefer='Close', debug=TRUE)

############################# UPDATE ########################################

updatePortf(port, symbol, Date=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(acct)

##################### CONTAINERS CALLED IN TESTING #####################
rets  = PortfReturns(acct)                                     #########
book  = getOrderBook(port)                                     #########
tStats = tradeStats(port)
dStats = dailyStats(port)                                  #########
########################################################################


############################# RESULTS ################################
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)], 2)
#Trades Statistics
print(data.frame(t(tStats[,-c(1,2)])))
#write.csv(data.frame(t(tStats[,-c(1,2)])),paste(fast, slow, sd,from, to,".csv", sep="_"))


#Daily Statistics
print(t(dStats))

# FORMAT THEME
theme<-chart_theme()
theme$col$up.col<-'#81F7BE'
theme$col$up.border<-'#81F7BE'
theme$col$dn.col<-'#FAAC58'
theme$col$dn.border<-'#FAAC58'


# Absolute Cumulutive P/L
chart.Posn(port, symbol,
           theme=theme)
           #Dates="2014-12-10/2014-12-10", 
           #TA=c("add_SMA(n=fast)","add_BBands(n=slow, sd=sd)"))

# Maximum Adverse Execursion
chart.ME(Portfolio=port,
         Symbol=symbol,
         type='MAE',
         scale='percent')

# Maximum Favourable Execursion
chart.ME(Portfolio=port,
         Symbol=symbol,
         type='MFE',
         scale='percent')

# Returns
charts.PerformanceSummary(PortfReturns(acct))

# Returns by year
pRet<-data.table(index(PortfReturns(acct)),PortfReturns(acct))
pRet[,V1:=as.POSIXct(V1)]
setnames(pRet, c("DateTime", "Return"))
qplot(x=Day, y=YearCumReturn,
      color=factor(year),
      geom=c("line", "point"),
      data=pRet[,.(YearCumReturn=cumsum(Return),Day=yday(DateTime)), by=year(DateTime)])


# PL by year
pPL<-data.table(index(getPortfolio(port)$symbols[[symbol]]$posPL$Net.Trading.PL),
                getPortfolio(port)$symbols[[symbol]]$posPL$Net.Trading.PL)
pPL[,V1:=as.POSIXct(V1)]

setnames(pPL, c("DateTime", "PL"))
pPL[,TradeFlag:=PL>0]
pPL[,cumPL:=cumsum(PL)]

qplot(x=DateTime, y=cumPL,
      geom=c("line"),
      data=pPL)

qplot(x=Day, y=YearCumPL,
      color=factor(year),
      geom=c("line"),
      data=pPL[,.(YearCumPL=cumsum(PL),Day=yday(DateTime)), by=year(DateTime)])

# NumTrades by year
pNT<-data.table(index(getPortfolio(port)$symbols[[symbol]]$posPL$Net.Trading.PL),
                getPortfolio(port)$symbols[[symbol]]$posPL$Net.Trading.PL)
pNT[,V1:=as.POSIXct(V1)]
setnames(pNT, c("DateTime", "PL"))
pNT[,TradeFlag:=PL>0]

qplot(x=Day, y=YearCumNumTrades,
      color=factor(year),
      geom=c("line"),
      data=pNT[,.(YearCumNumTrades=cumsum(TradeFlag),Day=yday(DateTime)), by=year(DateTime)])


Sys.time()-timerS

#Check overnight position exists
# timeLimit<-"23:50:00"
# datesLimit<-paste(seq(from, to, 1), timeLimit)
# lapply(datesLimit, function(x) getPos(port, symbol, x))
# 

############################# OPTIMIZATION ################################

timerS<-Sys.time()

deltaThreshold = seq(0.006, 0.006,by=0.001)
deltaPeriod = seq(2, 2,by=1)
openLag = seq(3, 3,by=1)
holdPostion = seq(25, 35,by=1)

#sd=seq(0.1,0.3, by=0.1)
#stopLossPercent = seq(0.01,0.01,by=0.001)
#takeProfitPercent = seq(0.01,0.05,by=0.01)

add.distribution(strat, 
                 paramset.label = 'POPT', 
                 component.type = 'indicator', 
                 component.label = 'Delt', 
                 variable = list( k= deltaPeriod), 
                 label = 'ddelt') 



add.distribution(strat, 
                 paramset.label = 'POPT', 
                 component.type = 'signal', 
                 component.label = 'dltbuy', 
                 variable = list( threshold= deltaThreshold), 
                 label = 'dbuy') 

add.distribution(strat, 
                 paramset.label = 'POPT', 
                 component.type = 'signal', 
                 component.label = 'dltsell', 
                 variable = list( threshold= -deltaThreshold), 
                 label = 'dsell') 


add.distribution(strat, 
                 paramset.label = 'POPT', 
                 component.type = 'signal', 
                 component.label = 'ttopen', 
                 variable = list( timestamp=paste(10, formatC(openLag,width=2, flag="0"), sep=":")), 
                 label = 'dtopen') 
# 
add.distribution(strat, 
                 paramset.label = 'POPT', 
                 component.type = 'signal', 
                 component.label = 'ttclose', 
                 variable = list( timestamp=paste(10, formatC(openLag+holdPostion,width=2, flag="0"), sep=":")),
                 label = 'dtclose')
# 
# add.distribution.constraint(strat, 
#                             paramset.label = 'POPT',
#                             distribution.label.1 = 'topen',
#                             distribution.label.2 = 'tclose',
#                             operator = '<',
#                             label = 'POPT'
#                             
#                             )

library(foreach) 
library(doParallel)
#library(parallel)
cores<-detectCores()

if( Sys.info()['sysname'] == "Windows" )
{

           cl <- makeCluster(cores)
           registerDoParallel(cl)
           #registerDoSEQ() # NO PARALLEL
          
              #library(doRedis)
    #registerDoRedis(queue = "jobs", host = "192.168.137.1",port = 6379)
    #startLocalWorkers(n=cores, queue="jobs", host = "192.168.137.1",port = 6379)
    
} else {
    library(doMC)
    registerDoMC(cores=cores)
}
#registerDoSEQ() # NO PARALLEL


results<-apply.paramset(strategy.st=strat, 
                        paramset.label='POPT', 
                        portfolio.st=port, 
                        account.st=acct,
                        verbose=TRUE) 

stopCluster(cl)

df<-data.table((results$tradeStats))
#df[order(df[,10]),c(1,2,3,4,5,8,9,10)]  
Sys.time()-timerS


df[order(-Net.Trading.PL)]



#qplot(x=Max.Drawdown, y=Net.Trading.PL, colour=Num.Trades,data=df, facets=.~Symbol)
# 
# df<-data.frame(t(results$tradeStats))
# for(s in symbols){
#     res<-results$tradeStats[results$tradeStats[,5]==s,]
#     res<-res[res[,"33"]==max(res[,"Profit.Factor"]),]
#     #Max End Eq
#     df<-cbind(df, t(res))
#     #print(t(res[res[,33]==max(res[,33])
#     #                         ,c(1,2,3,5,6,8,16,17,18,25,26,27,33)]))
#     #Min DrowDown
#     #print(t(res[res[,27]==max(res[,27])
#     #          ,c(1,2,3,5,6,8,16,17,18,25,26,27,33)]))
#     
# }
# 
# 
# 3D
require(rgl)
s<-symbol[1]
res<-results$tradeStats[results$tradeStats[,5]==s,]
df<-df[complete.cases(df)]
df[,topen:=as.numeric(gsub(":","",topen))]
df[,tclose:=as.numeric(gsub(":","",tclose))]

tradeGraphs (
    stats = df,
    free.params = c("topen", "tclose"),
    statistics = c("Net.Trading.PL","Max.Drawdown", "Avg.Trade.PL", "Num.Trades"),
    title = 'Ri GAP'
)







