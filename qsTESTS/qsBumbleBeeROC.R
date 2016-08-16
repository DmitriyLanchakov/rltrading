library(rusquant)
library(quantstrat)
source('sigAND.R', echo=TRUE)
source('sigTimeStampSession.R', echo=TRUE)
source('customsindicators.R', echo=TRUE)

# Bumblebee trading system
# copyright (c) 2009-2013, Algorithm Alpha, LLC
# RUSSIAN MARKET INTRADAY
# WITH STOP LOSS

timerS<-Sys.time()
############################# GET DATA ######################################
from<-as.Date("2015-04-01")
to<-as.Date("2015-05-29") #Sys.Date()
period="1min"
symbols<-c("SiM5 (06.2015)",
            "SRM5 (06.2015)",
            "GZM5 (06.2015)",
            "RIM5 (06.2015)")

# 
#For stock names in Russian
#Sys.setlocale(category = "LC_ALL", locale = "Russian")
#MAC options(encoding="windows-1251")
#MAC options(encoding="utf-8")
if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()
#if(!exists(toupper(symbols[1])))
    for(s in symbols)
        getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)
symbols<-toupper(symbols)

#Time frame filer to reduce market opening and closing gap
# for(s in symbols)
#     assign(s,get(s)["T10:05/T23:50"])

symbol<-symbols[1]
############################# DEFINE VARIABLES ##############################
Sys.setenv(TZ="UTC")
port          = 'bumblebeePort'
acct          = 'bumblebeeAcct'
strat         = "bumblebee"
initEq        = 100000
initDate      = '1969-12-31'
fast          = 10
slow          = 30
sd            = 0.1
.nROC         = 1
thresholdROC  = 0
startTime     ="10:05"
endTime       ="18:15"
intraDay      = TRUE
StopLossFlag  = TRUE
stopLossPercent= 0.25/100
StopTrailingFlag = FALSE
takeProfitPercent = 0.5/100
takeProfitFlag=TRUE
brokerFee = -2


############################# INITIALIZE ####################################
#Remove account, strategy and portfolio if run previously
rm(list=ls(.strategy), pos=.strategy)
rm(list=ls(.blotter), pos=.blotter)
rm_instruments( keep.currencies = FALSE)
# try(rm(list=c(paste("portfolio",port,sep="."),
#               paste("account",acct,sep=".")),
#        pos=.blotter), silent =FALSE)
# try(rm(list=c(strat,
#               paste("order_book",strat,sep="."),
#               paste("order_book",port,sep=".")),
#        pos=.strategy), silent =FALSE)
#nuke()
cur<-currency('RUR')
for(s in symbols)
    stock(s,currency=cur, multiplier=1)

initPortf(port, symbols, initDate=initDate, currency=cur)
initAcct(acct, port, initEq=initEq, initDate=initDate, currency=cur)
initOrders(port, initDate=initDate )
strategy(strat, store=TRUE)

############################# MAX POSITION LOGIC ############################

for(s in symbols)
    addPosLimit(
        portfolio=port,
        symbol=s, 
        timestamp=initDate,  
        maxpos=1,
        minpos=0)


############################# INDICATORS ####################################

# Bolinger Bands  is like bumblebee body
add.indicator(strat, 
              name='BBands', label='bb',
              arguments = list(HLC=quote(HLC(mktdata)), n=slow, sd=sd, maType='SMA'),
              storefun=TRUE)

# SMA is is like bumblebee wings
add.indicator(strat, name='MySMA', label='fast', 
              arguments = list(x=quote(Cl(mktdata)), n=fast),
              storefun=TRUE)

# ROC
add.indicator(strat, name='ROConSMA', label='angle', 
              arguments = list(x=quote(Cl(mktdata)), nSMA=fast, nROC=.nROC),
              storefun=TRUE)


############################# SIGNALS #######################################
add.signal(strat, 
           name='sigCrossover', 
           label= 'fld', 
           arguments = list(columns=c('SMA','dn'), relationship='lt'),
           storefun=TRUE)

#NEW
add.signal(strat, 
           name='sigCrossover', 
           label= 'flu', 
           arguments = list(columns=c('SMA','up'), relationship='lt'),
           storefun=TRUE)

add.signal(strat, 
           name='sigCrossover', 
           label='fgu', 
           arguments = list(columns=c('SMA','up'), relationship='gt'),
           storefun=TRUE)

#NEW
add.signal(strat, 
           name='sigCrossover', 
           label='fgd', 
           arguments = list(columns=c('SMA','dn'), relationship='gt'),
           storefun=TRUE)


add.signal(strategy = strat, 
           name="sigThreshold",
           arguments = list(threshold=thresholdROC, column="ROC", relationship="gt"),
           label="rgt0")


add.signal(strategy = strat, 
           name="sigThreshold",
           arguments = list(threshold=-thresholdROC, column="ROC", relationship="lt"),
           label="rlt0")



add.signal(strat,
           name='sigTimeStampSession',
           label='worksession',
           arguments=list(startTimeStamp=startTime,endTimeStamp=endTime ))

add.signal(strat, name="sigAND",
           arguments=list(columns=c("fgu", 
                                    "rgt0",
                                    "worksession")),
           label="longEntry")


add.signal(strat, name="sigAND",
           arguments=list(columns=c("fld", 
                                    "rlt0",
                                    "worksession")),
           label="shortEntry")
           
############################# RULES #########################################

######################## LONG RULES #########################################
#Enter Rule
add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='enter', 
         label='EnterLONG',
         path.dep=TRUE,
         arguments=list(sigcol= 'longEntry', 
                        sigval= TRUE, 
                        orderqty=100, 
                        ordertype = 'market', 
                        orderside='long',
                        TxnFees=brokerFee,
                        orderset='ocolong',
                        osFUN= 'osMaxPos'))




#Exit Rule
add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='exit', 
         label='ExitLONG',
         path.dep=TRUE,
         arguments=list(sigcol='rlt0', 
                        sigval=TRUE, 
                        orderqty ='all', 
                        ordertype='market',
                        TxnFees=brokerFee,
                        orderset='ocolong',
                        orderside='long'))

# Take Profit
add.rule(strategy  =strat, 
         name = 'ruleSignal',
         arguments=list(sigcol='longEntry' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='long',
                        TxnFees=brokerFee,
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=quote(takeProfitPercent),
                        orderqty='all',
                        orderset='ocolong'),
         type='chain', 
         parent='EnterLONG',
         label='TakeProfitLONG',
         enabled=takeProfitFlag)

# Stop Loss Rule
add.rule(strategy =strat,
         name="ruleSignal",
         type="chain", parent="EnterLONG",
         label="StopLossLONG",
         path.dep=TRUE,
         arguments = list(sigcol="longEntry", 
                          sigval=TRUE,
                          replace=FALSE,
                          orderqty="all",
                          orderside="long",
                          TxnFees=brokerFee,
                          ordertype="stoplimit",
                          orderset='ocolong',
                          tmult=TRUE,
                          threshold=quote(stopLossPercent)),
         enabled=StopLossFlag)

# StopTrailing Rule
add.rule(strategy  =strat,
         name="ruleSignal",
         type="chain", parent="EnterLONG",
         label="StopTrailingLONG",
         path.dep=TRUE,
         arguments = list(sigcol="longEntry", 
                          sigval=TRUE,
                          replace=FALSE,
                          orderqty="all",
                          orderside="long",
                          TxnFees=brokerFee,
                          ordertype="stoptrailing",
                          orderset='ocolong',
                          tmult=TRUE,
                          threshold=quote(stopLossPercent)),
         enabled=StopTrailingFlag)

######################## SHORT RULES #########################################
# Enter Rule
add.rule(strategy  = strat,
         name      = 'ruleSignal',
         arguments = list(sigcol     = 'shortEntry',
                          sigval    = TRUE,
                          orderqty  =  -100,
                          ordertype = 'market',
                          orderside = 'short',
                          TxnFees=brokerFee,
                          orderset='ocoshort',
                          osFUN     = 'osMaxPos'),
         type      = 'enter',
         label     = 'EnterSHORT')

#Exit Rule
add.rule(
    strategy  = strat,
    name      = 'ruleSignal',
    arguments = list(sigcol     = 'rgt0',
                     sigval     = TRUE,
                     orderqty   = 'all',
                     ordertype  = 'market',
                     TxnFees=brokerFee,
                     orderset='ocoshort',
                     orderside  = 'short'),
    type      = 'exit',
    label     = 'ExitSHORT')

# Take Profit
add.rule(strategy  =strat, 
         name = 'ruleSignal',
         arguments=list(sigcol='shortEntry' , 
                        sigval=TRUE,
                        replace=FALSE,
                        orderside='short',
                        TxnFees=brokerFee,
                        ordertype='limit', 
                        tmult=TRUE, 
                        threshold=quote(takeProfitPercent),
                        orderqty='all',
                        orderset='ocoshort'),
         type='chain', 
         parent='EnterSHORT',
         label='TakeProfitSHORT',
         enabled=takeProfitFlag)


# Stop Loss Rule
add.rule(strategy  =strat,
         name="ruleSignal",
         type="chain", parent="EnterSHORT",
         label="StopLossSHORT",
         path.dep=TRUE,
         arguments = list(sigcol="shortEntry", 
                          sigval=TRUE,
                          replace=FALSE,
                          orderqty="all",
                          TxnFees=brokerFee,
                          orderside="short",
                          orderset='ocoshort',
                          ordertype="stoplimit",
                          tmult=TRUE,
                         threshold=quote(stopLossPercent)),
         enabled=StopLossFlag)

# StopTrailing Rule
add.rule(strategy  =strat,
         name="ruleSignal",
         type="chain", parent="EnterSHORT",
         label="StopTrailingSHORT",
         path.dep=TRUE,
         arguments = list(sigcol="shortEntry", 
                          sigval=TRUE,
                          replace=FALSE,
                          orderqty="all",
                          orderside="short",
                          TxnFees=brokerFee,
                          ordertype="stoptrailing",
                          orderset='ocoshort',
                          tmult=TRUE,
                          threshold=quote(stopLossPercent)),
         enabled=StopTrailingFlag)

######################## NO OVERNIGHT RULES #########################################
add.rule(
    strategy  = strat,
    name      = 'ruleSignal',
    arguments = list(sigcol     = 'worksession',
                     sigval     = FALSE,
                     orderqty   = 'all',
                     TxnFees=brokerFee,
                     ordertype  = 'market'),
    type      = 'exit',
    label     = 'CloseOpenPos',
    enabled   = intraDay)

############################# APPLY STRATEGY ################################

applyStrategy(strat, port, prefer='Close', verbose=FALSE)

############################# UPDATE ########################################

updatePortf(port, symbols, Date=paste('::',as.Date(Sys.time()),sep=''))
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
           theme=theme,
           #Dates="2014-12-10/2014-12-10", 
           TA=c("add_SMA(n=fast)","add_BBands(n=slow, sd=sd)"))

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


Sys.time()-timerS

#Check overnight position exists
# timeLimit<-"23:50:00"
# datesLimit<-paste(seq(from, to, 1), timeLimit)
# lapply(datesLimit, function(x) getPos(port, symbol, x))
# 

############################# OPTIMIZATION ################################

timerS<-Sys.time()

fast = 5:10
slow = 30
sd=0.1
#sd=seq(0.1,0.3, by=0.1)
#stopLossPercent = seq(0.01,0.01,by=0.001)
#takeProfitPercent = seq(0.01,0.05,by=0.01)

add.distribution(strat, 
                 paramset.label = 'BBOPT', 
                 component.type = 'indicator', 
                 component.label = 'fast', 
                 variable = list(n = fast), 
                 label = 'fast') 

add.distribution(strat, 
                 paramset.label = 'BBOPT', 
                 component.type = 'indicator', 
                 component.label = 'bb', 
                 variable = list(n = slow), 
                 label = 'slow') 
 
add.distribution(strat, 
                 paramset.label = 'BBOPT', 
                 component.type = 'indicator', 
                 component.label = 'bb', 
                 variable = list(sd = sd), 
                 label = 'sd') 
# 
# # Stop Loss
# add.distribution(strat,
#                  paramset.label = "BBOPT",
#                  component.type = "chain",
#                  component.label = "StopLossLONG",
#                  variable = list( threshold = stopLossPercent),
#                  label = "StopLossLong")
# 
# add.distribution(strat,
#                  paramset.label = "BBOPT",
#                  component.type = "chain",
#                  component.label = "StopLossSHORT",
#                  variable = list( threshold = stopLossPercent),
#                  label = "StopLossShort")
# 
# add.distribution.constraint(strat,
#                             paramset.label="BBOPT",
#                             distribution.label.1="StopLossShort",
#                             distribution.label.2="StopLossLong",
#                             operator="==",
#                             label="StopLossEq")

# # Take Profit
# add.distribution(strat,
#                  paramset.label = "TPOPT",
#                  component.type = "chain",
#                  component.label = "TakeProfitLONG",
#                  variable = list( threshold = takeProfitPercent),
#                  label = "TakeProfitLong")
# 
# add.distribution(strat,
#                  paramset.label = "TPOPT",
#                  component.type = "chain",
#                  component.label = "TakeProfitSHORT",
#                  variable = list( threshold = takeProfitPercent),
#                  label = "TakeProfitShort")
# 
# add.distribution.constraint(strat,
#                             paramset.label="TPOPT",
#                             distribution.label.1="TakeProfitShort",
#                             distribution.label.2="TakeProfitLong",
#                             operator="==",
#                             label="TakeProfitEq")

library(foreach) 
library(parallel)
cores<-detectCores()

 if( Sys.info()['sysname'] == "Windows" )
 {
     library(doParallel)
      cl <- makeCluster(cores)
      registerDoParallel(cl)
#     library(doRedis)
#     registerDoRedis(queue = "jobs", host = "192.168.137.1",port = 6379)
#     startLocalWorkers(n=cores, queue="jobs", host = "192.168.137.1",port = 6379)
     
 } else {
     library(doMC)
     registerDoMC(cores=cores)
 }
#registerDoSEQ() # NO PARALLEL
# 
# grep<-function (pattern, x, ignore.case = FALSE, perl = FALSE, value = TRUE, 
#                 fixed = FALSE, useBytes = FALSE, invert = FALSE) 
#     base::grep(pattern, x, ignore.case, perl, value,fixed, useBytes, invert)


results<-apply.paramset(strategy.st=strat, 
                          paramset.label='BBOPT', 
                          portfolio.st=port, 
                          account.st=acct,
                          verbose=TRUE) 

removeQueue ("jobs")

df<-data.frame((results$tradeStats))
df[order(df[,10]),c(1,2,3,4,5,8,9,10)]  
Sys.time()-timerS


df[order(df[,10]),] 





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
# # 3D
# require(rgl)
# s<-symbol[1]
# res<-results$tradeStats[results$tradeStats[,5]==s,]
# tradeGraphs (
#     stats = res,
#     free.params = c("fast", "slow"),
# 
#     statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades"),
#     title = 'bumblebee'
# )




