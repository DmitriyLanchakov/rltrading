library(rusquant)
library(quantstrat)
# Bumblebee trading system
# copyright (c) 2009-2013, Algorithm Alpha, LLC
# RUSSIAN MARKET INTRADAY
# WITH STOP LOSS

timerS<-Sys.time()
############################# GET DATA ######################################
from<-as.Date("2014-12-20")
to<-as.Date("2014-12-31")#Sys.Date()
period="1min"
symbols<-c("SiH5 (03.2015)",
            "SRH5 (03.2015)",
            "GZH5 (03.2015)",
            "RIH5 (03.2015)")


#symbols<-c("SiZ4 (12.2014)")#,
          # "SRZ4 (12.2014)",
          # "GZZ4 (12.2014)",
          # "RIZ4 (12.2014)")
#symbols<-c("SiH5 (03.2015)")#,

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
EndOFDatTime  ="23:48"
intraDay      = TRUE
StopLossFlag  = FALSE
stopLossPercent= 0.01
StopTrailingFlag = !StopLossFlag
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
        maxpos=1)


############################# INDICATORS ####################################

# Bolinger Bands  is like bumblebee body
add.indicator(strat, 
              name='BBands', label='bb',
              arguments = list(HLC=quote(HLC(mktdata)), n=slow, sd=sd, maType='SMA'))

# SMA is is like bumblebee wings
add.indicator(strat, name='SMA', label='fast', arguments = list(x=quote(Cl(mktdata)), n=fast))


############################# SIGNALS #######################################
add.signal(strat, 
           name='sigCrossover', 
           label= 'fld', 
           arguments = list(columns=c('SMA.fast','dn'), relationship='lt'))

#NEW
add.signal(strat, 
           name='sigCrossover', 
           label= 'flu', 
           arguments = list(columns=c('SMA.fast','up'), relationship='lt'))

add.signal(strat, 
           name='sigCrossover', 
           label='fgu', 
           arguments = list(columns=c('SMA.fast','up'), relationship='gt'))

#NEW
add.signal(strat, 
           name='sigCrossover', 
           label='fgd', 
           arguments = list(columns=c('SMA.fast','dn'), relationship='gt'))


add.signal(strat,
           name='sigTimestamp',
           label='endday',
           arguments=list(timestamp=EndOFDatTime))
           
############################# RULES #########################################

# Just imagine fat bumblebee. It's trying to fly.
# See when he works hardly his wings are above the body and he will up.
# Otherwise he will go down. So growing fast SMA should make main trend growing.

######################## LONG RULES #########################################
#Enter Rule
add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='enter', 
         label='EnterLONG',
         path.dep=TRUE,
         arguments=list(sigcol= 'fgu', 
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
         arguments=list(sigcol='flu', 
                        sigval=TRUE, 
                        orderqty ='all', 
                        ordertype='market',
                        TxnFees=brokerFee,
                        orderset='ocolong',
                        orderside='long'))

# Stop Loss Rule

add.rule(strategy =strat,
         name="ruleSignal",
         type="chain", parent="EnterLONG",
         label="StopLossLONG",
         path.dep=TRUE,
         arguments = list(sigcol="fgu", 
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
         arguments = list(sigcol="fgu", 
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
         arguments = list(sigcol     = 'fld',
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
    arguments = list(sigcol     = 'fgd',
                     sigval     = TRUE,
                     orderqty   = 'all',
                     ordertype  = 'market',
                     TxnFees=brokerFee,
                     orderset='ocoshort',
                     orderside  = 'short'),
    type      = 'exit',
    label     = 'ExitSHORT')

# Stop Loss Rule

add.rule(strategy  =strat,
         name="ruleSignal",
         type="chain", parent="EnterSHORT",
         label="StopLossSHORT",
         path.dep=TRUE,
         arguments = list(sigcol="fld", 
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
         arguments = list(sigcol="fld", 
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
    arguments = list(sigcol     = 'endday',
                     sigval     = TRUE,
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
write.csv(data.frame(t(tStats[,-c(1,2)])),paste(fast, slow, sd,from, to,".csv", sep="_"))


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
slow = 25:30
sd=seq(0.1,0.3, by=0.1)
stopLossPercent = seq(0.01,0.01,by=0.001)

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

add.distribution(strat,
                 paramset.label = "BBOPT",
                 component.type = "chain",
                 component.label = "StopLossLONG",
                 variable = list( threshold = stopLossPercent),
                 label = "StopLossLong")

add.distribution(strat,
                 paramset.label = "BBOPT",
                 component.type = "chain",
                 component.label = "StopLossSHORT",
                 variable = list( threshold = stopLossPercent),
                 label = "StopLossShort")

add.distribution.constraint(strat,
                            paramset.label="BBOPT",
                            distribution.label.1="StopLossShort",
                            distribution.label.2="StopLossLong",
                            operator="==",
                            label="StopLossEq")
library(foreach) 
library(parallel)
cores<-detectCores()

if( Sys.info()['sysname'] == "Windows" )
{
    library(doParallel)
    registerDoParallel(cores=cores)  
    
} else {
    library(doMC)
    registerDoMC(cores=cores)
}
#registerDoSEQ() # NO PARALLEL
results<-apply.paramset(strat, 
                          paramset.label='BBOPT', 
                          portfolio.st=port, 
                          account.st=acct,
                          verbose=FALSE) 


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




