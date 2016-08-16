library(rusquant)
library(quantstrat)
# Bumblebee trading system
# copyright (c) 2009-2013, Algorithm Alpha, LLC
# RUSSIAN MARKET INTRADAY
# 
timerS<-Sys.time()
############################# GET DATA ######################################
from<-as.Date("2014-12-22")
to<-as.Date("2014-12-29")#Sys.Date()
period="1min"
symbols<-c("SiH5 (03.2015)",
            "SRH5 (03.2015)",
            "GZH5 (03.2015)",
            "RIH5 (03.2015)")
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

port          = 'bumblebeePort'
acct          = 'bumblebeeAcct'
strat         = "bumblebee"
initEq        = 100000
initDate      = '1969-12-31'
fast          = 15
slow          = 30
sd            = 0.75
intraDay      = TRUE

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
currency('RUR')
stock(symbols,currency='RUR', multiplier=1)
initPortf(port, symbols, initDate=initDate)
initAcct(acct, port, initEq=initEq, initDate=initDate)
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
              name='BBands',label='bb', 
              arguments = list(HLC=quote(HLC(mktdata)), n=slow, sd=sd, maType='SMA'))

# SMA is is like bumblebee wings
add.indicator(strat, name='SMA', label='fast', arguments = list(x=quote(Cl(mktdata)), n=fast))


############################# SIGNALS #######################################

add.signal(strat, 
           name='sigCrossover', 
           label= 'fast.lt.dn', 
           arguments = list(columns=c('fast','dn'), relationship='lt'))

add.signal(strat, 
           name='sigCrossover', 
           label='fast.gt.up', 
           arguments = list(columns=c('fast','up'), relationship='gt'))

add.signal(strat,
           name='sigTimestamp',
           label='endday',
           arguments=list(timestamp="23:48"))
           
############################# RULES #########################################

# Just imagine fat bumblebee. It's trying to fly.
# See when he works hardly his wings are above the body and he will up.
# Otherwise he will go down. So growing fast SMA should make main trend growing.

######################## LONG RULES #########################################
add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='enter', 
         label='EnterLONG',
         arguments=list(sigcol= 'fast.gt.up', 
                        sigval= TRUE, 
                        orderqty=100, 
                        ordertype = 'market', 
                        orderside='long', 
                        osFUN= 'osMaxPos'))


add.rule(strategy  =strat, 
         name='ruleSignal', 
         type='exit', 
         label='ExitLONG', 
         arguments=list(sigcol='fast.lt.dn', 
                        sigval=TRUE, 
                        orderqty ='all', 
                        ordertype='market', 
                        orderside='long'))

######################## SHORT RULES #########################################
add.rule(strategy  = strat,
         name      = 'ruleSignal',
         arguments = list(sigcol     = 'fast.lt.dn',
                          sigval    = TRUE,
                          orderqty  =  -100,
                          ordertype = 'market',
                          orderside = 'short',
                          osFUN     = 'osMaxPos'),
         type      = 'enter',
         label     = 'EnterSHORT')

add.rule(
    strategy  = strat,
    name      = 'ruleSignal',
    arguments = list(sigcol     = 'fast.gt.up',
                     sigval     = TRUE,
                     orderqty   = 'all',
                     ordertype  = 'market',
                     orderside  = 'short'),
    type      = 'exit',
    label     = 'ExitSHORT')

######################## NO OVERNIGHT RULES #########################################
add.rule(
    strategy  = strat,
    name      = 'ruleSignal',
    arguments = list(sigcol     = 'endday',
                     sigval     = TRUE,
                     orderqty   = 'all',
                     ordertype  = 'market'),
    type      = 'exit',
    label     = 'CloseOpenPos')

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
# ######################## Optimization ##########################################
# timerS<-Sys.time()
# # # 3D
# # tradeGraphs (
# #     stats = tStats,
# #     free.params = c("fast.lt.dn", "fast.gt.up"),
# # 
# #     statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades"),
# #     title = 'bumblebee'
# # )
# 
# 
# #Check overnight position exists
# # timeLimit<-"23:50:00"
# # datesLimit<-paste(seq(from, to, 1), timeLimit)
# # lapply(datesLimit, function(x) getPos(port, symbol, x))
# 
# fast = 10:15 
# slow = 30:40
# sd=seq(0.5,1.25, by=0.25)
# 
# add.distribution(strat, 
#                  paramset.label = 'BBOPT', 
#                  component.type = 'indicator', 
#                  component.label = 'fast', 
#                  variable = list(n = fast), 
#                  label = 'fast') 
# 
# add.distribution(strat, 
#                  paramset.label = 'BBOPT', 
#                  component.type = 'indicator', 
#                  component.label = 'bb', 
#                  variable = list(n = slow), 
#                  label = 'slow') 
# 
# add.distribution(strat, 
#                  paramset.label = 'BBOPT', 
#                  component.type = 'indicator', 
#                  component.label = 'bb', 
#                  variable = list(sd = sd), 
#                  label = 'sd') 
# 
# # add.distribution.constraint(strat.st, 
# #                             paramset.label = 'SMA', 
# #                             distribution.label.1 = 'nFAST', 
# #                             distribution.label.2 = 'nSLOW', 
# #                             operator = '<', 
# #                             label = 'SMA' 
# # ) 
# 
# require(foreach) 
# require(doMC)  #  OSX
# registerDoMC(cores=2)  # OSX
#  #OSX
# #require(doParallel)  # windows 
# #cl <- makeCluster(2) # windows 
# #registerDoParallel(cl) #windows
# 
# results <- apply.paramset(strat, 
#                           paramset.label='BBOPT', 
#                           portfolio.st=port, 
#                           account.st=acct) 
# 
# 
# 
# 
# #stopCluster(cl) # windows
# df<-data.frame((results$tradeStats))
# qplot(x=Max.Drawdown, y=Net.Trading.PL, colour=Num.Trades,data=df, facets=.~Symbol)
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
# df  
# Sys.time()-timerS
# 
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
