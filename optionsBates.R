# Bates Option Volatility Model calibration

library(ggplot2)
library(data.table)
library(stringr)
library(tidyr)
library(NMOF)
library(DEoptim)
library(fOptions)
library(rusquant)
######################################################################################
# Market VOL research
#
#
######################################################################################


# MOEX Option Desk for current datetime
symbol<-"Si-3.16"
expDate<-"2016-03-15"
period="1min"
symb<-"SiH6 (03.2016)"
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
curOpDesk<-data.table(curOpDesk, DateTime=as.POSIXct(Sys.time()))

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
data("tickers")
LastPriceSymb<-data.table(getSymbols(symb, from=curOpDesk$DateTime[1], period=period, src='mfd',adjust=TRUE, auto.assign=FALSE))[.N,]
curOpDesk[,StockPriceMid:=LastPriceSymb[,(Open+High+Low+Close)/4]]
curOpDesk[,c("PRICE", "tau"):=.((Bid+Ask)/2,
                                as.numeric((as.POSIXct(expDate)-DateTime)/365))]
curOpDesk[,c("PRICEBid", "tau"):=.(Bid,
                                as.numeric((as.POSIXct(expDate)-DateTime)/365))]

curOpDesk[,c("PRICEAsk", "tau"):=.(Ask,
                                   as.numeric((as.POSIXct(expDate)-DateTime)/365))]

curOpDesk[,id:=.I]
curOpDesk<-curOpDesk[PRICE<TheoPrice*1.2 & PRICE>TheoPrice*0.8]
curOpDesk<-curOpDesk[PRICEBid<TheoPrice*1.2 & PRICEBid>TheoPrice*0.8]
curOpDesk<-curOpDesk[PRICEAsk<TheoPrice*1.2 & PRICEAsk>TheoPrice*0.8]

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

#Plot Hist GBS Volatility Smile
nStrikes<-25
StrikeStep<-250
MaxStrike<-floor(LastPriceSymb$Close/StrikeStep)*StrikeStep+nStrikes*StrikeStep
MinStrike<-floor(LastPriceSymb$Close/StrikeStep)*StrikeStep-nStrikes*StrikeStep

ggplot()+
    #geom_line(data=histOptData[GBSIV<1],aes(x=Strike,y=GBSIV*100,colour=format(DateTime,"%d %H")))+
    geom_line(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
              aes(x=Strike,y=IV),colour="#4b0082")+
#     geom_point(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
#                aes(x=Strike,y=GBSIVBidAsk*100,shape=factor(TypeFlag), size=OI),colour="indianred")+
    geom_smooth(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
                aes(x=Strike,y=GBSIVBidAsk*100),colour="indianred")+

  geom_point(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
             aes(x=Strike,y=GBSIVBid*100,shape=factor(TypeFlag), size=OI),colour="darkgreen")+
  geom_line(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
              aes(x=Strike,y=GBSIVBid*100),colour="darkgreen")+
  
  
  
  geom_point(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
             aes(x=Strike,y=GBSIVAsk*100,shape=factor(TypeFlag), size=OI),colour="darkred")+
  geom_line(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
              aes(x=Strike,y=GBSIVAsk*100),colour="darkred")+
  
  #    geom_point(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
#               aes(x=Strike,y=GBSIVCalc*100,shape=factor(TypeFlag),size=OI),colour="darkcyan")+
#    geom_smooth(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
#                aes(x=Strike,y=GBSIVCalc*100),colour="darkcyan")+
#     geom_point(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
#                aes(x=Strike,y=GBSIVTheo*100, shape=factor(TypeFlag),size=OI),colour="forestgreen")+
    geom_smooth(data=curOpDesk[Strike>=MinStrike &Strike<=MaxStrike],
                aes(x=Strike,y=GBSIVTheo*100),colour="forestgreen")+
    geom_vline(xintercept=LastPriceSymb$Close)+
    scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StrikeStep*2)) + 
    scale_y_continuous(breaks=seq(1,100,0.25))+
    annotate("text", label = "Market IV", x = MinStrike+StrikeStep*1, y = 25, size = 4, colour = "#4b0082")+
  annotate("text", label = "Bid/Ask IV", x = MinStrike+StrikeStep*6, y = 25, size = 4, colour = "indianred")+
  annotate("text", label = "Bid IV", x = MinStrike+StrikeStep*11, y = 25, size = 4, colour = "darkgreen")+
  annotate("text", label = "Ask IV", x = MinStrike+StrikeStep*16, y = 25, size = 4, colour = "darkred")+
  annotate("text", label = "CalcPrice IV", x = MinStrike+StrikeStep*21, y = 25, size = 4, colour = "darkcyan")+
    annotate("text", label = "TheoPrice IV", x = MinStrike+StrikeStep*26, y = 25, size = 4, colour = "forestgreen")
    

######################################################################################
# Stochastic VOL research
######################################################################################
#Load MOEX option trades history
histDates<-paste("201602", 17:17, sep="")
getOptionHitory<-function(histDate){
  link<-paste("ftp://ftp.moex.com/pub/FORTS/pubstat/",
              histDate, "/",
              histDate,
              "_csv.zip",
              sep="")
  destFile<-tempfile()
  download.file(link, destfile =destFile)
  unzip(destFile, files="opt_deal.csv")
  df<-read.csv("opt_deal.csv",sep=";",stringsAsFactors = FALSE)
  file.remove("opt_deal.csv")
  file.remove(destFile)
  df<-data.table(df)
  df[,ISIN:=gsub(" ", "", ISIN)]
  df[,Expiration:=as.POSIXct(strptime(str_extract(ISIN,"[0-9]{6,6}"),"%d%m%y"))]
  df[,c("Symbol","X"):=tstrsplit(ISIN,"M[0-9]{6,6}")]
  df[,Strike:=as.numeric(tstrsplit(X,"PA|CA")[[2]])]
  df[,OptType:=tstrsplit(X,"[0-9]")[[1]]]
  df[,DateTime:=as.POSIXct(strptime(paste(DATE, TIME), "%d.%m.%Y %H:%M:%S"))]
 
  print(link)
  df[,.(DateTime, Symbol, OptType, Strike, Expiration, PRICE, VOL)]
}

tradesOpdf<-rbindlist(lapply(histDates, getOptionHitory))
tradesOpdf[,id:=.I]
tradesOpdf[,tau:=as.numeric(difftime(Expiration,DateTime, "days"))/365,by=id]


histOptData<-tradesOpdf[Expiration==expDate & Symbol==symbol,.SD,by=Strike]
from<-as.Date(histOptData[,min(DateTime)])
to<-as.Date(histOptData[,max(DateTime)])
period="1min"
symb<-"SiH6 (03.2016)"
symbData<-getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=FALSE)
symbData<-data.frame(DateTimeSymb=as.POSIXct(index(symbData)),as.data.frame(symbData, stringsAsFactors=FALSE),stringsAsFactors=FALSE)
symbData=data.table(symbData)
symbData[,dtkey:=format(DateTimeSymb, "%Y%m%d%H%M")]
histOptData[,dtkey:=format(DateTime, "%Y%m%d%H%M")]
setkey(symbData,dtkey)
setkey(histOptData, dtkey)
histOptData<-symbData[histOptData]

histOptData[,PriceMid:=(Open+High+Low+Close)/4,]
histOptData[,id:=.I]
histOptData[,GBSIV:=GBSVolatility(price=PRICE, 
                             TypeFlag = strtrim(tolower(OptType),1), 
                             S=PriceMid,
                             X=Strike,
                             Time=as.numeric(tau),
                             r=0,
                             b=0,
                             maxiter=100), by=id]


ggplot()+
    geom_point(data=histOptData[Strike>=MinStrike & Strike<=MaxStrike & GBSIV<1],aes(x=Strike,y=GBSIV*100,colour=format(DateTime,"%d%H")))+
    geom_line(data=histOptData[Strike>=MinStrike & Strike<=MaxStrike & GBSIV<1],aes(x=Strike,y=GBSIV*100,colour=format(DateTime,"%d%H")))+
    geom_vline(xintercept=LastPriceSymb$Close)+
    scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StrikeStep*2)) + 
    scale_y_continuous(breaks=seq(1,100,0.25))+
    annotate("text", label = "HistPrice IV by hours", x = MinStrike*1.2, y = 13, size = 4, colour = "forestgreen")



# BATES Model calibration
#' This is the R version of the error function and should be used for performance benchmarking and diagnostic purposes only

# optimfun<-function(p, data){
#   
#   res=apply(data, 1, FUN=function(x){
#     (callCF(cf = cfBates, S = x[2], X = x[3], tau = x[4], 
#             r=0,
#             q=0,
#             v0=p[1],
#             vT=p[2],
#             rho=p[3],
#             k=p[4],
#             sigma=p[5],
#             lambda=p[6],
#             muJ=p[7],
#             vJ=p[8], implVol = FALSE)-x[1])^2
#   })
#   sqrt(sum(res))/(length(res))
# }

optimfun<-function(p, data){
    data[, res:=
        (callCF(cf = cfBates, S = PriceMid, X = Strike, tau = tau, 
                r=0,
                q=0,
                v0=p[1],
                vT=p[2],
                rho=p[3],
                k=p[4],
                sigma=p[5],
                lambda=p[6],
                muJ=p[7],
                vJ=p[8], implVol = FALSE)-PRICE)^2,by=1:nrow(data)]
    sqrt(sum(data$res))/nrow(data)
}

optimfunNL<-function(p){
    data<-histOptData[,.(PRICE, PriceMid, Strike, tau)]
    data[, res:=
             (callCF(cf = cfBates, S = PriceMid, X = Strike, tau = tau, 
                     r=0,
                     q=0,
                     v0=p[1],
                     vT=p[2],
                     rho=p[3],
                     k=p[4],
                     sigma=p[5],
                     lambda=p[6],
                     muJ=p[7],
                     vJ=p[8], implVol = FALSE)-PRICE)^2,by=1:nrow(data)]
    sqrt(sum(data$res))/nrow(data)
}

# optimfunloop<-function(p, data){
#     res<-rep(0,nrow(data))
#     for(i in 1:nrow(data))
#     {
#         res[i]<-(callCF(cf = cfBates, S = data[i,PriceMid], X = data[i,Strike], tau = data[i,tau], 
#                         r=0,
#                         q=0,
#                         v0=p[1],
#                         vT=p[2],
#                         rho=p[3],
#                         k=p[4],
#                         sigma=p[5],
#                         lambda=p[6],
#                         muJ=p[7],
#                         vJ=p[8], implVol = FALSE)-data[i,PRICE])^2
#     }
#     sqrt(sum(res))/(length(res))
# }

# library(microbenchmark)
# microbenchmark(optimfunloop(u,data =histOptData[format(DateTime,timeInterval)==timeSlice&OptType=="CA",
#                                       .(PRICE, PriceMid, Strike, tau)]),
#           optimfun(u,data =histOptData[format(DateTime,timeInterval)==timeSlice&OptType=="CA",
#                                    .(PRICE, PriceMid, Strike, tau)]),
#           optimfunDT(u,data =histOptData[format(DateTime,timeInterval)==timeSlice&OptType=="CA",
#                                   .(PRICE, PriceMid, Strike, tau)]),times=5)

# Calculate Call prices only through put/call parity
histOptData[OptType=="PA",PRICE:=putCallParity("call", put = PRICE,  S=PriceMid, X=Strike, tau=tau, r=0)]
histOptData[OptType=="PA",OptType:="CA"]

timeInterval<-"%d%H%M"
timeSlice<-as.numeric(histOptData[OptType=="CA",.N, by=format(DateTime,timeInterval)][N>10][.N-1,format])
histOptDataBackUp<-histOptData

nStrikes<-20
StrikeStep<-250

histOptData<-histOptData[OptType=="CA" & 
                             as.numeric(format(DateTime,timeInterval))>=timeSlice&
                             as.numeric(format(DateTime,timeInterval))<=timeSlice+10]

MaxStrike<-floor(histOptData[.N,PriceMid]/StrikeStep)*StrikeStep+nStrikes*StrikeStep
MinStrike<-floor(histOptData[.N,PriceMid]/StrikeStep)*StrikeStep-nStrikes*StrikeStep

histOptData<-histOptData[Strike>=MinStrike & Strike<=MaxStrike]

#Optimisation setup
eps <- 1e-8
l<-c(eps, eps, -1.0+eps,eps,eps, eps,eps,eps)
u<-c(5.0-eps, 1.0-eps,1.0-eps, 1.0-eps, 1.0-eps,1.-eps,1.0-eps,1.0-eps)
startTime<- Sys.time()
maxIt <- 20 
population <- 100

fit = DEoptim(fn=optimfun, lower=l, upper=u, control=list(NP=population, itermax=maxIt),
              data =histOptData[,.(PRICE, PriceMid, Strike, tau)] )
Sys.time()-startTime

#as.numeric(fit$optim$bestmem)
#summary(fit)

# #inpstall.packages("nloptr")
library(nloptr)
p<-as.numeric(fit$optim$bestmem)
# eval_g_ineq <- function (x) {
#     grad <- c(-2.0*x[2],-2.0*x[1],2.0*x[3],0,0)
#     return(list("constraints"=c(x[3]*x[3] - 2.0*x[1]*x[2]), "jacobian"=grad))  
# }
res<- nloptr( x0=p, 
              eval_f=optimfunNL, 
              #eval_g_ineq=eval_g_ineq,
              lb = l, 
              ub = u, 
              opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-7))

print(paste("Total Calibration Time:",Sys.time()-startTime))
print(paste("Solution: ", res$solution))
print(paste("RMSE: ", res$objective))
p<-res$solution

r=0
q=0
v0=p[1]
vT=p[2]
rho=p[3]
k=p[4]
sigma=p[5]
lambda=p[6]
muJ=p[7]
vJ=p[8]

# lastSpotPrice<-last(symbData$Close)
# tau<-as.numeric(diff(c(last(symbData$DateTimeSymb),as.POSIXct("2015-12-15 18:45:00"))))/252
# bdf<-curOpDesk[TypeFlag=="CA",c("BatesCall","BatesIV"):= callCF(cf = cfBates, S = lastSpotPrice, X = Strike, tau = tau, r = r, q = q,
#                                                   v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
#                                                   lambda = lambda, muJ = muJ, vJ = vJ, implVol = TRUE), by=Strike]

histOptData[,c("BatesCall","BatesIV"):= callCF(cf = cfBates, S = PriceMid, X = Strike, tau = tau, r = r, q = q,
                                                           v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
                                                           lambda = lambda, muJ = muJ, vJ = vJ, implVol = TRUE), by=id]

ggplot()+
    geom_point(data=histOptData,aes(x=Strike,y=GBSIV*100),colour="mediumaquamarine")+
    geom_line(data=histOptData,aes(x=Strike,y=GBSIV*100),colour="mediumaquamarine")+
    geom_point(data=histOptData,aes(x=Strike,y=BatesIV*100),color="lightcoral")+
    geom_line(data=histOptData,aes(x=Strike,y=BatesIV*100),color="lightcoral")+
    geom_vline(xintercept=histOptData[.N,PriceMid])+
    scale_x_continuous(breaks=seq(MinStrike,MaxStrike,StrikeStep*2)) + 
    scale_y_continuous(breaks=seq(1,100,0.25))+
    annotate("text", label = "HistPrice IV", x = MinStrike+StrikeStep*nStrikes, y = histOptData[,min(GBSIV)]*100, size = 4, colour = "mediumaquamarine")+
    annotate("text", label = "Bates IV", x = MinStrike+StrikeStep*nStrikes, y = histOptData[,min(BatesIV)]*100, size = 4, colour = "lightcoral")

# 
# 
# 
# smile<-ggplot()+
#   geom_line(data=bdf,aes(x=Strike, y=IV),color="mediumaquamarine")+
#   geom_point(data=bdf,aes(x=Strike, y=IV), colour="mediumaquamarine")+
#   geom_line(data=bdf,aes(x=Strike, y=BatesIV*100),color="lightcoral")+
#   geom_point(data=bdf,aes(x=Strike, y=BatesIV*100), colour="lightcoral")
# #  geom_line(data=histOptData,aes(x=Strike,y=GBSIV*100), colour="green")+
# #  geom_point(data=histOptData,aes(x=Strike,y=GBSIV*100), colour="green")
# smile
