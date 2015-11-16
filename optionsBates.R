# Bates Option Volatility Model calibration

library(ggplot2)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(NMOF)
library(DEoptim)
library(fOptions)
library(rusquant)

# MOEX Option Desk for current datetime
symbol<-"Si-12.15"
expDate<-"2015-12-15"
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
curOpDesk<-data.table(curOpDesk)

#Plot MOEX Volatility Smile
qplot(x=Strike,y=IV, data=curOpDesk)


#Load MOEX pption trades history
histDates<-paste("201509", 29, sep="")
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
  
  df<-df%>%
    mutate(ISIN=gsub(" ", "", ISIN))%>%
    mutate(Expiration=str_extract(ISIN,"[0-9]{6,6}"))%>%
    separate(ISIN,c("Symbol","X"),sep="M[0-9]{6,6}", remove=FALSE)%>%
    separate(X,c("X1","Strike"),sep="PA|CA", remove=FALSE)%>%
    separate(X,c("OptType","X2"),sep="[0-9]", remove=FALSE, extra="drop")%>%
    select(-X, -X1, -X2)
  
  df<-df%>% 
    mutate(Symbol=as.factor(Symbol),
           OptType=as.factor(OptType),
           Strike=as.numeric(Strike),
           Expiration=as.POSIXct(strptime(Expiration, "%d%m%y")),
           DateTime=as.POSIXct(strptime(paste(DATE, TIME), "%d.%m.%Y %H:%M:%S")))%>%
    select(-DATE, -TIME, -ISIN,-ID_DEAL, -TYPE)%>%
    select(DateTime, Symbol, OptType, Strike, Expiration, PRICE, VOL)
  print(link)
  df
}

tradesOpdf<-rbindlist(lapply(histDates, getOptionHitory))
tradesOpdf<-data.table(tradesOpdf)
tradesOpdf[,tau:=Expiration-DateTime]
tradesOpdf[,id:=.I]


mydata<-tradesOpdf[Expiration==expDate & Symbol==symbol,.SD,by=Strike]
from<-as.Date(mydata[,min(DateTime)])
to<-as.Date(mydata[,max(DateTime)])
period="1min"
symb<-"SiZ5 (12.2015)"
symbData<-getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=FALSE)
symbData<-data.frame(DateTimeSymb=as.POSIXct(index(symbData)),as.data.frame(symbData, stringsAsFactors=FALSE),stringsAsFactors=FALSE)
symbData=data.table(symbData)
symbData[,dtkey:=format(DateTimeSymb, "%Y%m%d%H%M")]
mydata[,dtkey:=format(DateTime, "%Y%m%d%H%M")]
setkey(symbData,dtkey)
setkey(mydata, dtkey)
mydata<-symbData[mydata]
mydata[,c("PriceMid", "tau"):=.((Open+High+Low+Close)/4,as.numeric((Expiration-DateTime)/252))]
mydata[,id:=.I]
mydata[,GBSIV:=GBSVolatility(price=PRICE, 
              TypeFlag = strtrim(tolower(OptType),1), 
              S=PriceMid,
              X=Strike,
              Time=as.numeric(tau),
              r=0,
              b=0,
              maxiter=100), by=id]


#Plot Hist GBS Volatility Smile
qplot(x=Strike,y=GBSIV*100, data=mydata, color=OptType)

# BATES Model calibration
optimfun<-function(p, data){
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
  res=apply(data, 1, FUN=function(x){
    (callCF(cf = cfBates, S = x[2], X = x[3], tau = x[4], r = r, q = q,
            v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
            lambda = lambda, muJ = muJ, vJ = vJ, implVol = FALSE)-x[1])^2
  })
  sum(res)
}

low = c(0.001,0.001,-1,0.001,0.001,0.001,0.001,0.001)
high = c(1,1,1,1,1,1,1,1)

#fit = DEoptim(optimfun, low, high, 
#       data =mydata[OptType=="CA",.(PRICE, PriceMid, Strike, tau)] )
#summary(fit)

p<-c(0.032934,    0.145827,    0.075226,    0.349588,    0.437278,    0.029334,    0.175097,    0.439060)
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

lastSpotPrice<-last(symbData$Close)
tau<-as.numeric(diff(c(last(symbData$DateTimeSymb),as.POSIXct("2015-12-15 18:45:00"))))/252
bdf<-curOpDesk[,c("BatesCall","BatesIV"):=
                                                        callCF(cf = cfBates, S = lastSpotPrice, X = Strike, tau = tau, r = r, q = q,
                                                               v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
                                                               lambda = lambda, muJ = muJ, vJ = vJ, implVol = TRUE), by=Strike]


smile<-ggplot()+
  geom_line(data=bdf,aes(x=Strike, y=IV),color="mediumaquamarine")+
  geom_point(data=bdf,aes(x=Strike, y=IV), colour="mediumaquamarine")+
  geom_line(data=bdf,aes(x=Strike, y=BatesIV*100),color="lightcoral")+
  geom_point(data=bdf,aes(x=Strike, y=BatesIV*100), colour="lightcoral")+
  geom_line(data=mydata,aes(x=Strike,y=GBSIV*100), colour="green")+
  geom_point(data=mydata,aes(x=Strike,y=GBSIV*100), colour="green")
smile
