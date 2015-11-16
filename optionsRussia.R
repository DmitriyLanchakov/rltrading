library(ggplot2)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
# MOEX Option Desk
symbol<-"Si-12.15"
expDate<-"15-12-15"
link<-paste("http://moex.com/ru/derivatives/optionsdesk-csv.aspx?code=",
            symbol,
            "&sid=1&c1=on&c2=on&c3=on&c4=on&c5=on&c6=on&c7=on&marg=1&delivery=",
            expDate, sep="")

df<-read.csv(link, sep=",",header=FALSE, stringsAsFactors=FALSE, skip=1)
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
df<-df[,1:30]
colnames(df)<-header
df<-data.table(df)

qplot(x=Strike,y=IV, data=df)
qplot(x=Strike,y=COI, data=df)
qplot(x=Strike,y=POI, data=df)
qplot(x=Strike,y=CBid, data=df)
qplot(x=Strike,y=CAsk, data=df)
qplot(x=Strike,y=(CAsk-CBid+PAsk-PBid)/2, data=df)
qplot(x=Strike, y=value, 
      data=melt(df, id="Strike",measure.vars =c("CBid", "PBid", "CAsk", "PAsk"))[value<10000],
      color=variable)

#Bates model
library(NMOF)
library(DEoptim)
library(fOptions)
S <- 65600:65700; X <- 65000; tau <- seq(3.01,3.1, by=0.001)/252
st<-data.frame(S, tau)
r <- 0; q <- 0
v0 <- 0.2^2  ## variance, not volatility
vT <- 0.2^2  ## variance, not volatility
v <- vT
rho <- -0.3; k <- .2
sigma <- 0.2

## jump parameters (Merton and Bates)
lambda <- 0.1
muJ <- -0.2
vJ <- 0.1^2
# Cost of call option
apply(st,1, FUN=function(x)callCF(cf = cfBates, S = x[1], X = X, tau = x[2], r = r, q = q,
       v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
       lambda = lambda, muJ = muJ, vJ = vJ))

GBSVolatility(price=929, 
              TypeFlag = "c", 
              S=S, 
              X=X,
              Time=tau,
              r=0,
              b=0,
              maxiter=100)



#MOEX OPtionHistory
histDates<-paste("201509", 21:25, sep="")

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

opdf<-rbindlist(lapply(histDates, getOptionHitory))

opdf<-data.table(opdf)

mydata<-opdf[Expiration=="2015-12-15" & Symbol=="Si-12.15"& OptType=="CA",.SD,by=Strike]
from<-as.Date(mydata[,min(DateTime)])
to<-as.Date(mydata[,max(DateTime)])
period="1min"
symb<-"SiZ5 (12.2015)"
library(rusquant)
symbData<-getSymbols(symb, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=FALSE)
symbData<-data.frame(DateTimeSymb=as.POSIXct(index(symbData)),as.data.frame(symbData, stringsAsFactors=FALSE),stringsAsFactors=FALSE)
symbData=data.table(symbData)
symbData[,dtkey:=format(DateTimeSymb, "%Y%m%d%H%M")]
mydata[,dtkey:=format(DateTime, "%Y%m%d%H%M")]
setkey(symbData,dtkey)
setkey(mydata, dtkey)

mydata<-symbData[mydata]
#mydata[format(DateTime, "%Y%m%d%H%M")==symbData[,format(DateTime, "%Y%m%d%H%M")]]

mdata<-mydata[,.(PRICE, (Open+High+Low+Close)/4,Strike,as.numeric((Expiration-DateTime)/252))]
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

library(NMOF)
library(DEoptim)
library(fOptions)

fit = DEoptim(optimfun, low, high, 
              data =mdata )
summary(fit)

getBates<-function(Strikes){
  p<-c( 0.024414,0.203118,0.701287,0.208044,0.524402,0.182265, 0.223023,0.086310)
  r=0
  q=0
  tau=as.numeric(diff(as.POSIXct(c("2015-08-21 12:45:00","2015-09-15 18:45:00"))))/252
  v0=p[1]
  vT=p[2]
  rho=p[3]
  k=p[4]
  sigma=p[5]
  lambda=p[6]
  muJ=p[7]
  vJ=p[8]
  lapply(Strikes, FUN=function(x)callCF(cf = cfBates, S = x, X = 65000, tau = tau, r = r, q = q,
         v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
         lambda = lambda, muJ = muJ, vJ = vJ, implVol = TRUE))

}


bdf<-df[CLastTradePrice>0 & PLastTradePrice>0][,c("BatesCall","BatesIV"):=
     callCF(cf = cfBates, S = 68689, X = Strike, tau = tau, r = r, q = q,
                                     v0 = v0, vT = vT, rho = rho, k = k, sigma = sigma,
                                     lambda = lambda, muJ = muJ, vJ = vJ, implVol = TRUE), by=Strike]


smile<-ggplot()+
  geom_line(data=bdf,aes(x=Strike, y=IV),color="mediumaquamarine")+
  geom_point(data=bdf,aes(x=Strike, y=IV), colour="mediumaquamarine")+
  geom_line(data=bdf,aes(x=Strike, y=BatesIV*100),color="lightcoral")+
  geom_point(data=bdf,aes(x=Strike, y=BatesIV*100), colour="lightcoral")
smile