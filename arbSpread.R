library(dplyr)
library(data.table)

#Rate depended futures Spread calculation
getRateSpread<-function(symbols, rate,dateExp,lot=1, sdPeriod=30, daysYear=365){
    df<-cbind(SMA(Cl(get(symbols[1])),3),SMA(Cl(get(symbols[2])),3), Vo(get(symbols[1])))
    df<-data.frame(datetime=as.POSIXct(index(df)),as.data.frame(df))
    colnames(df)<-c("datetime","fut", "asset", "futvol")
    #df<-distinct(df)
    df$daysexp<-sapply(df$datetime, FUN=function(x) diff(as.numeric(x-dateExp, "days")))
    df<-mutate(df,
               asset=asset*lot,
               futfair=asset*exp(rate*daysexp/daysYear),
               spread=fut-futfair, 
               ratefair=log(fut/asset)*daysYear/daysexp,
               spreadabs=fut-asset)
    df$spreadsd<-c(rep(NA, sdPeriod-1),rollapply(df$spread,sdPeriod, sd, na.rm=TRUE))
    df
}



#Stat Arb depended futures Spread calculation
getStatSpread<-function(symbols, rollperiod=0, smaPeriod=1, daysYear=365){
    symbDT<-cbind(SMA(Cl(get(symbols[1])),smaPeriod),
                  SMA(Cl(get(symbols[2])),smaPeriod))
    
    symbDT<-data.table(as.POSIXct(index(symbDT)), symbDT)

    setnames(symbDT,c("datetime","symbA", "symbB"))
    symbDT<-symbDT[complete.cases(df),]
    symbDT[,retA:=as.numeric(Delt(symbA,type="log"))]
    symbDT[,retB:=as.numeric(Delt(symbB,type="log"))]
    symbDT[,retBeta:=as.numeric(coef(lm(retA ~ retB+0))[1])]
    symbDT[,retSpread := retA - retBeta*retB]
    #priceModel<-lm(as.numeric(symbDT$symbA) ~ as.numeric(symbDT$symbB)+0)
    betaCoef<-as.numeric(lm(as.numeric(symbDT$symbA) ~ as.numeric(symbDT$symbB)+0)[1])
    betaCoef<-floor(betaCoef)
    symbDT[,spread:=symbA - betaCoef*symbB]
    
#     rolllm<-rollapply(df,width =100,
#               FUN= function (x)coef(lm(x[,1] ~ x[,2], data=as.data.frame(x))),
#               by.column=FALSE, fill = NULL)
    
    #summary(symbDT)
}



