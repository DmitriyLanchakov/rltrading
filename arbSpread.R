library(dplyr)

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
getStatSpread<-function(symbols,lots, rollperiod=0, smaPeriod=1, daysYear=365){
    df<-cbind(lots[1]*SMA(Cl(get(symbols[1])),1),lots[2]*SMA(Cl(get(symbols[2])),1))
    #df<-data.frame(datetime=as.POSIXct(index(df)),as.data.frame(df))
    colnames(df)<-c("datetime","symbA", "symbB")
    
    retA=as.numeric(Delt(df$symbA,type="log"))
    retB=as.numeric(Delt(df$symbB,type="log"))
    retBeta <- as.numeric(coef(lm(retA ~ retB + 0))[1])
    retSpread <- retA - retBeta*retB
    priceModel<-lm(as.numeric(df$symbA) ~ as.numeric(df$symbB))
    
    rolllm<-rollapply(df,width =100,
              FUN= function (x)coef(lm(x[,1] ~ x[,2], data=as.data.frame(x))),
              by.column=FALSE, fill = NULL)
    
    summary(priceModel)
    spread <- df$symbA - as.numeric(coef(priceModel)[2])*df$symbB-as.numeric(coef(priceModel)[1])
    
    df<-mutate(df, 
               retspread=retSpread,
               spread=spread
               )
    #df$spreadsd<-c(rep(NA, sdPeriod-1),rollapply(df$spread,sdPeriod, sd, na.rm=TRUE))
    df
}



