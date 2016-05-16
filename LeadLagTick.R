# Lead Lag research by tick Data

library(data.table)
library(xts)
library(yuima)
options(digits.secs=3)


 from<-as.Date("2016-02-02")
 to<-as.Date("2016-02-02")
 symb1<-"SiH6"
 symb2<-"RIH6"
 
 symbDT<-getSymbol.MOEX(from, to,c(symb1,symb2))
 setkey(symbDT,dat_time)
 symbDT[code==symb2,logRet:=log(shift(price,15,type="lead")/price)]
 symbDT[code==symb1,logRet:=log(shift(price,15,type="lead")/price)]
 symbDT<-symbDT[as.numeric(format(dat_time,"%H")) %in% 11:16]
 
 
 
 symb1.zoo<-xts(x=symbDT[code==symb1,.(logRet)], order.by=symbDT[code==symb1,dat_time])
 symb2.zoo<-xts(x=symbDT[code==symb2,.(logRet)], order.by=symbDT[code==symb2,dat_time])
 
 symbDT<-cbind(symb1.zoo,symb2.zoo)
 symbDT<-symbDT[complete.cases(symbDT)]
 head(symbDT)
 
 yuima <- setData(list( symbDT$logRet, symbDT$logRet.1))
 #yuima <- setData(list( symb1.zoo, symb2.zoo))
 
 G <- seq(-0.1, 0.1, by = 1/1000)
 est <- llag(yuima, grid = G, ci = TRUE)
 ## The shape of the plotted cross-correlation is evidently bimodal,
 ## so there are likely two lead-lag parameters
 
 ## Lead-lag estimation by mllag
 mllag(est) # succeeds in detecting two lead-lag parameters
 mllag(est, alpha = 0.001)
 
 