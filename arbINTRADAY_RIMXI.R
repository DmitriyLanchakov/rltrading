#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Зависимость между ценами  на RI-MXI-SI
#'  INTRADAY
#' 2016-08-22 | rlukerin@gmail.com
#' 
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


library(data.table)
library(ggplot2)
from=as.Date("2016-03-24")
to=as.Date("2016-03-24")
symbList=c("RIM6", "MMM6", "SiM6")
symbPriceStep=c(12/10,10,1)
symbDT<-getSymbol.MOEX(from=from, to=to,symbList=symbList)

symbDT[code==symbList[1],price:=price*symbPriceStep[1]]
symbDT[code==symbList[2],price:=price*symbPriceStep[2]]
symbDT[code==symbList[3],price:=price*symbPriceStep[3]]

setkey(symbDT, dat_time)


spreadDT<-symbDT[code==symbList[1]][symbDT[code==symbList[2]]][symbDT[code==symbList[3]], allow.cartesian=T][!is.na(code)][as.Date(dat_time)>=to]

ggplot(spreadDT)+
    geom_point(aes(x=price, y=i.price+i.price.1, color=factor(format(dat_time, "%H"))), alpha=0.5)


spreadDT[,retSi:=log(price)-log(shift(price, n=100, type="lag"))]
spreadDT[,retBR:=log(i.price)-log(shift(i.price, n=100, type="lag"))]

ggplot(spreadDT)+
    geom_point(aes(x=retSi, y=retBR,color=factor(format(dat_time, "%H"))), alpha=0.5)


#lm(retSi~retBR-1, data = spreadDT)$coefficients

spreadDT[,lm(retSi~retBR-1, data = .SD)$coefficients, by=format(dat_time,"%H")]
spreadDT[,(lm(price~i.price+i.price.1-1, data = .SD)$coefficients), by=format(dat_time,"%H")]



absCoef<-lm(price~i.price+i.price.1-1, data = spreadDT)$coefficients
spreadDT[, spread:=price-(absCoef[1]*i.price+absCoef[2]*i.price.1)]

ggplot(spreadDT)+
    geom_point(aes(x=dat_time, y=spread))

summary(spreadDT$spread)
spreadDT[, spread:=log(price)-log(absCoef*i.price)]
summary(spreadDT$spread)



