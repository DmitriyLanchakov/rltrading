#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Зависимость между ценами  на нефть и доллар/рубль
#'  INTRADAY
#' 2016-08-22 | rlukerin@gmail.com
#' 
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


library(data.table)
library(ggplot2)
from=as.Date("2016-03-17")
to=as.Date("2016-03-17")
symbList=c("SiM6", "BRJ6")
symbPriceStep=c(1,6.5/0.01)
symbDT<-getSymbol.MOEX(from=from, to=to,symbList=symbList)

symbDT[code==symbList[1],price:=price*symbPriceStep[1]]
symbDT[code==symbList[2],price:=price*symbPriceStep[2]]

setkey(symbDT, dat_time)


spreadDT<-symbDT[code==symbList[1]][symbDT[code==symbList[2]]][!is.na(code)][as.Date(dat_time)>=to]

ggplot(spreadDT)+
    geom_point(aes(x=price, y=i.price, color=factor(format(dat_time, "%H"))), alpha=0.5)


spreadDT[,retSi:=log(price)-log(shift(price, n=100, type="lag"))]
spreadDT[,retBR:=log(i.price)-log(shift(i.price, n=100, type="lag"))]

ggplot(spreadDT)+
    geom_point(aes(x=retSi, y=retBR,color=factor(format(dat_time, "%H"))), alpha=0.5)


#lm(retSi~retBR-1, data = spreadDT)$coefficients

spreadDT[,lm(retSi~retBR-1, data = .SD)$coefficients, by=format(dat_time,"%H")]
spreadDT[,lm(price~i.price-1, data = .SD)$coefficients, by=format(dat_time,"%H")]

absCoef<-spreadDT[,lm(price~i.price-1, data = .SD)$coefficients, by=format(dat_time,"%H")][,mean(V1)]
spreadDT[, spread:=price-absCoef*i.price]

ggplot(spreadDT)+
    geom_point(aes(x=dat_time, y=spread))

summary(spreadDT$spread)
spreadDT[, spread:=log(price)-log(absCoef*i.price)]
summary(spreadDT$spread)



