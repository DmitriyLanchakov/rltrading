#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Зависимость между ценами  на нефть и доллар/рубль
#'  INTRADAY
#' 2016-08-22 | rlukerin@gmail.com
#' 
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


library(data.table)
library(ggplot2)
from=as.Date("2016-03-28")
to=as.Date("2016-03-28")
symbList=c("SiM6", "BRJ6")
symbPriceStep=c(1,1/0.01)
symbDT<-getSymbol.MOEX(from=from, to=to,symbList=symbList)

symbDT[code==symbList[1],price:=price*symbPriceStep[1]]
symbDT[code==symbList[2],price:=price*symbPriceStep[2]]

setkey(symbDT, dat_time)


spreadDT<-symbDT[code==symbList[1]][symbDT[code==symbList[2]]][!is.na(code)][as.Date(dat_time)>=to]

ggplot(spreadDT)+
    geom_point(aes(x=price, y=i.price, color=factor(format(dat_time, "%H"))), alpha=0.5)


spreadDT[,retSi:=log(price)-log(shift(price, n=1000, type="lag"))]
spreadDT[,retBR:=log(i.price)-log(shift(i.price, n=1000, type="lag"))]

ggplot(spreadDT)+
    geom_point(aes(x=retSi, y=retBR,color=factor(format(dat_time, "%H"))), alpha=0.5)

spreadDT[,movcor:=rollapply(cbind(price, i.price), 
                            width=10000, 
                            by=1,
                            by.column=FALSE,
                            FUN=function(xy){cor(xy[,1],xy[,2],use="complete.obs")})]

#lm(retSi~retBR-1, data = spreadDT)$coefficients

spreadDT[,lm(retSi~retBR-1, data = .SD)$coefficients, by=format(dat_time,"%H")]
spreadDT[,lm(price~i.price-1, data = .SD)$coefficients, by=format(dat_time,"%H")]

absCoef<-spreadDT[,lm(price~i.price-1, data = .SD)$coefficients, by=format(dat_time,"%H")][,mean(V1)]
#absCoef<-spreadDT[,lm(retSi~retBR-1, data = .SD)$coefficients, by=format(dat_time,"%H")][,mean(V1)]
spreadDT[, spread:=price-absCoef*i.price]

#spreadDT[, spread:=rollapply(data.table(price,i.price),width=10000, by=10000,FUN=function(xy){coef(lm(xy[,price]~xy[,i.price]-1))[1]})]
#spreadDT[,stdev:=rollapply(spread, width=10000, by=5000, FUN=sd)]
#spreadDT[,pricemean:=rollapply(spread, width=10000, by=50, FUN=mean)]



p0<-ggplot(spreadDT)+
    geom_line(aes(x=dat_time, y=price),colour="lightblue")+
    ylab("Si")

p1<-ggplot(spreadDT)+
    geom_line(aes(x=dat_time, y=i.price*0.01/6.5),colour="lightgreen")+
    ylab("Brent")


p2<-ggplot(spreadDT)+    
    geom_line(aes(x=dat_time, y=movcor),colour="lightcoral")+
    ylab("Moving Cor")
p3<-ggplot(spreadDT)+    
    geom_line(aes(x=dat_time, y=spread),colour="lightgrey")+
    ylab(paste("Abs Spread: coeff=",absCoef))

p4<-ggplot(spreadDT)+
    geom_line(aes(x=dat_time, y=((max(price)+min(price))-price)/max(price)),colour="lightblue")+
    geom_line(aes(x=dat_time, y=i.price/max(i.price)),colour="lightgreen")+
    ylab("Si (blue) and Brent (green)")

grid.arrange(p4, p2,p3,nrow=3 )

    

summary(spreadDT$spread)
spreadDT[, spread:=log(price)-log(absCoef*i.price)]
summary(spreadDT$spread)


# Считает спред по приращениям за 1с
spread1s<-spreadDT[,.(dat_time=dat_time[1],
                      priceSi=mean(price),
                      priceBr=mean(i.price)), by=format(dat_time,"%Y%m%d%H%M%S")]
spread1s[,retSi:=log(priceSi)-log(shift(priceSi, n=1, type="lag"))]
spread1s[,retBr:=log(priceBr)-log(shift(priceBr, n=1, type="lag"))]

#spread1s[,retSi:=priceSi-shift(priceSi, n=1, type="lag")]
#spread1s[,retBr:=priceBr-shift(priceBr, n=1, type="lag")]

ggplot(spread1s)+
    geom_point(aes(x=retSi, y=retBr))


betaRel<-coef(lm(retSi~retBr-1, data=spread1s))

spread1s[,spreadRel:=retSi-betaRel*retBr]

ggplot(spread1s)+
    geom_point(aes(x=dat_time, y=spreadRel))

spread1s[,summary(spreadRel)]

ggplot(spread1s)+
    geom_point(aes(x=dat_time, y=priceSi, color=spreadRel))+
    geom_line(aes(x=dat_time, y=priceSi, color=spreadRel, group=1))+
    scale_colour_gradient2(low="red",mid="lightgrey", high="blue")



spread1s[,movbetaRel:=rollapply(cbind(retSi, retBr), 
                            width=60, 
                            by=1,
                            by.column=FALSE,
                            FUN=function(xy)coef(lm(xy[,1]~xy[,2]-1)))]

ggplot(spread1s[100:2000])+
    geom_point(aes(x=dat_time, y=movbetaRel ))


ggplot(spread1s[100:2000])+
    geom_point(aes(x=dat_time, y=priceSi, color=mean(movbetaRel)-movbetaRel))+
    geom_line(aes(x=dat_time, y=priceSi, color=mean(movbetaRel)-movbetaRel, group=1))+
    scale_colour_gradient2(low="red",mid="lightgrey", high="blue")


spread1s[,summary(movbetaRel)]
