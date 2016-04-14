#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Статистика арбитража Ри против синтетики миниМИКС + Си 
#' #' 2016-04-06 | rlukerin@gmail.com
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(data.table)
library(ggplot2)
options(digits.secs=3)

setwd("~/repos/SAZAN/ru.sazan.arbitrage/ru.sazan.arbitrage/bin/Debug/")
DT<-read.csv("2016-04-13-0.log", head=F, stringsAsFactors = F, col.names=1:5)
DT<-data.table(DT)
DT<-DT[X2==" Spread Bid/Ask"]
DT[,X3:=as.numeric(X3)]
DT[,X4:=as.numeric(X4)]
DT<-DT[X3< -20 & X4< -20]
DT<-DT[X3> -500 & X4> -500]
summary(DT[,.(X3, X4)])

DT[,X1:=as.POSIXct(strptime(X1,format="%d-%m-%Y %H:%M:%OS"))]

ggplot()+
  #geom_point(data=DT[X3>= -260],aes(x=X1, y=X3), color="darkgreen")+
  #geom_point(data=DT[X4<= -25],aes(x=X1, y=X4), color="darkred")+ scale_y_continuous(breaks=seq(-400,0,10))
 geom_point(data=DT,aes(x=X1, y=X3), color="darkgreen")+
 geom_point(data=DT,aes(x=X1, y=X4), color="darkred")+ 
 scale_y_continuous(breaks=seq(-400,0,10))+
 geom_hline(yintercept = quantile(DT[,X3])[2])+
 geom_hline(yintercept = quantile(DT[,X4])[2])
  
ggplot()+
  #geom_point(data=DT[X3>= -260],aes(x=X1, y=X3), color="darkgreen")+
  #geom_point(data=DT[X4<= -25],aes(x=X1, y=X4), color="darkred")+ scale_y_continuous(breaks=seq(-400,0,10))
  geom_density(data=DT,aes(X3), color="darkgreen")+
  geom_density(data=DT,aes(X4), color="darkred")+
  scale_x_continuous(breaks=seq(-400,0,10))+ 
  geom_vline(xintercept = quantile(DT[,X3])[2],color="darkgreen")+
  geom_vline(xintercept = quantile(DT[,X4])[2],color="darkred")
  