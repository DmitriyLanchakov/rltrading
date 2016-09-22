#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Зависимость между ценами  на нефть и доллар/рубль
#' 
#' 2016-04-07 | rlukerin@gmail.com
#' 
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(rusquant)
library(ggplot2)
require(scales)
library(data.table)
symbols<-c("@Si",
           "@BR", 
           "@RI"
)

from<-"2013-09-01"
to<-Sys.Date()
period<-"day"
for(s in symbols)
    getSymbols(s, from=from, to=to, period=period, src='mfd',adjust=TRUE, auto.assign=TRUE)


SI<-data.table(date=index(`@SI`),`@SI`)
SI[, step:=1]
SI[, pricestep:=1]
SI[, BaseFutFee:=0.14]
SI[, oldcomis:=0.5]
SI[, comis:=BaseFutFee*Close*(pricestep/step)*0.01/100]


BR<-data.table(date=index(`@BR`),`@BR`)
BR[, si:=SI$Close]
BR[, step:=0.01]
BR[, pricestep:=si/10000]
BR[, BaseFutFee:=0.4]
BR[, oldcomis:=1]
BR[, comis:=BaseFutFee*Close*(pricestep/step)*0.01/100]


RI<-data.table(date=index(`@RI`),`@RI`)
RI[, si:=SI$Close]
RI[, step:=10]
RI[, pricestep:=si/(5*1000)]
RI[, BaseFutFee:=0.2]
RI[, oldcomis:=2]
RI[, comis:=BaseFutFee*Close*(pricestep/step)*0.01/100]



ggplot()+geom_point(data=RI,aes(date, comis),color="red")+
    geom_line(data=RI, aes(date, oldcomis), color="red")+
    
    geom_point(data=SI,aes(date, comis),color="blue")+
    geom_line(data=SI, aes(date, oldcomis), color="blue")+
    
    geom_point(data=BR,aes(date, comis),color="black")+
    geom_line(data=BR, aes(date, oldcomis), color="black")+
    
    ggtitle("Старый / новый комис на MOEX \n точки- новый \n линия - старый \n BR - черный \n SI - синий \n RI - красный ")
    


ggplot()+geom_point(data=RI,aes(date, pricestep/comis),color="red")+
    geom_line(data=RI,aes(date, pricestep/oldcomis),color="red")+
    geom_point(data=SI,aes(date, pricestep/comis),color="blue")+
    geom_line(data=SI,aes(date, pricestep/oldcomis),color="blue")+
    geom_point(data=BR,aes(date, pricestep/comis),color="black")+
    geom_line(data=BR,aes(date, pricestep/oldcomis),color="black")+
    ggtitle("Стоимость пункта к комису \n точки- новый \n линия - старый \n BR - черный \n SI - синий \n RI - красный ")



#  Increase
ggplot()+geom_line(data=RI,aes(date, 100*(-1+comis/oldcomis)),color="red")+
    geom_line(data=SI,aes(date,  100*(-1+comis/oldcomis)),color="blue")+
    geom_line(data=BR,aes(date,  100*(-1+comis/oldcomis)),color="black")+
    ylab("newcomis/oldcomis")+
    scale_y_continuous(breaks = seq(-150,150, 10))+
    ggtitle("Процентное изменение биржевого сбора на срочной секции MOEX \n (ретроспектива) \n BR - черный \n SI - синий \n RI - красный ")





