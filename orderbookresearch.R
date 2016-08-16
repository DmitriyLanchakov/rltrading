#Orderbook Research

#Load libraries
library(dplyr)
library(tidyr)
library(xts)
library(zoo)
library(ggplot2)
library(scales)
library(caret)
source('G:/TRADE/R/rltrading/sandBox/orderbookggplot.R', echo=TRUE)
options(digits.secs=3)


#' Идея: 
#' Исходные данные:
#' Цена с объемом сделок и 6 уровней журнала лимитных заявок с привязкой по времени.
#' 
#' 
#' 
#' Вопросы:
#' 1. Определить Факторы влияющие на направление следующей сделки и их вес
#' 2. Прогнозирование скачка.
#' 


load("tickorderbook.RData")
df<-data.frame(datetime=as.POSIXct(index(datats)),as.data.frame(datats))
rm(datats)

names(df)
#' "datetime"   "price"      "volume"     "bidprice0"  "bidvolume0" "askprice0"  "askvolume0"
#' "bidprice1"  "bidvolume1" "askprice1"  "askvolume1" "bidprice2"  "bidvolume2" "askprice2" 
#' "askvolume2" "bidprice3"  "bidvolume3" "askprice3"  "askvolume3" "bidprice4"  "bidvolume4"
#' "askprice4"  "askvolume4" "bidprice5"  "bidvolume5" "askprice5"  "askvolume5"

# Day seesion
downlimit<-as.POSIXct("2015-02-18 10:30:00.000")
uplimit<-as.POSIXct("2015-02-18 15:00:00.000")
df<-df %>% 
    filter(datetime>downlimit & datetime<uplimit)%>%
    filter(price>=bidprice5 & price<= askprice5)
    
#orderbookggplot(df)

#' Cleaning Data
#' - add bidaskspread variabble
#' - add total ask volume
#' - add total bid volume
#' - add vol delta
#' - bid ask spread should be >= 0
#' 

#' Price of Next order. We will calculate outcomes basing on next price
df$pricenext<-lag(df$price, 50)
df$direction<-factor(sign(df$pricenext-df$price),levels=c(-1,0,1),labels=c("down", "flat","up"))
table(df$direction)

#' Define outcome variable which takes following values:
#' moa - market order close to ask side
#' mob - market order closed to bid side
#' al - order occured at ask level 0 price
#' bl - order occured at bid level 0 price
#' aj - price jump through ask side
#' bj - price jump through bid side
#' baj - price jump from bid side to ask side at next price
#' abj - price jump from ask side to bid side at next price

#df$y[with(df, pricenext>bidprice0 & pricenext<askprice0 & pricenext>=(bidprice0+askprice0)/2)]<-"moa"
#df$y[with(df, pricenext>bidprice0 & pricenext<askprice0 & pricenext<(bidprice0+askprice0)/2)]<-"mob"

#df$y[with(df, pricenext>bidprice0 & pricenext<askprice0 )]<-"mo"
# 
# df$y[with(df, pricenext>=askprice0 & pricenext<askprice1)]<-"al"
# df$y[with(df, pricenext<=bidprice0 & pricenext>bidprice1)]<-"bl"
# df$y[with(df, pricenext<=bidprice1)]<-"bj"
# df$y[with(df, pricenext>=askprice1)]<-"aj"
# df$y<-as.factor(df$y)

#df$y[with(df, pricenext<bidprice0)]<-"bj"
#df$y[with(df, pricenext>askprice0)]<-"aj"

df$jump[with(df, price<=bidprice0 & pricenext>=askprice0)]<-"baj"
df$jump[with(df, price>=askprice0 & pricenext<=bidprice0)]<-"abj"
df$jump[is.na(df$jump)]<-"nojump"
df$jump<-as.factor(df$jump)
table(df$jump)

#orderbookggplot(df)+geom_point(data=df,aes(x=datetime, y=price, shape=jump), size=I(3))
    


#df$y<-as.factor(df$y)
#table(df$y)

#df$y<-factor(c(0, sign(diff(df$pricenext))),levels=c(-1,0,1),labels=c("down", "flat","up")))

#' Define features
#' side  - Current order side which takes following values:
#' moa - market order close to ask side
#' mob - market order closed to bid side
#' al - order occured at ask level 0 price
#' bl - order occured at bid level 0 price
#' aj - price jump through ask side
#' bj - price jump through bid side
#df$side[with(df, price>bidprice0 & price<askprice0 & price>=(bidprice0+askprice0)/2)]<-"moa"
#df$side[with(df, price>bidprice0 & price<askprice0 & price<(bidprice0+askprice0)/2)]<-"mob"

df$side[with(df, price>bidprice0 & price<askprice0)]<-"mo"
# df$side[with(df, price>=askprice0 & price<askprice1)]<-"al"
# df$side[with(df, price<=bidprice0 & price>bidprice1)]<-"bl"
df$side[with(df, price<=bidprice0)]<-"bj"
df$side[with(df, price>=askprice0)]<-"aj"
df$side<-as.factor(df$side)
table(df$side)

#' order direction sign:
#' -1 - negative
#'  0 - netral
#'  1 - positive
df$ordersign<-factor(c(0, sign(diff(df$price))),levels=c(-1,0,1),labels=c("down", "flat","up"))
table(df$ordersign)

#' mean and sd for the last 100 ticks
df$MA100<-EMA(df$price, 100)
df$MA200<-EMA(df$price, 200)
df$cMA<-factor(sign(df$MA100-df$MA200),levels=c(-1,0,1),labels=c("fastlower", "cross","fastupper"))


df<-df %>%
    mutate(
        #' bidaskspread
        bidaskspread=askprice0-bidprice0,

#         bidgap0=bidprice0-bidprice1,
#         bidgap1=bidprice1-bidprice2,
#         bidgap2=bidprice2-bidprice3,
#         bidgap3=bidprice3-bidprice4,
#         bidgap4=bidprice4-bidprice5,
#         
#         askgap0=-askprice0+askprice1,
#         askgap1=-askprice1+askprice2,
#         askgap2=-askprice2+askprice3,
#         askgap3=-askprice3+askprice4,
#         askgap4=-askprice4+askprice5,
#         
#         #' Price gap - orderbook density
#         totalaskgap= askgap0+askgap1+askgap2+askgap3+askgap4,
#         totalbidgap= bidgap0+bidgap1+bidgap2+bidgap2+bidgap4,
#         
#         #' Orderside volumes
#         totalaskvol=askvolume0+askvolume1+askvolume2+askvolume3+askvolume4+askvolume5,
#         totalbidvol=bidvolume0+bidvolume1+bidvolume2+bidvolume3+bidvolume4+bidvolume5,
#         
#         #' Orderside amounts
#         totalbidamount=bidprice0*bidvolume0+bidprice1*bidvolume1,#+bidprice2*bidvolume2,
#         totalaskamount=askprice0*askvolume0+askprice1*askvolume1,#+askprice2*askvolume2,
        
        
        #' Level ask/bid volume delata
        voldelta=askvolume0-bidvolume0,
        logvoldelta=log(askvolume0)-log(bidvolume0)

        #' Total ask/bid volume delata
        #totalvoldelta=totalaskvol-totalbidvol
    
    )%>%
    filter(bidaskspread>0)%>%
    filter(side!="mo")

#' Ask and Bid price difference
df$bidaskspreaddiff=c(NA, diff(log(df$bidaskspread)))
df$askdiff=c(NA, diff(log(df$askprice0)))
df$biddiff=c(NA, diff(log(df$bidprice0)))

#' Total Ask and Bid Gap price difference
#df$totalaskgapdiff=c(NA, diff((df$totalaskgap)))
#df$totalbidgapdiff=c(NA, diff((df$totalbidgap)))


#' Total Ask and Bid Volume difference
#df$totalaskvoldiff=c(NA, diff((df$totalaskgap)))
#df$totalbidvoldiff=c(NA, diff((df$totalbidgap)))

#' Total Ask and Bid amount difference
#df$totalaskamountdiff=c(NA, diff((df$totalaskamount)))
#df$totalbidamountdiff=c(NA, diff((df$totalbidamount)))

#' Ask / Bid Volume  difference
df$voldeltadiff=c(NA, diff((df$voldelta)))
#df$totalvoldeltadiff=c(NA, diff((df$totalvoldelta)))

df$voldeltasign<-factor(sign(df$voldelta))
#df$totalvoldelta<-factor(sign(df$totalvoldelta))


# Selecting Feutures and Filtering data
df<-df %>%
    select(datetime, price, 
           jump, 
           #side, 
           #ordersign,
           bidaskspread,
           bidaskspreaddiff,
           #            totalaskgap,
           #            totalbidgap,
           #            totalaskvol,
           #            totalbidvol,
           #            totalaskamount,
           #            totalbidamount,
           #voldelta,
           logvoldelta,
           voldeltadiff,
           #           totalvoldelta,
           askdiff,
           biddiff,
           #           totalaskgapdiff,
           #           totalbidgapdiff,
           #           totalaskvoldiff,
           #           totalbidvoldiff,
           #           totalaskamountdiff,
           #           totalbidamountdiff,

           # MA100,
           # MA200,
           cMA
           
           #           totalvoldeltadiff
    )
    
    #select(-side)

# Create training and test sets
df<-df[complete.cases(df),]
inTrain <- createDataPartition(df$jump,  p=0.7, list=FALSE)
training<-df[inTrain,c(-1,-2)]
testing<-df[-inTrain,c(-1,-2)]

rm(inTrain)
dim(training); dim(testing)


# Model rpart
modFit <- train(jump ~ .,method="rpart",data=training)
#print(modFit$finalModel)

# Plot model
library(rattle)
fancyRpartPlot(modFit$finalModel)

pred=predict(modFit,newdata=testing)
confusionMatrix(data=pred, testing$jump)
#table(pred, testing$y)

# Random Forest
modFit <- train(jump ~ .,method="rf",data=training,prox=TRUE, ntree=100)
pred=predict(modFit,newdata=testing)
confusionMatrix(data=pred, testing$jump)


# test H voldelta
df<-df[complete.cases(df),]

qplot(voldelta,fill=direction,
      data=df)      


qplot(voldelta,color=direction,fill=direction, geom="density",
      facets = bidaskspread~direction,
      data=filter(df, bidaskspread<10, abs(voldelta)>50, abs(voldelta)<500))+
    geom_vline(xintercept = 50,alpha =I(0.7))+
    geom_vline(xintercept = -50,alpha =I(0.7))


qplot(voldelta,color=direction,fill=direction, alpha=I(0.7), geom="density",
      data=filter(df, bidaskspread<10, abs(voldelta)>50, abs(voldelta)<500))

#Animation
saveGIF(orderbookAll(from=as.POSIXct("2015-02-18 10:30:00.001"),
                           to=as.POSIXct("2015-02-18 10:30:40.001")), 
        interval = 0.1,
        movie.name = "orderbook.gif", ani.width = 800, ani.height = 400)

saveGIF(densityAll(data=df, from=downlimit, to=uplimit, i=20), 
        interval = 0.5,
        movie.name = "density.gif", ani.width = 800, ani.height = 800)
