#Orderbook Research VOL DELTA

#Load libraries
library(dplyr)
library(tidyr)
library(xts)
library(zoo)
library(ggplot2)
library(scales)
library(animation)
library(rattle)
library(caret)
source('G:/TRADE/R/rltrading/sandBox/orderbookggplot.R', echo=TRUE)
source('G:/TRADE/R/rltrading/sandBox/orderbookanimation.R', echo=TRUE)

options(digits.secs=3)


#' Идея: 
#' Исходные данные:
#' Цена с объемом сделок и 6 уровней журнала лимитных заявок с привязкой по времени.
#' 
#' 
#' Вопросы:
#' 1. Определить возможность определение направления цены 
#' в зависимости от изменения разницы объемов ask / bid
#' 2. Определить пороговые значения
#'  - voldelta
#'  - bidaskspread 
#'  - pricelag горизонт прогноза
#' 

load("tickorderbookSI2804.RData")
#df<-data.frame(datetime=as.POSIXct(index(datats)),as.data.frame(datats), check.names = FALSE)
#rm(datats)
if(!is.numeric(df[,2]))
    for(i in 2 :ncol(df))
        df[,i]<-as.numeric(levels(df[,i]))[df[,i]]
names(df)
#' "datetime"   "price"      "volume"     "bidprice0"  "bidvolume0" "askprice0"  "askvolume0"
#' "bidprice1"  "bidvolume1" "askprice1"  "askvolume1" "bidprice2"  "bidvolume2" "askprice2" 
#' "askvolume2" "bidprice3"  "bidvolume3" "askprice3"  "askvolume3" "bidprice4"  "bidvolume4"
#' "askprice4"  "askvolume4" "bidprice5"  "bidvolume5" "askprice5"  "askvolume5"

# Filter Data
dfdate<-format(df$datetime[1], "%Y-%m-%d")
downlimit<-as.POSIXct(paste(dfdate,"10:05:00.000"))
uplimit<-as.POSIXct(paste(dfdate,"18:00:00.000"))

df<-df %>% 
    filter(datetime>downlimit & datetime<uplimit)#%>%
    #filter(price>=bidprice5 & price<= askprice5)

#orderbookggplot(df)

orderDensity<-as.data.frame(table(format(df$datetime, "%Y-%m-%d %H%M%S")))
orderPerSec<-mean(orderDensity$Freq)

pricelag = round(10*orderPerSec)
timelag<-function(dflag, timelag=0.5){
    #for(i in 1:nrow(dflag))
    #    dflag$pricenext[i]<-filter(dflag, datetime>=dflag$datetime[i]+timelag)$price[1]
    apply(dflag[,c("datetime","price")], 1, 
          function(x)filter(dflag, datetime>=as.POSIXct(x[1])+timelag)$price[1])
}


timelagCycle<-function(dflag, timelag=0.5){
    for(i in 1:nrow(dflag))
        dflag$pricenext[i]<-filter(dflag, datetime>=dflag$datetime[i]+timelag)$price[1]
    dflag$pricenext
}

timelagCycleNF<-function(dflag, timelag=0.5){
    for(i in 1:nrow(dflag))
        dflag$pricenext[i]<-first(dflag[dflag$datetime>=dflag$datetime[i]+timelag,"price"])
    dflag$pricenext
}

#' Price of Next order. We will calculate outcomes basing on next price
df$pricenext<-timelag(df)

#df$direction<-factor(sign(df$pricenext-df$price),levels=c(-1,0,1),labels=c("down", "flat","up"))
#table(df$direction)

############## WITH DATA TABLE #################################
df<-data.table(df)

df[,pricenext:=shift(price,10, type="lag")]

df[,bidaskspread:=askprice0-bidprice0]
df[,pricediff:=pricenext-price]
df[,direction:=factor(sign(pricediff),levels=c(-1,0,1),labels=c("down", "flat","up"))]
df[,voldelta:=askvolume0-bidvolume0]
df[,logvoldelta:=log(askvolume0)-log(bidvolume0)]
table(df$direction)

qplot(pricediff,voldelta,color=direction,
      data=df) 


bidaskspreadTreshold<-80
voldeltaTreshold<-50
pricelag<-100


qplot(voldelta,color=direction,fill=direction, stat="bin",binwidth = 1,
      facets = bidaskspread~direction,
      data=df[complete.cases(df),][bidaskspread<=bidaskspreadTreshold,])+
  geom_vline(xintercept = voldeltaTreshold,alpha =I(0.7))+
  geom_vline(xintercept = -voldeltaTreshold,alpha =I(0.7))+
  ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                paste("VolDelta threshold",voldeltaTreshold, sep=" : "),sep="\n"))


qplot(pricediff,color=direction,fill=direction, stat="bin",binwidth = 1,
      facets = bidaskspread~direction,
      data=df[complete.cases(df),])+
  ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                paste("Price Lag",pricelag, sep=" : "),
                paste("BidAskSpread threshold",bidaskspreadTreshold, sep=" : "),
                sep="\n"))

qplot(voldelta,color=direction,fill=direction, alpha=I(0.7), stat="bin",
      facets = bidaskspread~direction,
      binwidth = 10,
      data=filter(df, bidaskspread<bidaskspreadTreshold, abs(voldelta)>voldeltaTreshold, abs(voldelta)<500))+
  geom_vline(xintercept = voldeltaTreshold,alpha =I(0.7))+
  geom_vline(xintercept = -voldeltaTreshold,alpha =I(0.7))+
  ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                paste("VolDelta threshold",voldeltaTreshold, sep=" : "),
                paste("BidAskSpread threshold",bidaskspreadTreshold, sep=" : "),
                paste("Price Lag",pricelag, sep=" : "),
                sep="\n"))

qplot(pricediff,color=direction,fill=direction, alpha=I(0.7), stat="bin",
      data=df[complete.cases(df),])+
  ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                paste("VolDelta threshold",voldeltaTreshold, sep=" : "),
                paste("BidAskSpread threshold",bidaskspreadTreshold, sep=" : "),
                sep="\n"))

#Accuracy Tests
voldeltaMin<-0
voldeltaMax<-500
voldeltaStep<-500
bidaskspreadMax<-70
bidaskspreadMin<-10

pricelagMin <- 10
pricelagMax <- 1000
pricelagStep<-10
pred<-"up"


checkAccuracy<-function(predValue, bidaskspreadMin,bidaskspreadMax,voldeltaLow,voldeltaUp){
  dfVolDelta<-df[bidaskspread>=bidaskspreadMin]
  if(nrow(dfVolDelta)==0){
    return (0)
  } 
  
  else{
    dfVolDelta$pred<-predValue
    dfVolDelta$pred<-factor(dfVolDelta$pred, levels=c("down","flat", "up"))
    confusionMatrix(data=dfVolDelta$pred, dfVolDelta$direction)  
    
  }
  
}


dfData<-df
resDf<-data.table(PriceLag=NA,Prediction=NA,VolDeltaDown=NA, VolDeltaUp=NA, Accuracy=NA, Observations=NA)

for(prl in seq(pricelagMin, pricelagMax, pricelagStep)){
  
  df[,pricenext:=shift(price,prl, type="lag")]
  df[,pricediff:=pricenext-price]
  df[,direction:=factor(sign(pricediff),levels=c(-1,0,1),labels=c("down", "flat","up"))]
  df<-df[complete.cases(df),]
  
      pred="down"
      resRow<-checkAccuracy(predValue="down",bidaskspreadMin, bidaskspreadMax, voldeltaLow, voldeltaMax)
      if(is.list(resRow)){
        resRow<-data.table(prl,pred,vdL, vdU, as.numeric(resRow$overall["Accuracy"]), sum(resRow$table))
        resDf<-rbind(resDf, resRow, use.names=FALSE)
        print(tail(resDf,1))
      }
      #print(resRow)
      pred="up"
      resRow<-checkAccuracy(predValue=pred,bidaskspreadMin, bidaskspreadMax, -voldeltaMax,-voldeltaLow)
      if(is.list(resRow)){
        resRow<-data.table(prl,pred,-vdL, -vdU, as.numeric(resRow$overall["Accuracy"]), sum(resRow$table))
        resDf<-rbind(resDf, resRow, use.names=FALSE)
        print(tail(resDf,1))
      }

}

colnames(resDf)<-c("PriceLag","Prediction","VolDeltaDown", "VolDeltaUp", "Accuracy", "Observations")

resDfClean<-resDf[complete.cases(resDf),]
resDfClean<-mutate(resDfClean,
                   PriceLag=as.numeric(PriceLag),
                   Prediction=factor(Prediction, levels=c("down", "flat","up"), labels=c("down", "flat","up")),
                   VolDeltaDown=as.numeric(VolDeltaDown),
                   VolDeltaUp=as.numeric(VolDeltaUp),
                   Accuracy=as.numeric(Accuracy),
                   Observations=as.numeric(Observations))

qplot(PriceLag, Accuracy, color = Prediction,
      #facets=Prediction~.,
      data= resDfClean, geom=c("point", "line"))





###################################################


df<-df %>%
    mutate(
        bidaskspread=askprice0-bidprice0,
        pricediff=pricenext-price,
        direction=factor(sign(pricediff),levels=c(-1,0,1),labels=c("down", "flat","up")),
        #' Level ask/bid volume delata
        voldelta=askvolume0-bidvolume0,
        logvoldelta=log(askvolume0)-log(bidvolume0)
    
    )%>%
    filter(bidaskspread>0)
table(df$direction)

# Selecting Features and Filtering data
df<-df %>%
    dplyr::select(datetime, 
           price,
           pricediff,
           direction,
           bidaskspread,
           voldelta,
           logvoldelta
    )
    

qplot(pricediff,voldelta,color=direction,
      data=df)      

#'TRESHOLDS:
bidaskspreadTreshold<-80
voldeltaTreshold<-50
pricelag<-100
df$pricenext<-lag(df$price, pricelag)
df<-df %>%
    mutate(
        pricediff=pricenext-price,
        direction=factor(sign(pricediff),levels=c(-1,0,1),labels=c("down", "flat","up"))

    )
    
df<-df[complete.cases(df),]

qplot(voldelta,color=direction,fill=direction, stat="bin",binwidth = 1,
      facets = bidaskspread~direction,
      data=filter(df, bidaskspread<bidaskspreadTreshold, 
                  abs(voldelta)>voldeltaTreshold, abs(voldelta)<500))+
    geom_vline(xintercept = voldeltaTreshold,alpha =I(0.7))+
    geom_vline(xintercept = -voldeltaTreshold,alpha =I(0.7))+
    ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                  paste("VolDelta threshold",voldeltaTreshold, sep=" : "),sep="\n"))


qplot(pricediff,color=direction,fill=direction, stat="bin",binwidth = 1,
      facets = bidaskspread~direction,
      data=filter(df, bidaskspread<=bidaskspreadTreshold))+
    ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                  paste("Price Lag",pricelag, sep=" : "),
                  paste("BidAskSpread threshold",bidaskspreadTreshold, sep=" : "),
                  sep="\n"))


qplot(voldelta,color=direction,fill=direction, alpha=I(0.7), stat="bin",
      facets = bidaskspread~direction,
      binwidth = 10,
      data=filter(df, bidaskspread<bidaskspreadTreshold, abs(voldelta)>voldeltaTreshold, abs(voldelta)<500))+
    geom_vline(xintercept = voldeltaTreshold,alpha =I(0.7))+
    geom_vline(xintercept = -voldeltaTreshold,alpha =I(0.7))+
    ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                  paste("VolDelta threshold",voldeltaTreshold, sep=" : "),
                  paste("BidAskSpread threshold",bidaskspreadTreshold, sep=" : "),
                  paste("Price Lag",pricelag, sep=" : "),
                  sep="\n"))

qplot(pricediff,color=direction,fill=direction, alpha=I(0.7), stat="bin",
      data=filter(df, bidaskspread<bidaskspreadTreshold, abs(voldelta)>voldeltaTreshold, abs(voldelta)<500))+
    ggtitle(paste(paste(min(df$datetime), max(df$datetime), sep=" :: "),
                  paste("VolDelta threshold",voldeltaTreshold, sep=" : "),
                  paste("BidAskSpread threshold",bidaskspreadTreshold, sep=" : "),
                  sep="\n"))
#Animation
oopt <- animation::ani.options(interval = 0.1,
                               convert = 'c:/PROGRA~1/ImageMagick-6.9.0-Q16/convert.exe')

#' Volume delat Histogramm animation:
#' voldelta 0 to 400 step 20
#' bidaskspreadLimit =10
#' priceLag=50
#' voldeltaUp=500
#'
saveGIF(histVolDelta(obdf=df, i=20,threshold=500), 
        interval = 0.5,
        movie.name = "binVolDelta.gif", ani.width = 800, ani.height = 800)

#' Price lag Histogramm animation
#' pricelag  0 to 200 step 10
#' voldeltaLow = 50
#' bidaskspreadLimit =10
#' 
saveGIF(histPriceLag(obdf=df, i=10,threshold=200), 
        interval = 0.5,
        movie.name = "binPriceLag.gif", ani.width = 800, ani.height = 800)




#Animation
saveGIF(orderbookAll(datadf=df,from=as.POSIXct("2015-04-16 19:05:00.001"),
                     to=as.POSIXct("2015-04-16 19:05:20.001"),i=0.1),
        interval = 0.2,
        movie.name = "orderbook1604.gif", ani.width = 800, ani.height = 400)

#' Here we calculate average qty of ticks per second
#' 
df$timeF<-format(df$datetime,"%H%M%S")
freqtime<-as.data.frame(table(df$timeF))
summary(freqtime$Freq)
#' Resulst 8-12 tick per second.

#' Hypothesis testing
#'  
bidaskspreadTreshold<-10
voldeltaTreshold<-60
    

dfVolDelta<-filter(df, bidaskspread<=bidaskspreadTreshold, 
                   abs(voldelta)>=voldeltaTreshold & abs(voldelta)<=500)

qplot(direction,voldelta, color=direction,fill=direction, 
      geom=c("boxplot", "violin"), 
      #facets = bidaskspread~direction,
      alpha=I(0.5),
      data=dfVolDelta)


#' HO - if Voldelta>100 and <200 then direction is down
#' 
voldeltaMin<-5
voldeltaMax<-500
voldeltaStep<-10
bidaskspreadMax<-5
bidaskspreadMin<-4

pricelagMin <- 5
pricelagMax <- 500
pricelagStep<-5
pred<-"up"

#' Now we are going to find best voldeltaLow, voldeltaUP for 
#' gixed bidaskspreadTreshold and pricegap pricelag
dfData<-df
resDf<-data.frame()
resDf<-rbind(resDf,rep(NA, 6))
colnames(resDf)<-c("PriceLag","Prediction","VolDeltaDown", "VolDeltaUp", "Accuracy", "Observations")

for(prl in seq(pricelagMin, pricelagMax, pricelagStep)){
    dfData$pricenext<-lag(dfData$price, prl)
    df<-mutate(dfData,
               pricediff=pricenext-price,
               direction=factor(sign(pricediff),levels=c(-1,0,1),
                                labels=c("down", "flat","up")))
    df<-df[complete.cases(df),]
    
    for(vdL in seq(voldeltaMin, voldeltaMax, voldeltaStep)){
        for(vdU in seq(voldeltaMax, voldeltaMax, voldeltaStep)){
            pred="down"
            resRow<-checkAccuracy(predValue="down",bidaskspreadMin, bidaskspreadMax, vdL, vdU)
            if(is.list(resRow)){
                resRow<-c(prl,pred,vdL, vdU, as.numeric(resRow$overall["Accuracy"]), sum(resRow$table))
                resDf<-rbind(resDf, resRow)
                print(tail(resDf,1))
            }
            #print(resRow)
            pred="up"
            resRow<-checkAccuracy(predValue=pred,bidaskspreadMin, bidaskspreadMax, -vdU,-vdL)
            if(is.list(resRow)){
                resRow<-c(prl,pred,-vdL, -vdU, as.numeric(resRow$overall["Accuracy"]), sum(resRow$table))
                resDf<-rbind(resDf, resRow)
                print(tail(resDf,1))
            }
            
            
        }
    }
}

colnames(resDf)<-c("PriceLag","Prediction","VolDeltaDown", "VolDeltaUp", "Accuracy", "Observations")

resDfClean<-resDf[complete.cases(resDf),]
resDfClean<-mutate(resDfClean,
                   PriceLag=as.numeric(PriceLag),
                   Prediction=factor(Prediction, levels=c("down", "flat","up"), labels=c("down", "flat","up")),
                   VolDeltaDown=as.numeric(VolDeltaDown),
                   VolDeltaUp=as.numeric(VolDeltaUp),
                   Accuracy=as.numeric(Accuracy),
                   Observations=as.numeric(Observations))

qplot(PriceLag, Accuracy, color = Prediction,
      #facets=Prediction~.,
      data= filter(resDfClean,abs(VolDeltaDown)>=100&abs(VolDeltaDown)<=150), geom=c("point", "line"))

qplot(VolDeltaDown,VolDeltaUp,color=Accuracy, size= Observations, 
      data=resDfClean,  facets = Prediction~Accuracy)

resDFUp<-resDfClean%>% filter(Prediction=="up",
                              Accuracy>=0.85,
                              Observations>=10
                             ) %>%
    arrange(Accuracy,PriceLag, Observations)

qplot(VolDeltaDown,PriceLag,size=Accuracy, color= Accuracy, data=resDFUp)


resDFDown<-resDfClean%>% filter(Prediction=="down",
                           Accuracy>=0.85,
                           Observations>10
                           ) %>%
    arrange(Accuracy,PriceLag , Observations)

qplot(VolDeltaDown,VolDeltaUp,size=Accuracy, color= Observations, data=resDFDown)


checkAccuracy<-function(predValue, bidaskspreadMin,bidaskspreadMax,voldeltaLow,voldeltaUp){
    dfVolDelta<-filter(df, bidaskspread>=bidaskspreadMin & bidaskspread<=bidaskspreadMax,
                       voldelta>=voldeltaLow & voldelta<=voldeltaUp)
    if(nrow(dfVolDelta)==0){
        return (0)
    } 
        
    else{
        dfVolDelta$pred<-predValue
        dfVolDelta$pred<-factor(dfVolDelta$pred, levels=c("down","flat", "up"))
        confusionMatrix(data=dfVolDelta$pred, dfVolDelta$direction)  
        
    }
    
}

dfConf<-filter(df, bidaskspread>=3 & bidaskspread<=5,
               voldelta>=100 & voldelta<=1000)
dfConf$pred<-"down"
dfConf$pred<-factor(dfConf$pred, levels=c("down","flat", "up"))
confusionMatrix(data=dfConf$pred,
                dfConf$direction)


#' Machine Learning
# Create training and test sets

dfVolDelta<-df
dfVolDelta<-dfVolDelta[complete.cases(dfVolDelta),]
inTrain <- createDataPartition(dfVolDelta$direction,  p=0.7, list=FALSE)
training<-dfVolDelta[inTrain,c(-1,-2, -3)]
testing<-dfVolDelta[-inTrain,c(-1,-2, -3)]

dim(training); dim(testing)


#reduce the number of features with Dimensionality reduction procedure:
preproc <- preProcess(dfVolDelta[,c(-1,-2, -3,-4)], method='pca', thresh=0.99)

training.pca <- predict(preproc, training[,-1])     
testing.pca <- predict(preproc, testing[,-1])     


# Model rpart Prepr
modFit <- train(direction ~ bidaskspread+logvoldelta,method="rpart",data=training)
modFitPCA <- train(training$direction ~ .,method="rpart",data=training.pca)

#print(modFit$finalModel)
modFit <- train(direction ~ .,method="gbm",data=training, verbose=FALSE)


# Plot model

fancyRpartPlot(modFit$finalModel)
pred=predict(modFit,newdata=testing)
predPCA=predict(modFitPCA,newdata=testing.pca)

confusionMatrix(data=pred, testing$direction)
confusionMatrix(data=predPCA, testing$direction)

