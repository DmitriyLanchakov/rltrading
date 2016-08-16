library(quantmod)
library(caret)
library(data.table)
# Load Dataset
options(digits.secs=3)
fname<-"g:/TRADE/Data/research/"
setwd(fname)
#fname<-c("dfplazaSBER3007.RData")
fname<-c("dfplazaSBRF0308.RData")
#fname<-c("tickorderbookSI24062015.RData")
#fname<-c("tickorderbookSI24062015XPL.RData")

load(fname) 
#df<-data.table(df)
df<-dfplaza
rm(dfplaza)
dfdate<-format(df$datetime[2], "%Y-%m-%d")
downlimit<-as.POSIXct(paste(dfdate,"10:10:00.000"))
uplimit<-as.POSIXct(paste(dfdate,"18:00:00.000"))
df[,deltaS:=askprice0-bidprice0]
#df[,deltaVol:=log(askvolume0)-log(bidvolume0)]

depthago=4
depthorderbook=2
depthforward=50
threshold=0

df[,pricemid:=(askprice0+bidprice0)/2]
df<-df[datetime>downlimit & datetime<uplimit]


# Volume with signature for the last 4 deals
df[,paste("signedvol", depthago:1, sep=""):=
       lapply(depthago:1,FUN=function(x) shift(volume,x)*ifelse(shift(buysell,x)=="Buy",1, -1))]

# bid volume change for rows from 0 to 2
df[,paste("bidvolgap",depthorderbook:0,sep=""):=
       lapply(depthorderbook:0, 
              FUN=function(x) log(get(paste("bidvolume",x,sep="")))-log(shift(get(paste("bidvolume",x,sep="")),1)))]

# ask volume change for rows from 0 to 2
df[,paste("askvolgap",depthorderbook:0,sep=""):=
       lapply(depthorderbook:0, 
              FUN=function(x) log(get(paste("askvolume",x,sep="")))-log(shift(get(paste("askvolume",x,sep="")),1)))]

# bid price change for rows from 0 to 2
df[,paste("bidpricegap",depthorderbook:0,sep=""):=
       lapply(depthorderbook:0, 
              FUN=function(x) log(get(paste("bidprice",x,sep="")))-log(shift(get(paste("bidprice",x,sep="")),1)))]

# ask price change for rows from 0 to 2
df[,paste("askpricegap",depthorderbook:0,sep=""):=
       lapply(depthorderbook:0, 
              FUN=function(x) log(get(paste("askprice",x,sep="")))-log(shift(get(paste("askprice",x,sep="")),1)))]



# bidask volume change for rows from 0 to 2
df[,paste("bidaskvoldelta",depthorderbook:0,sep=""):=
       lapply(depthorderbook:0, 
              FUN=function(x) log(get(paste("bidvolume",x,sep="")))-log(get(paste("askvolume",x,sep=""))))]

# Market order at ask side
df[,paste("AMO", depthago:0, sep=""):=
       lapply(depthago:0,FUN=function(x) ifelse(shift(price,x)>=shift(askprice0,x),1, 0))]

# Market order at bid side
df[,paste("BMO", depthago:0, sep=""):=
       lapply(depthago:0,FUN=function(x) ifelse(shift(price,x)<=shift(bidprice0,x),1, 0))]

# Market order through ask side
df[,paste("ATT", depthago:1, sep=""):=
       lapply(depthago:1,FUN=function(x) ifelse(price>shift(askprice0,x),1, 0))]

# Market order through bid side
df[,paste("BTT", depthago:1, sep=""):=
       lapply(depthago:1,FUN=function(x) ifelse(price<shift(bidprice0,x),1, 0))]

# ROC fast
df[,"fastROC":=log(pricemid)- log(shift(pricemid,depthago))]

# ROC slow
df[,"slowROC":=log(pricemid)- log(shift(pricemid,depthago*10))]

# Fast > Slow
#df[,"fupsROC":=factor(sign(fastROC-slowROC),levels=c(-1,0,1),labels=c("down", "flat", "up"))]
#df[,omega:=ifelse(dc>0 & vol1<=2,1,0)]

# prediction variable
df[,yvalues:=shift(pricemid, depthforward,type="lead")-pricemid]
df[,direction:=factor(sign(yvalues),levels=c(-1,1),labels=c("down", "up"))]
#df[,direction:=factor(yvalues>threshold,levels=c(TRUE,FALSE),labels=c("up", "no"))]

#df[,direction:=factor(as.numeric(abs(yvalues)>threshold)*sign(yvalues),levels=c(-1,1),labels=c("down", "up"))]


df<-df[complete.cases(df),]
#confusionMatrix(data=df[,fupsROC], df[,direction])

features<-c("direction",
            #paste("signedvol", depthago:1, sep=""),
            paste("askvolgap",depthorderbook:0,sep=""),
            paste("bidvolgap",depthorderbook:0,sep=""),
            paste("askpricegap",depthorderbook:0,sep=""),
            paste("bidpricegap",depthorderbook:0,sep=""),
            paste("bidaskvoldelta",depthorderbook:0,sep=""),
            "fastROC",
            "slowROC",
            paste("AMO", depthago:0, sep=""),
            paste("BMO", depthago:0, sep=""),
            paste("BTT", depthago:1, sep=""),
            paste("ATT", depthago:1, sep=""))



#' Machine Learning
# Create training and test sets
#df<-df[1:300000]
inTrain <- as.numeric(createDataPartition(df[,direction],  p=0.7, list=FALSE))
training<-df[inTrain, .SD, .SDcols=features]
testing<-df[-inTrain, .SD, .SDcols=features]

dim(training); dim(testing)


# Model GBM
modFitGBM <- train(direction ~ .,method="gbm",data=training, verbose=FALSE)
predGBM=predict(modFitGBM,newdata=testing)
confusionMatrix(data=predGBM, testing[,direction])
modFitGBM$finalModel


# Model GLM
modFitGLM <- train(direction ~ .,method="glm",data=training)
predGLM=predict(modFitGLM,newdata=testing)
confusionMatrix(data=predGLM, testing[,direction])
summary(modFitGLM)

# Model RPART
#modFitPCA <- train(training$direction ~ .,method="rpart",data=training.pca)
modFitRPART <- train(direction ~ .,method="rpart",data=training)
predRPART=predict(modFitRPART,newdata=testing)
confusionMatrix(data=predRPART, testing[,direction])
print(modFitRPART$finalModel)

# #Model RPART with preprocessing
# #reduce the number of features with Dimensionality reduction procedure:
# preproc <- preProcess(df[,.SD, .SDcols=features[-1]], method='pca', thresh=0.99)
# training.pca <- predict(preproc, training[,-1, with=FALSE])
# testing.pca <- predict(preproc, testing[,-1, with=FALSE])
# modFitRPART <- train(training[,direction] ~ .,method="rpart",data=training.pca)
# predRPART=predict(modFitRPART,newdata=testing.pca)
# confusionMatrix(data=predRPART, testing[,direction])
# print(modFitRPART$finalModel)
# 


# Benchmark
library(microbenchmark)
microbenchmark(predict(modFitGBM,newdata = testing[1000,]),
               predict(modFitGLM,newdata = testing[1000,])
               )
