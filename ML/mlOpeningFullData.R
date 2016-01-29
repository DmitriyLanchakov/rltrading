library(caret)
library(data.table)
library(ggplot2)
fname<-"ML"
setwd(fname)

##
dt<-fread("Ri.txt", stringsAsFactors = FALSE)
dt[,DateTime:=paste(Date, Time)]
dt[, DateTime:=as.POSIXct(strptime(DateTime, format="%m/%d/%y %H%M"))]

#qplot(y=Close, x= DateTime, geom="line",data=dt)


daydt<-dt[,.(PriceMinDay=min(Close),
             PriceMaxDay=max(Close),
             PriceOpenDay=Open[1], 
             PriceCloseDay=Close[.N],
             TimeOpen=Time[1],
             CloseTimeOpen=Close[1],
             OpenTimeOpen=Open[1]),
          
          by=Date]

daydt[,':='(PriceMinPrevDay=shift(PriceMinDay,1, type="lag"),
            PriceMaxPrevDay=shift(PriceMaxDay,1, type="lag"),
            PriceOpenPrevDay=shift(PriceOpenDay,1, type="lag"),
            PriceClosePrevDay=shift(PriceCloseDay,1, type="lag"))]

setkey(daydt,Date)
setkey(dt,Date)
dt<-daydt[dt]
setkey(dt, DateTime)

dt[,PriceShift15:=log(shift(Close,15, type="lead")/Close)]
dt[,PriceGap1min:=log(Close/Open)]
dt[,PricGap1minToPrevDayClose:=log(Close,PriceClosePrevDay)]
dt[,PricGap1minToPrevDayOpen:=log(Close,PriceOpenPrevDay)]
dt[,PricGap1minToPrevDayMax:=log(Close,PriceMaxPrevDay)]
dt[,PricGap1minToPrevDayMin:=log(Close,PriceMinPrevDay)]

dt<-dt[Time>=1001 & Time<=1010]
dt[,direction:=factor(sign(PriceShift15),levels=c(-1,0,1),labels=c("down","flat", "up"))]
dt[,weekday:=weekdays(DateTime)]
dt[,monthday:=format(DateTime,"%d")]
dt[,month:=format(DateTime,"%m")]
features<-c("direction",
#             "weekday",
#             "monthday",
#             "month",
            "PriceGap1min",
            "PricGap1minToPrevDayClose",
            "PricGap1minToPrevDayOpen",
            "PricGap1minToPrevDayMax",
            "PricGap1minToPrevDayMin"
            )
dt<-dt[weekday!="Sunday" & weekday!="Saturday"]
dt<-dt[complete.cases(dt)]
processModFit<-function(yr){
    
    training<-dt[format(DateTime,"%Y")==yr-1, .SD, .SDcols=features]
    testing<-dt[format(DateTime,"%Y")==yr, .SD, .SDcols=features]
    testProfit<-dt[format(DateTime,"%Y")==yr]
    
    print(paste("Training:",yr-1, "/ Testing:", yr))
    
    # Center and Scale data
    preproc <- preProcess(dt[,.SD, .SDcols=features[-1]], method='pca', thresh=0.99)
    training.sc <- predict(preproc, training[,-1, with=FALSE])
    testing.sc <- predict(preproc, testing[,-1, with=FALSE])
#    dim(training.sc); dim(testing.sc)
    
    
    #' k-Nearest Neighbors
    modFit<-train(training[,direction] ~ .,method="knn",data=training.sc)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modKNN:=(as.numeric(modPred)-2)*abs(PriceShift15)]
    print("**k-Nearest Neighbors model**")
    print(Sys.time())
    print(confusionMatrix(data=modPred, testing[,direction]))
    gc()
#     
    #' Support Vector Machines with Radial Basis Function Kernel
    modFit<-train(training[,direction] ~ .,method="svmRadial",data=training.sc)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modsvmRadial:=(as.numeric(modPred)-2)*abs(PriceShift15)]
    print("**Support Vector Machines with Radial Basis Function Kernel**")
    print(Sys.time())
    print(confusionMatrix(data=modPred, testing[,direction]))
    gc()
#     
    #' Recursive Partitioning and Regression Trees
    modFit<-train(training[,direction] ~ .,method="rpart",data=training.sc)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modRpart:=(as.numeric(modPred)-2)*abs(PriceShift15)]
    print("**Recursive Partitioning and Regression Trees**")
    print(Sys.time())
    print(confusionMatrix(data=modPred, testing[,direction]))
    gc()
#     
    #' Random Forest
    modFit<-train(training[,direction] ~ .,method="rf",data=training.sc)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modRF:=(as.numeric(modPred)-2)*abs(PriceShift15)]
    print("**Random Forest**")
    print(Sys.time())
    print(confusionMatrix(data=modPred, testing[,direction]))
    gc()
#     
    #' Generalized Boosted Regression Models
    modFit<-train(training[,direction] ~ .,method="gbm",data=training.sc, verbose=FALSE)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modGBM:=(as.numeric(modPred)-2)*abs(PriceShift15)]
    print("**Generalized Boosted Regression Models**")
    print(Sys.time())
    print(confusionMatrix(data=modPred, testing[,direction]))
    gc()
    
    #' eXtreme Gradient Boosting
    print("**Start eXtreme Gradient Boosting**")
    print(Sys.time())

    modFit<-train(training[,direction] ~ .,method="xgbLinear",data=training.sc)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modxgbLinear:=(as.numeric(modPred)-2)*abs(PriceShift15)]
    print(confusionMatrix(data=modPred, testing[,direction]))

    print(Sys.time())
    print("**End eXtreme Gradient Boosting**")
    gc()
    
    testProfit
    
}

tP<-rbindlist(lapply(2012:2015, FUN=processModFit))
pfvars<-c("Profit",
          "modKNN",
          "modsvmRadial",
          "modRpart",
          "modRF",
          "modGBM",
          "modxgbLinear")
tP<-melt(tP,measure.vars = pfvars)

dtEq=tP[,.(cumsum(value)/5,DT),by=.(variable)]
qplot(y=V1,x=DT,data=dtEq,color=factor(variable),geom="line")

