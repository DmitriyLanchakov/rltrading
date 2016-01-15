library(caret)
library(data.table)
library(ggplot2)
fname<-"ML"
setwd(fname)

dt<-fread("R.csv", stringsAsFactors = FALSE)


dt[,DT:=as.POSIXct(DT)]
dt[,direction:=factor(sign(Profit),levels=c(-1,0,1),labels=c("down","flat", "up"))]
features<-c("direction","Gap", "Timeout")


processModFit<-function(yr){
    
    training<-dt[format(DT,"%Y")==yr-1, .SD, .SDcols=features]
    testing<-dt[format(DT,"%Y")==yr, .SD, .SDcols=features]
    testProfit<-dt[format(DT,"%Y")==year(dt[, max(DT)])]
    
    print(paste("Training:",yr-1, "/ Testing:", yr))
    
    # Center and Scale data
    preproc <- preProcess(dt[,.SD, .SDcols=features[-1]])#, method='pca', thresh=0.99)
    training.sc <- predict(preproc, training[,-1, with=FALSE])
    testing.sc <- predict(preproc, testing[,-1, with=FALSE])
    dim(training.sc); dim(testing.sc)
    
    
#     #' k-Nearest Neighbors
#     modFit<-train(training[,direction] ~ .,method="knn",data=training.sc)
#     modPred<-predict(modFit,newdata=testing.sc)
#     testProfit[,modKNN:=(as.numeric(modPred)-2)*Profit]
#     print("**k-Nearest Neighbors model**")
#     confusionMatrix(data=modPred, testing[,direction])
#     gc()
#     
#     #' Support Vector Machines with Radial Basis Function Kernel
#     modFit<-train(training[,direction] ~ .,method="svmRadial",data=training.sc)
#     modPred<-predict(modFit,newdata=testing.sc)
#     testProfit[,modsvmRadial:=(as.numeric(modPred)-2)*Profit]
#     print("**Support Vector Machines with Radial Basis Function Kernel**")
#     confusionMatrix(data=modPred, testing[,direction])
#     gc()
#     
#     #' Recursive Partitioning and Regression Trees
#     modFit<-train(training[,direction] ~ .,method="rpart",data=training.sc)
#     modPred<-predict(modFit,newdata=testing.sc)
#     testProfit[,modRpart:=(as.numeric(modPred)-2)*Profit]
#     print("**Recursive Partitioning and Regression Trees**")
#     confusionMatrix(data=modPred, testing[,direction])
#     gc()
#     
#     #' Random Forest
#     modFit<-train(training[,direction] ~ .,method="rf",data=training.sc)
#     modPred<-predict(modFit,newdata=testing.sc)
#     testProfit[,modRF:=(as.numeric(modPred)-2)*Profit]
#     print("**Random Forest**")
#     confusionMatrix(data=modPred, testing[,direction])
#     gc()
#     
#     #' Generalized Boosted Regression Models
#     modFit<-train(training[,direction] ~ .,method="gbm",data=training.sc, verbose=FALSE)
#     modPred<-predict(modFit,newdata=testing.sc)
#     testProfit[,modGBM:=(as.numeric(modPred)-2)*Profit]
#     print("**Generalized Boosted Regression Models**")
#     confusionMatrix(data=modPred, testing[,direction])
#     gc()
    
    #' eXtreme Gradient Boosting
    modFit<-train(training[,direction] ~ .,method="xgbLinear",data=training.sc)
    modPred<-predict(modFit,newdata=testing.sc)
    testProfit[,modxgbLinear:=(as.numeric(modPred)-2)*Profit]
    print("**eXtreme Gradient Boosting**")
    print(confusionMatrix(data=modPred, testing[,direction]))
    gc()
    
    testProfit
    
}

tP<-rbindlist(lapply(2007:2015, FUN=processModFit))
pfvars<-c("Profit",
          "modKNN",
          "modsvmRadial",
          "modRpart",
          "modRF",
          "modGBM",
          "modxgbLinear")
tP<-melt(tP,measure.vars = pfvars)

dtEq=tP[,.(cumsum(value),DT),by=.(variable,Timeout)]
qplot(y=V1,x=DT,data=dtEq,color=factor(Timeout),geom="line",facets = variable~.)

