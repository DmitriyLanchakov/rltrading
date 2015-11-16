library("rusquant")
library("PerformanceAnalytics")
library("fUnitRoots")
library("tseries")
library(ggplot2)
library(grid)

StatArb<-function(symbolLst,startDate=Sys.Date(), numdays, period){
   
    #Download the data
    dev.new()
    
    symbA<-getSymbols(symbolLst[1], from=startDate-numdays,to=startDate, src="mfd",period=period,auto.assign=FALSE)
    symbB<-getSymbols(symbolLst[2], from=startDate-numdays,to=startDate, src="mf",period=period,auto.assign=FALSE)
    
    symbA<-Cl(symbA)
    symbB<-Cl(symbB)
    tmp<-cbind(symbA, symbB)
    
    symbA<-tmp[,1]
    symbB<-tmp[,2]
    
    #Step 1: Calculate continius comp returns
    Ret.a <- as.numeric(Delt(symbA,type="log"))
    Ret.b <- as.numeric(Delt(symbB,type="log"))
    
    #Step 2: Regress the returns onto each other
    #Regression finds BETA and C in the linear regression retA = BETA * retB + C
    regression <- lm(Ret.a ~ Ret.b + 0)
    betaRet <- as.numeric(coef(regression)[1])
    Ret.betab<-betaRet*  Ret.b
    #Step 3: Use the regression co-efficients to generate the spread
    spread <- symbA - betaRet*symbB
    #spread<-spread[!is.na(spread)]    
    spreadRet <- Delt(spread,type="log")*100
    #spreadRet <- na.omit(spreadRet)
    
    data<-data.frame(datetime=index(spread),spread, spreadRet, 
                     symbA, symbB, Ret.a, Ret.b,Ret.betab,  row.names=NULL)
    colnames(data)<-c("datetime", "spread", 
                      "spreadRet","symbA", "symbB", "Ret.a", "Ret.b", "Ret.betab")
    
    #Step 4: Use the ADF to test if the spread is stationary
    #can use tSeries library
    spread<-spread[!is.na(spread)]
    adfResults <- adf.test((spread),k=0,alternative="stationary")
    filenm<-paste(symbolLst[1], symbolLst[2],startDate,numdays,period, sep="_")
    png(paste(filenm,
              ".png"),
        width = 1200, height = 800 )
    pushViewport(viewport(layout = grid.layout(2, 2)))
    gg<-ggplot(data)
    
    #Plot the pairs
    print(gg+
              geom_line(aes(x=datetime, y=symbA), colour="red", size=1)+
              geom_line(aes(x=datetime, y=symbB), colour="blue", size=1)+
              ylab("Price")+xlab(" ")+ 
              ggtitle(paste("Prices ",symbolLst[1]," & ",symbolLst[2])), 
          vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    
    
    #Plot correlation and regression
    print(gg+
              geom_line(aes(x=Ret.b, y=Ret.betab), colour="blue")+
              geom_point(aes(x=Ret.b,y= Ret.a), colour="black", size=3)+             
              ylab(paste("Stock A return ",symbolLst[1]))+
              xlab(paste("Stock B return", symbolLst[1]))+ 
              ggtitle(paste("Regression of RETURNS ",symbolLst[1],
                            " & ",symbolLst[2], "\n BETA=", round(betaRet,3))), 
          vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    
    #plot Spread Returns
    print(gg+
              geom_line(aes(datetime, spreadRet), colour="black")+
              ylab("Spread Returns, %")+xlab("")+ 
              ggtitle(paste("Spread Returns \n ADF test pvalue =",
                            round(adfResults$p.value,3), ", Stationary ",
                            adfResults$p.value <= 0.05)), 
          vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    
    #plot Spread density
    print(gg+stat_density(aes(spreadRet))+
              ylab("Density")+xlab("Spread Returns, %")+ ggtitle(""), 
          vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    #For a cointegrated spread the cumsum should not deviate very far from 0
    #For a none-cointegrated spread the cumsum will likely show some trending characteristics
    write.csv(summary(spreadRet),    paste(filenm,".csv"))
    dev.off ();
    
}



