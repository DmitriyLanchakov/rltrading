# Bates Model Calibration
library(NMOF)
library(DEoptim)
library(fOptions)
library(nloptr)

optimfun<-function(p){
    data<-histOptData
    data[, res:=
             (callCF(cf = cfBates, S = PriceMid, X = Strike, tau = tau, 
                     r=0,
                     q=0,
                     v0=p[1],
                     vT=p[2],
                     rho=p[3],
                     k=p[4],
                     sigma=p[5],
                     lambda=p[6],
                     muJ=p[7],
                     vJ=p[8], implVol = FALSE)-PRICE)^2,by=1:nrow(data)]
    sqrt(sum(data$res))/nrow(data)
}

findBatesModelParams<-function(histOptData){
    eps <- 1e-8
    l<-c(eps, eps, -1.0+eps,eps,eps, eps,eps,eps)
    u<-c(5.0-eps, 1.0-eps,1.0-eps, 1.0-eps, 1.0-eps,1.-eps,1.0-eps,1.0-eps)
    maxIt <- 20 
    population <- 100
    startTime<- Sys.time()
    fit = DEoptim(fn=optimfun, lower=l, upper=u, control=list(NP=population, itermax=maxIt))
    p<-as.numeric(fit$optim$bestmem)
    
    res<- nloptr( x0=p, 
                  eval_f=optimfun, 
                  lb = l, 
                  ub = u, 
                  opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-7))
    
    print(paste("Total Calibration Time:",Sys.time()-startTime))
    print(paste("Solution: ", res$solution))
    print(paste("RMSE: ", res$objective))
    res$solution
    
}


