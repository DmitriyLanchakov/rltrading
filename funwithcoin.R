# Fun with Coin
theta <- 0.6
N <- 1000
acc<-0.9999

ddf<-rbindlist(lapply(1:1000, FUN=function(j){
    flips <- rbinom(n = N, 
                size = 1, 
                prob = theta)

df<-as.data.table(t(sapply(1:N, FUN=function(x){ 
    p=mean(flips[1:x])
    ci=p + c(-qnorm(acc),qnorm(acc))*sqrt((1/x)*p*(1-p))
    c(p=p,low=ci[2], up=ci[1])
    })))


df[,i:=.I]
df[,j:=j]
}
))

ggplot(data=ddf)+
    geom_line(aes(y=p, x=i, color=j))+
    #geom_line(aes(y=low, x=i), color="red")+
    #geom_line(aes(y=up, x=i), color="red")+
    scale_x_continuous(breaks=seq(0,N, 50))