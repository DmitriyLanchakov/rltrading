#Limit Orderbook plot
orderbookggplot<-function(df, depth=3){
    ggplot()+
        geom_line(data=df,aes(x=datetime, y=bidprice0),color="mediumaquamarine")+
        geom_point(data=df,aes(x=datetime, y=bidprice0,size=bidvolume0), colour="mediumaquamarine")+
        
        geom_line(data=df,aes(x=datetime, y=bidprice1), colour="mediumaquamarine")+
        geom_point(data=df,aes(x=datetime, y=bidprice1,size=bidvolume1), colour="mediumaquamarine")+
        
        geom_line(data=df,aes(x=datetime, y=bidprice2), colour="mediumaquamarine")+
        geom_point(data=df,aes(x=datetime, y=bidprice2,size=bidvolume2), colour="mediumaquamarine")+
        
#         geom_line(data=df,aes(x=datetime, y=bidprice3), colour="mediumaquamarine")+
#         geom_point(data=df,aes(x=datetime, y=bidprice3,size=bidvolume3), colour="mediumaquamarine")+
#         
#         geom_line(data=df,aes(x=datetime, y=bidprice4), colour="mediumaquamarine")+
#         geom_point(data=df,aes(x=datetime, y=bidprice4,size=bidvolume4), colour="mediumaquamarine")+
#         
#         geom_line(data=df,aes(x=datetime, y=bidprice5), colour="mediumaquamarine")+
#         geom_point(data=df,aes(x=datetime, y=bidprice5,size=bidvolume5), colour="mediumaquamarine")+
        
        geom_line(data=df,aes(x=datetime, y=askprice0),colour="lightcoral")+
        geom_point(data=df,aes(x=datetime, y=askprice0,size=askvolume0), colour="lightcoral")+
        
        geom_line(data=df,aes(x=datetime, y=askprice1),colour="lightcoral")+
        geom_point(data=df,aes(x=datetime, y=askprice1,size=askvolume1), colour="lightcoral")+
        
        geom_line(data=df,aes(x=datetime, y=askprice2),colour="lightcoral")+
        geom_point(data=df,aes(x=datetime, y=askprice2,size=askvolume2), colour="lightcoral")+
        
#         geom_line(data=df,aes(x=datetime, y=askprice3),colour="lightcoral")+
#         geom_point(data=df,aes(x=datetime, y=askprice3,size=askvolume3), colour="lightcoral")+
#         
#         geom_line(data=df,aes(x=datetime, y=askprice4),colour="lightcoral")+
#         geom_point(data=df,aes(x=datetime, y=askprice4,size=askvolume4), colour="lightcoral")+
#         
#         geom_line(data=df,aes(x=datetime, y=askprice5),colour="lightcoral")+
#         geom_point(data=df,aes(x=datetime, y=askprice5,size=askvolume5), colour="lightcoral")+    
        
        geom_line(data=df,aes(x=datetime, y=price),colour="goldenrod")+
        geom_point(data=df,aes(x=datetime, y=price, size=volume),colour="goldenrod")+
        labs(size = "volume")+
        xlab("dateime, sec")+
        ylab("price")+
        ggtitle(paste(min(df$datetime), max(df$datetime), sep=" :: "))#+
        #scale_x_datetime(breaks = date_breaks("5 sec"))
    
}