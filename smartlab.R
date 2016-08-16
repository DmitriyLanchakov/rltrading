library(RCurl)
library(XML)
library(data.table)

### read user names and profile refs
peopleurl<-"http://smart-lab.ru/people/all/page"
Sys.setlocale(category = "LC_ALL", locale = "Russian")
page<-1
peopleDT<-data.table()
pageUsers<-1
while(length(pageUsers)){
    pageeurl<-paste(peopleurl, page, sep="")
    if(url.exists(pageeurl)){
        doc <- htmlParse(pageeurl)
        pageUsers<-xpathSApply(doc,"//tbody/tr/td[@class='user']/a[@class='trader_other']/img",xmlGetAttr,"alt")
        pageHref<-xpathSApply(doc,"//tbody/tr/td[@class='user']/a[@class='trader_other']",xmlGetAttr,"href")
        peopleDT<-rbind(peopleDT,data.table(cbind(pageUsers, pageHref)),use.names=FALSE, fill=FALSE)
    }
    page<-page+1
    Sys.sleep(5)
    print(page)
}
peopleDT[,user_login:=gsub("(http://smart-lab.ru/profile/)|(/)", "", pageHref)]

save(peopleDT, file="sl.RData")
load("sl.RData")
#Russian Month convertion

rusDateConvert<-function(rusdate, pattern="%d %m %Y, %H:%M"){
    rusmonth<-c("января",
                "февраля",
                "марта",
                "апреля",
                "мая",
                "июня",
                "июля",
                "августа",
                "сентября",
                "октября",
                "ноября",
                "декабря")
    if(!length(rusdate)) rusdate="01 января 1900"
    strptime(gsub(strsplit(rusdate," ",fixed=TRUE)[[1]][2],
                  grep(strsplit(rusdate," ",fixed=TRUE)[[1]][2],rusmonth),rusdate),
             pattern)
    
    
}

## User Data
getUserData<-function(userurl){
  userlogin<-gsub("(http://smart-lab.ru/profile/)|(/)", "", userurl)
  if(url.exists(userurl)){
    doc <- htmlParse(userurl)
    userTables<-readHTMLTable(doc, header=FALSE, as.data.frame=TRUE, stringsAsFactors=FALSE)
    userTables<-rbindlist(userTables, fill=TRUE)
    userRatio<-userTables[grep("Рейтинг", V1),gsub(" \\(.*","",V2)]
    userPower<-userTables[grep("Сила", V1),V2]
    userSex<-userTables[grep("Пол", V1),V2]
    userBirth<-userTables[grep("Дата рождения", V1),V2]
    userLocation<-userTables[grep("Местоположение", V1),V2]
    userReg<-userTables[grep("Зарегистрирован", V1),V2]
    userLastVisit<-userTables[grep("Последний визит", V1),V2]
    #userLastVisit<-rusDateConvert(userLastVisit, "%d %m %Y, %H:%M")
    #userReg<-rusDateConvert(userReg, "%d %m %Y, %H:%M")
    #userBirth<-rusDateConvert(userBirth, "%d %m %Y")
  }
  else{
      userRatio<-userPower<-userSex<-userBirth<-userLocation<-userReg<-userLastVisit<-NA
  }
      
  
  #Sys.sleep(2)
  #Friends script
  friendurl<-paste("http://smart-lab.ru/profile/ajaxfriendslist/?JsHttpRequest=99&login=",userlogin,sep="")
  if(url.exists(friendurl)){
    friendlist<-readLines(friendurl)
    friendlist<-gsub("\"","", friendlist)
    friendlist<-gsub("\\{","", friendlist)
    friendlist<-gsub("\\}","", friendlist)
    friendlist<-unlist(strsplit(friendlist, ",", fixed=TRUE))
    friendlist<-gsub("user_login:","",friendlist[grepl("user_login:", friendlist)])
  }
  else
    friendlist<-NA
  #Sys.sleep(2)
  #Readers script
  readerurl<-paste("http://smart-lab.ru/profile/ajaxreaderslist/?JsHttpRequest=99&login=",userlogin,sep="")
  
  if(url.exists(readerurl)){
    readerlist<-readLines(readerurl)
    readerlist<-gsub("\"","", readerlist)
    readerlist<-gsub("\\{","", readerlist)
    readerlist<-gsub("\\}","", readerlist)
    readerlist<-unlist(strsplit(readerlist, ",", fixed=TRUE))
    readerlist<-gsub("user_login:","",readerlist[grepl("user_login:", readerlist)])
  }  
  else 
    readerlist<-NA
  
  
  if(is.null(userRatio)|length(userRatio)==0)userRatio=NA
  if(is.null(userPower)|length(userPower)==0)userPower=NA
  if(is.null(userSex)|length(userSex)==0) userSex=NA
  if(is.null(userBirth)|length(userBirth)==0)userBirth=NA
  if(is.null(userLocation)|length(userLocation)==0)userLocation=NA
  if(is.null(userReg)|length(userReg)==0)userReg=NA
  if(is.null(userLastVisit)|length(userLastVisit)==0)userLastVisit=NA
  if(is.null(readerlist)|length(readerlist)==0)readerlist=NA
  if(is.null(friendlist)|length(friendlist)==0)friendlist=NA
  
  
  print(paste(Sys.time(),
              userlogin,
              userRatio, 
              userPower,
              userSex, 
              userBirth, 
              userLocation, 
              userReg, 
              length(friendlist), 
              length(readerlist)))
  
  #Sys.sleep(2)
  data.table(userlogin,
                   userRatio, 
                   userPower,
                   userSex, 
                   userBirth, 
                   userLocation, 
                   userReg, 
                   userLastVisit, 
                   readerlist,
                   friendlist)
  
    
}

smartDT<-data.table()

for(usi in seq(29400,nrow(peopleDT), 10)){
  usiDT<-lapply(peopleDT$pageHref[seq(usi-9,usi)],getUserData)
  usiDT<-rbindlist(usiDT, fill=TRUE)
  smartDT<-rbind(smartDT, usiDT)
  print(paste(usi-9, usi))
  Sys.sleep(61*1)
}

load(file="alluser.RData")  
smartDT<-unique(smartDT)

save(smartDT,file="alluser.RData")  

smartDT[,id:=.I]

setkey(smartDT,id)
smartDT[,userBirthDate:=as.POSIXct(rusDateConvert(userBirth,"%d %m %Y")),by=1:nrow(smartDT)]
smartDT[,userRegDate:=as.POSIXct(rusDateConvert(userReg)),by=1:nrow(smartDT)]
smartDT[,userLastVisitDate:=as.POSIXct(rusDateConvert(userLastVisit)),by=1:nrow(smartDT)]
smartDT[,userAge:=year(Sys.Date())-year(userBirthDate)]
smartDT[,userSLAge:=year(userLastVisitDate)-year(userRegDate)]
smartDT[,userReg:=NULL]
smartDT[,userBirth:=NULL]
smartDT[,userLastVisit:=NULL]

library(ggplot2)
#User Age Hist

smartDT[userAge<100 & userAge>10,userlogin, by=.(userlogin, userAge), mult="first"][,.N]
qplot(userAge, data = smartDT[userAge<100 & userAge>10,userlogin, by=.(userlogin, userAge), mult="first"])+
    scale_y_continuous(breaks=seq(0,1000,100))+
    scale_x_continuous(breaks=seq(10,100,5))


smartDT[userAge<45 & userAge>25,userlogin, by=.(userlogin, userAge), mult="first"][,.N]

#User Sex Hist
qplot(userSex, data = smartDT[,userlogin, by=.(userlogin,userSex), mult="first"],
      fill=userSex)+
    scale_y_continuous(breaks=seq(0,20000,500))
    

#User Location Hist
smartDT[,userLocation, by=.(userlogin,userLocation), mult="first"][,.N, by=userLocation][order(-N)][N>10][1:20]

#SL user lifetime Hist
qplot(userSLAge, data = smartDT[userSLAge<6,mean(userSLAge), by=.(userlogin,userSLAge), mult="first"], binwidth=1)+
    scale_y_continuous(breaks=seq(0,10000,500))+
    scale_x_continuous(breaks=seq(0,10,1))


#Last Visit Hist
#Registration Hist
qplot(userLastVisitDate,
      data=smartDT[year(userLastVisitDate)>=2010,userlogin,
                   by=.(userlogin,
                        userLastVisitDate), mult="first"])+
    scale_y_continuous(breaks=seq(0,6000,500))+
    scale_x_datetime(breaks = date_breaks("1 month"),
                 labels = date_format("%b\n%Y"))

smartDT[userLastVisitDate>=as.POSIXct("2015-10-15") & userSLAge>0,userlogin,
        by=.(userlogin,
             userLastVisitDate), mult="first"][,.N]

#Registration Hist
qplot(userRegDate,
      data=smartDT[year(userRegDate)>=2010 & userRegDate<as.POSIXct(Sys.Date()),userlogin,
                   by=.(userlogin,
                        userRegDate), mult="first"])+
    scale_y_continuous(breaks=seq(0,2000,100))+
    scale_x_datetime(breaks = date_breaks("3 month"),
                                          labels = date_format("%b\n%Y"))


smartDT[readerlist=="r0man",unique(userlogin),by=.(readerlist)]

# Readers count rating
smartDT[,unique(readerlist),by=.(userlogin)][,.N, by=userlogin][order(-N)][1:20]

# User Reads count rating
smartDT[,unique(userlogin),by=.(readerlist)][,.N, by=readerlist][order(-N)][1:20]

# Friends count rating
smartDT[,unique(friendlist),by=.(userlogin)][,.N, by=userlogin][order(-N)][1:20]

