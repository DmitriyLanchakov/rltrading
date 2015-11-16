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
  friendurl<-paste("http://smart-lab.ru/profile/ajaxfriendslist/?JsHttpRequest=0&login=",userlogin,sep="")
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
  readerurl<-paste("http://smart-lab.ru/profile/ajaxreaderslist/?JsHttpRequest=0&login=",userlogin,sep="")
  
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
for(usi in seq(150,nrow(peopleDT), 150)){
  usiDT<-lapply(peopleDT$pageHref[seq(usi-149,usi)],getUserData)
  usiDT<-rbindlist(usiDT, fill=TRUE)
  smartDT<-rbind(smartDT, usiDT)
  print(paste(usi-149, usi))
  Sys.sleep(61*10)
}
  
smartDT
