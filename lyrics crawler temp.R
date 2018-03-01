library(rvest)
library(xml2)
library(stringr)

source("crawler.functions.R")

male.main.addr<-"https://mojim.com/cnza2.htm"
female.main.addr<-"https://mojim.com/cnzb2.htm"
band.main.addr<-"https://mojim.com/cnzc2.htm"
western.main.addr<-"https://mojim.com/cnze2.htm"


male.main.web<-read_html(male.main.addr)
web<-male.main.web



name1 <- html_nodes(male.main.web,'h1 a')

singer1.addr<-"https://mojim.com/cnh104350.htm"
singer1.web<-read_html(singer1.addr)


singer1.cont<-html_nodes(singer1.web,"dl dd")

singer1.cont[2]


#



lyrics.addr<-"https://mojim.com/cny104350x14x1.htm"
lyrics.web<-read_html(lyrics.addr)

html_nodes(lyrics.web,"#fsZx3")




#################################################################
# Song list under one singer

addr<-"https://mojim.com/cnh183024.htm"



sl<-html_nodes(sweb,"dl dd")

# name
sl[ind][1]%>%cleanFun
# bio
sl[ind][2]%>%cleanFun







#################################################################
# Singer list
s<-"https://mojim.com/cnzlhc2_01.htm"

sweb<-read_html(s)
sl<-html_nodes(sweb,"tr td")

# all sublist under major category
ind<-which(grepl("依 罗马拼音",sl)&(!grepl("魔镜",sl)))
aa<-sl[ind][1]%>%html_nodes("a")

# all artists' link
sl[mean(ind)]%>%html_nodes("a")%>%html_attr("href")
# all artists' name, use NA in above vector to filter
sl[mean(ind)]%>%html_nodes("a")%>%cleanFun


#################################################################
# Major category

male.main.addr<-"https://mojim.com/cnza2.htm"
female.main.addr<-"https://mojim.com/cnzb2.htm"
band.main.addr<-"https://mojim.com/cnzc2.htm"
western.main.addr<-"https://mojim.com/cnze2.htm"




cat.addr<-"https://mojim.com/cnzlhc2_01.htm"

cat2singerlist<-function(cat.addr){
  web<-read_html(cat.addr)
  webcont<-html_nodes(web,"tr td")
  
  # all sublist under major category
  ind<-which(grepl("依 罗马拼音",webcont)&(!grepl("魔镜",webcont)))
  sub.tail<-webcont[ind]%>%head(1)%>%html_nodes("a")%>%html_attr("href")
  sub.addr<-sort(addr.tail.replace(cat.addr,sub.tail))
  singer.lst<-lapply(sub.addr,function(x)get.singer.list(x,x==sub.addr[1]))
  full.singer.list<-do.call(rbind.data.frame,singer.lst)
  return(full.singer.list)
}

get.singer.list<-function(addr,hot=F){
  web<-read_html(addr)
  webcont<-html_nodes(web,"tr td")
  
  # all sublist under major category
  ind<-which(grepl("依 罗马拼音",webcont)&(!grepl("魔镜",webcont)))
  link.tail<-webcont[mean(ind)]%>%html_nodes("a")%>%html_attr("href")
  singer<-webcont[mean(ind)]%>%html_nodes("a")%>%cleanFun
  singer<-singer[!is.na(link.tail)]
  link.tail<-link.tail[!is.na(link.tail)]
  result<-data.frame(
    Singer=singer,
    Addr=addr.tail.replace(addr,link.tail),
    Hot=hot,
    stringsAsFactors=F
  )
  print(paste(addr,"read!"))
  return(result)
}


