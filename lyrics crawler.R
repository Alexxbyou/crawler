#############################################################################
# Libraries and sources
#############################################################################
library(rvest)
library(xml2)
library(stringr)
library(dplyr)
source("crawler.functions.R")

#############################################################################
# Variables
#############################################################################
main.addr<-c(
  male.main.addr="https://mojim.com/cnza2.htm",
  female.main.addr="https://mojim.com/cnzb2.htm",
  band.main.addr="https://mojim.com/cnzc2.htm",
  western.main.addr="https://mojim.com/cnze2.htm"
)

cat.lkup<-data.frame(
  MainCategory=c("Male Singer","Female Singer","Band","Western"),
  Addr=main.addr,
  stringsAsFactors = F
)

Singer.master.list<-c()
for(cat.n in 1:4){
  df.temp<-cat2singerlist(cat.lkup$Addr[cat.n])
  df.temp$MainCat<-cat.lkup$MainCategory[cat.n]
  df.temp<-df.temp[,c(4,1:3)]
  Singer.master.list<-unique(rbind(Singer.master.list,df.temp))
  print(cat("\n\n\n\n\n\n\n"))
}

#dir.create("data")
saveRDS(Singer.master.list,"data/Singer.master.list-20180228.RDS")


Singer.master.list.sub<-unique(Singer.master.list[,1:3])
Singer.master.list.hot<-Singer.master.list[Singer.master.list$Hot==T,]
Singer.master.list<-left_join(Singer.master.list.sub,Singer.master.list.hot)
ind<-is.na(Singer.master.list$Hot)
Singer.master.list$Hot[ind]<-F
#saveRDS(Singer.master.list,"data/Singer.master.list.RDS")
#Singer.master.list<-readRDS("data/Singer.master.list.RDS")

# Get song list
singer.info<-c()
n_singer<-nrow(Singer.master.list)
for(s in 1:n_singer){
  temp<-get.singer.info(Singer.master.list$Addr[s])
  singer.info<-rbind(singer.info,temp)
  print(s+1)
}

# 

singer.album<-c()
for(sa in 1:n_singer){
  temp<-get.singer.album(Singer.master.list$Addr[sa])
  singer.album<-rbind(singer.album,temp)
  print(sa+1)
}


