library(dplyr)
library(rvest)
library(xml2)
library(stringr)


cat2singerlist<-function(cat.addr){
  web<-read_html(cat.addr)
  webcont<-html_nodes(web,"tr td")
  
  # all sublist under major category
  ind<-which(grepl("依 罗马拼音",webcont)&(!grepl("魔镜",webcont)))
  sub.tail<-webcont[ind]%>%head(1)%>%html_nodes("a")%>%html_attr("href")
  sub.addr<-sort(addr.tail.replace(cat.addr,sub.tail))
  full.singer.list<-c()
  for(i in 1:length(sub.addr)){
    full.singer.list<-unique(rbind(full.singer.list,get.singer.list(sub.addr[i],i==1)))
  }
  return(full.singer.list)
}

get.singer.list<-function(addr,hot=F){

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

#
read_html_loop<-function(addr){
  do<-1
  while(do<5&(!exists("web"))){
    tryCatch({
      web<-read_html(addr)
    }, error=function(e){
      Sys.sleep(1)
      print(paste(addr,"redo",do,"times..."))
    })
    do<-do+1
  }
  if(exists("web")){
    return(web)
  }else{
    print(paste(addr,"redo",do,"GIVE UP..."))
    return(NULL)
  }
}

# Function
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

addr.tail.replace<-function(addr,repl){
  repl<-gsub("^/","",repl)
  tail.org<-strsplit(addr,"/")%>%unlist%>%tail(1)
  paste(gsub(tail.org,"",addr),repl,sep="")
}


get.singer.info<-function(addr){
  raw<-read_html_loop(addr)
  df=data.frame(
    Name=addr,
    Alias="",
    Album="",
    Song="",
    Bio="",
    stringsAsFactors = F
  )
  if(!is.null(raw)){
    web<-html_nodes(raw,"dl dd")
    ind<-grep("hb",web%>%html_attr("class"))
    name.trunk<-web[ind][1]%>%cleanFun
    name<-nt2name(name.trunk)
    void.test<-gsub("\\s+","",name)
    if(nchar(void.test)!=0){
      alias<-nt2alias(name.trunk)
      work<-nt2work(name.trunk)
      bio<-web[ind][2]%>%cleanFun
      df=data.frame(
        Name=name,
        Alias=alias,
        Album=work[1],
        Song=work[2],
        Bio=bio,
        stringsAsFactors = F
      )
      print(paste(addr,name,"done!"))
    }
  }
  return(df)
}

nt2name<-function(trunk){
  name<-str_extract(trunk,"[^(\\s|\\()]*(\\(|\\s)+")
  name<-gsub("\\(|\\s|【","",name)
  return(name)
}

nt2alias<-function(trunk){
  alias<-str_extract(trunk,"\\(.*\\)")
  alias<-gsub("\\(\\s+|\\s+\\)","",alias)
  return(alias)
}

nt2work<-function(trunk){
  str_extract_all(trunk,"[0-9]+")%>%unlist%>%as.numeric
}

rm.end.sp<-function(x){
  gsub("(^\\s+)|(\\s+$)","",x)
}


# Get Album Information
get.album.info<-function(row,singer){
  row.vect<-row%>%html_nodes("span")
  alb.name<-row.vect[1]%>%cleanFun%>%rm.end.sp
  col2<-row.vect[2]%>%cleanFun
  alb.date<-str_extract(col2,"[0-9]{4}-[0-9]{2}")
  alb.lan<-gsub(alb.date,"",col2)
  song.df<-data.frame(
    song.title=row.vect[3:4]%>%html_nodes("a")%>%cleanFun,
    song.link=row.vect[3:4]%>%html_nodes("a")%>%html_attr("href"),
    stringsAsFactors = F
  )
  tigong<-grep("提供",song.df$song.title)
  if(length(tigong)>0){
    song.df$song.link[c(tigong,tigong-1)]<-""
    song.df<-song.df[-tigong,]
  }
  result<-data.frame(
    Singer=singer,
    Album.name=alb.name,
    Album.date=alb.date,
    Album.lang=alb.lan,
    Song.title=song.df$song.title,
    Song.link=song.df$song.link,
    stringsAsFactors = F
  )
  paste(singer,alb.name,"done!")%>%print
  return(result)
}






get.singer.album<-function(addr){
  raw<-read_html_loop(addr)
  if(!is.null){
    web<-raw%>%html_nodes("dl dd")
    ind<-grep("hb",web%>%html_attr("class"))
    web<-web[ind]
    singer<-web[1]%>%cleanFun%>%nt2name
    alb.ind<-grep("歌词",web)
    if(!is.null(alb.ind)){
      web<-web[alb.ind]
      album.df<-c()
      for(alb in 1:length(web)){
        temp<-get.album.info(web[alb],singer)
        album.df<-rbind(album.df,temp)
      }
      paste(paste(rep("-",70),collapse=""),"\n",addr,singer,"done!\n\n\n")%>%cat
      return(album.df)
    }
  }else{
    paste(paste(rep("-",70),collapse=""),"\n",addr,singer,"empty!\n\n\n")%>%cat
    return(NULL)
  }
}







