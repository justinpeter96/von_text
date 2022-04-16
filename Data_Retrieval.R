rm(list=ls())

## Libraries ##

library(dplyr)
library(plyr)
library(rvest)
library(stringr)
library(xml2)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(tm)
library(SnowballC)
library(slam)
library(movMF)

setwd("~/Desktop/STAT6440/Text Mining")
comedy1 <- read.csv("Comedy1.csv")
comedy2 <- read.csv("Comedy2.csv")
crime <- read.csv("crime_new.csv")
crime2 <- crime[1:100,]
comedy_crime <- rbind(comedy1,comedy2,crime2)
write.csv(comedy_crime,"comedy_crime.csv")


crime <- read.csv("crime_ready.csv")
# Explain the small number of columns due to the rank
# Look to explain the ngram situation
# Cluster regarding the year and see result
load("URLS.RData")

total_data <- data.frame()
for(i in url_total[1:665]){
  url_page <- read_html(i)
  Names <- html_nodes(url_page,'.lister-item-header a') %>% html_text()
  Description <- ifelse(crime$X>0,gsub("\n","",html_nodes(url_page,'.ratings-bar + .text-muted') %>% html_text()),NA)
  total_data <- rbind(total_data,data.frame(Names,Description))
}
Description <- data.frame(Description)
crime$Description2 <- Description











language <- data.frame()
for (i in crime$Links){
  nd <- html_nodes(read_html(i), css = '.ipc-metadata-list-item__content-container') %>%html_nodes('.ipc-inline-list.ipc-inline-list--show-dividers.ipc-inline-list--inline.ipc-metadata-list-item__list-content.base') %>% html_text()
  language <- rbind(language,data.frame(nd[10]))
}







link <- data.frame()
for (i in url_total[1:665]){
  strong <- html_nodes(read_html(i), css = '.lister-item-header') %>% html_nodes("a") %>% html_attr('href')
  tmp <- paste("https://www.imdb.com",strong,sep="")
  link <- rbind(link,data.frame(tmp))
}


crime$link <- paste0("https://en.wikipedia.org/wiki/",gsub(" ","_",crime[,2]),"_","(film)")
#english <- data.frame(g = c("English_USA","English_USA","English_USA","English_USA","English_USA"))
#english$language <- str_split_fixed(english$g,"_",2)[1]
#english$country <- str_split_fixed(english$g,"_",2)[2]
info_url <- data.frame()
for (i in crime[,2]){
  info_url <- rbind(info_url,data.frame(i))
}
info_url$extras <- sapply(crime$link, function(url){
  tryCatch(
    url %>%
      httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
      read_html() %>% 
      html_nodes("tr:nth-child(16) .infobox-data") %>% 
      html_text(), 
    error = function(e){NA}    
  )
})


url_page <- i %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% read_html()
info <- html_nodes(url_page,"tr:nth-child(15) .infobox-label") %>% html_text()
info_url <- rbind(info_url,data.frame(info))
#.vevent

##  NEW PAGE ##
setwd("~/Desktop/STAT6440/Text Mining")
load("URLS.RData")
sub_nodes <- data.frame()
for (i in url_total[1:10]){
  urls <- read_html(i)
  url_nodes <- html_attr(html_nodes(urls,'.lister-item-header a'),"href")
  for(j in url_nodes){
    url_new_node <- paste0("https://www.imdb.com",j)
    url_new_page<-read_html(url_new_node)
    nodes <- html_nodes(url_new_page,'.ipc-metadata-list-item__list-content-item--link') %>% html_text()
    sub_nodes <- rbind(sub_nodes,data.frame(nodes))
  }
}
sub_nodes$Languages <- ifelse(grepl("English", sub_nodes$nodes), "English",
                              ifelse(grepl("Spanish|Portuguese|French|Italian", sub_nodes$nodes), "Romance",
                                     ifelse(grepl("Malayalam|Tamil|Telugu|Kannada|Hindi|Urdu|Bengali|Marathi", sub_nodes$nodes), "Indian",
                                            ifelse(grepl("Mandarin|Cantonese|Korean|Japanese|Vietnamese|Thai", sub_nodes$nodes), "Asian",
                                                   ifelse(grepl("Russian|Polish|Ukrainian|Bulgarian|Serbian|Croatian", sub_nodes$nodes), "Slavic",
                                                          ifelse(grepl("Swedish|Danish|Finnish|Dutch|German", sub_nodes$nodes), "Germanic",
                                                                 ifelse(grepl("Greek|Turkish|Persian|Arabic",sub_nodes$nodes),"Other",NA)))))))
sub_nodes <- na.omit(sub_nodes)
table(sub_nodes$Languages)
sub_nodes$nodes <- NULL

total_data <- data.frame()
for(i in url_total[1:665]){
  url_page <- read_html(i)
  Rank <- gsub(",","",html_nodes(url_page,'.text-primary') %>% html_text())
  Names <- html_nodes(url_page,'.lister-item-header a') %>% html_text()
  total_data <- rbind(total_data,data.frame(Names,Rank))
}
total_data$Rank <- as.numeric(total_data$Rank)
crime3 <- merge(crime2,total_data,by="Names")
crime4 <- crime3[!duplicated(crime3$Rank.y),]
crime5 <- crime3[order(crime3$Rank.y),]
crime6 <- crime5[,-c(2:4)]
row.names(crime6) <- NULL
write.csv(crime3,"crime_updated3.csv")

sub_nodes1 <- data.frame()
for (i in url_total){
  urls <- read_html(i)
  url_nodes <- html_attr(html_nodes(urls,'.lister-item-header a'),"href")
  for(j in url_nodes){
    url_new_node <- paste0("https://www.imdb.com",j)
    url_new_page<-read_html(url_new_node)
    nodes <- html_nodes(url_new_page,'.ipc-metadata-list-item__list-content-item--link') %>% html_text()
    sub_nodes1 <- rbind(sub_nodes1,data.frame(nodes))
  }
}
sub_nodes1$Languages <- ifelse(grepl("English", sub_nodes1$nodes), "English",
                              ifelse(grepl("Spanish|Portuguese|French|Italian", sub_nodes1$nodes), "Romance",
                                     ifelse(grepl("Malayalam|Tamil|Telugu|Kannada|Hindi|Urdu|Bengali|Marathi", sub_nodes1$nodes), "Indian",
                                            ifelse(grepl("Mandarin|Cantonese|Korean|Japanese|Vietnamese|Thai", sub_nodes1$nodes), "Asian",
                                                   ifelse(grepl("Russian|Polish|Ukrainian|Bulgarian|Serbian|Croatian", sub_nodes1$nodes), "Slavic",
                                                          ifelse(grepl("Swedish|Danish|Finnish|Dutch|German", sub_nodes1$nodes), "Germanic",
                                                                 ifelse(grepl("Greek|Turkish|Persian|Arabic",sub_nodes1$nodes),"Other",NA)))))))
sub_nodes1 <- na.omit(sub_nodes1)
table(sub_nodes1$Languages)
sub_nodes1$nodes <- NULL



sub_nodes_first <- read.csv("F430_links.csv")
sub_nodes_first$X <- NULL


## All URLs ##
main_url <- "https://www.imdb.com/search/title/?genres=crime&title_type=feature&sort=num_votes,desc"
url_links <- c()
for(i in seq(from=51, to=9951, by=50)){
  i1 <- sprintf('%02d', i)
  url_new <- paste0("https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&start=",i1,"&ref_=adv_nxt")
  url_links <- append(url_links,url_new)
}
url_total <- c(main_url,url_links,new_url)
#save(url_total, file="URLS.RData")

sub_nodesnurl <- data.frame()
for (i in url_total){
  url_totaln <- read_html(i)
  url_nodesn <- html_attr(html_nodes(url_totaln,'.lister-item-header a'),"href")
  for(j in url_nodesn){
    url_new_noden <- paste0("https://www.imdb.com",j)
    url_new_pagen<-read_html(url_new_noden)
    nodesnurl <- html_nodes(url_new_pagen,'.ipc-metadata-list-item__list-content-item--link') %>% html_text()
    sub_nodesnurl <- rbind(sub_nodesnurl,data.frame(nodesnurl))
  }
}
sub_nodesnurl$Languages <- ifelse(grepl("English", sub_nodesnurl$nodes), "English",NA)
sub_nodesnurl <- na.omit(sub_nodesnurl)


## Language and More ##
main_url <- "https://www.imdb.com/search/title/?genres=crime&title_type=feature&sort=num_votes,desc"
main_page <- read_html(main_url)
title_nodes <- html_nodes(main_page,'.lister-item-header a')
title_data <- as.data.frame(html_text(title_nodes))
names(title_data)[1] <- "Names"
main_nodes <- html_attr(html_nodes(main_page,'.lister-item-header a'),"href")
sub_nodes <- data.frame()
for(i in main_nodes){
  main_new_node <- paste0("https://www.imdb.com",i)
  main_new_page<-read_html(main_new_node)
  nodes <- html_nodes(main_new_page,'.ipc-metadata-list-item__list-content-item') %>% html_text()
  sub_nodes <- rbind(sub_nodes,data.frame(nodes))
}
sub_nodes$Languages <- ifelse(grepl("English", sub_nodes$nodes), "English",
                              ifelse(grepl("Spanish", sub_nodes$nodes), "Romance",
                                     ifelse(grepl("French", sub_nodes$nodes), "Romance",
                                            ifelse(grepl("Arabic", sub_nodes$nodes), "Asian",
                                                   ifelse(grepl("Mandarin", sub_nodes$nodes), "Asian",
                                                          ifelse(grepl("Portuguese", sub_nodes$nodes), "Romance",NA))))))
sub_nodes <- na.omit(sub_nodes)
table(sub_nodes$Languages)
sub_nodes$nodes <- NULL
rownames(sub_nodes) <- NULL
new_row <- c(4)
sub_nodes_new <- rbind(sub_nodes[1:26, ],new_row,sub_nodes[- (1:26), ])
lan_first <- cbind(title_data,sub_nodes_new)


movie_nodesurl <- data.frame()
sub_nodesurl <- data.frame()
for(i in seq(from=51, to=301, by=50)){
  i1 <- sprintf('%02d', i)
  url_new <- paste0("https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&start=",i1,"&ref_=adv_nxt")
  url_page <- read_html(url_new)
  Names <- html_nodes(url_page,'.lister-item-header a') %>% html_text()
  movie_nodesurl <- rbind(movie_nodesurl,data.frame(Names))
  url_nodes <- html_attr(html_nodes(url_page,'.lister-item-header a'),"href")
  for(j in url_nodes){
    url_new_node <- paste0("https://www.imdb.com",j)
    url_new_page<-read_html(url_new_node)
    nodesurl <- html_nodes(url_new_page,'.ipc-metadata-list-item__list-content-item--link') %>% html_text()
    sub_nodesurl <- rbind(sub_nodesurl,data.frame(nodesurl))
  }
}
sub_nodesurl$Languages <- ifelse(grepl("English", sub_nodesurl$nodes), "English",NA)
sub_nodesurl <- na.omit(sub_nodesurl)
sub_nodesurl$nodes <- NULL


## DATASET ##
setwd("~/Desktop/STAT6440/Text Mining")
crime_new <- read.csv("crime_new.csv")


## Page 1 ##

main_url <- "https://www.imdb.com/search/title/?genres=crime&title_type=feature&sort=num_votes,desc"
main_page <- read_html(main_url)
title_nodes <- html_nodes(main_page,'.lister-item-header a')
rank <- as.numeric(html_nodes(main_page,'.text-primary') %>% html_text())
title_data <- as.data.frame(html_text(title_nodes))
names(title_data)[1] <- "Names"
title_data$Rank <- rank
title_data$Year <- gsub("[()]", "",html_text(html_nodes(main_page, '.text-muted.unbold')))
title_data$Content <- html_nodes(main_page,'.text-muted .certificate') %>% html_text()
title_data$Length <- gsub(" min","",html_nodes(main_page,'.text-muted .runtime') %>% html_text())
title_data$Genre <- str_trim(gsub("\n","",html_nodes(main_page,'.genre') %>% html_text()))
title_data$Rating <- html_nodes(main_page,'.inline-block.ratings-imdb-rating strong') %>% html_text()
title_data$Metascore <- str_trim(html_nodes(main_page,'.inline-block.ratings-metascore span') %>% html_text())
title_data$Description <- gsub("\n","",html_nodes(main_page,'.ratings-bar + .text-muted') %>% html_text())
title_data$Director <- html_nodes(main_page,'.text-muted + p a:nth-child(1)') %>% html_text()
title_data$Star <- html_nodes(main_page,'.lister-item-content .ghost+ a') %>% html_text()
title_data$CoStar1 <- html_nodes(main_page,'.lister-item-content .ghost+ a+a') %>% html_text()
title_data$CoStar2 <- html_nodes(main_page,'.lister-item-content .ghost+ a+a+a') %>% html_text()
title_data$CoStar3 <- html_nodes(main_page,'.lister-item-content .ghost+ a+a+a+a') %>% html_text()
title_data$Votes <- html_nodes(main_page,'.sort-num_votes-visible span:nth-child(2)') %>% html_text()
title_data$Gross <- substring(gsub("M","",html_nodes(main_page,'.ghost~ .text-muted+ span') %>% html_text()),2,7)

## For Loop for more pages ##

total_data <- data.frame()
for(i in seq(from=51, to=9951, by=50)){
  i1 <- sprintf('%02d', i)
  url_new <- paste0("https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&start=",i1,"&ref_=adv_nxt")
  url_page <- read_html(url_new)
  Rank <- as.numeric(gsub(",","",html_nodes(url_page,'.text-primary') %>% html_text()))
  Votes <- html_nodes(url_page,'.sort-num_votes-visible span:nth-child(2)') %>% html_text()
  Names <- html_nodes(url_page,'.lister-item-header a') %>% html_text()
  Year <- gsub("I","",gsub("II ","",gsub("I ","",gsub("[()]", "",html_text(html_nodes(url_page, '.text-muted.unbold'))))))
  Content <- ifelse(Votes>=0,html_nodes(url_page,'.text-muted .certificate') %>% html_text(),NA)
  Length <- ifelse(Votes>=0,gsub(" min","",html_nodes(url_page,'.text-muted .runtime') %>% html_text()),NA)
  Genre <- str_trim(gsub("\n","",html_nodes(url_page,'.genre') %>% html_text()))
  Rating <- html_nodes(url_page,'.inline-block.ratings-imdb-rating strong') %>% html_text()
  Metascore <- ifelse(Votes>=0,str_trim(html_nodes(url_page,'.inline-block.ratings-metascore span') %>% html_text()),NA)
  Description <- gsub("\n","",html_nodes(url_page,'.ratings-bar + .text-muted') %>% html_text())
  Director <- ifelse(Votes>=0,html_nodes(url_page,'.text-muted + p a:nth-child(1)') %>% html_text(),NA)
  Star <- ifelse(Votes>=0,html_nodes(url_page,'.lister-item-content .ghost+ a') %>% html_text(),NA)
  CoStar1 <- ifelse(Votes>=0,html_nodes(url_page,'.lister-item-content .ghost+ a+a') %>% html_text(),NA)
  CoStar2 <- ifelse(Votes>=0,html_nodes(url_page,'.lister-item-content .ghost+ a+a+a') %>% html_text(),NA)
  CoStar3 <- ifelse(Votes>=0,html_nodes(url_page,'.lister-item-content .ghost+ a+a+a+a') %>% html_text(),NA)
  Gross <- ifelse(Votes>=0,substring(gsub("M","",html_nodes(url_page,'.ghost~ .text-muted+ span') %>% html_text()),2,7),NA)
  total_data <- rbind(total_data,data.frame(Names,Rank,Year,Content,
                                                           Length,Genre,Rating,Metascore,Description,
                                                           Director,Star,CoStar1,CoStar2,CoStar3,Votes,Gross))
  
}


## Combine Them ##

title_data_total <- rbind(title_data,total_data)
setwd("~/Desktop/STAT6440/Text Mining")
cri <- data.frame(table(title_data_total$Rank))
cri2 <- cri[cri$Freq >1,]
#write.csv(title_data_total,"crime_org.csv")






new_total_data <- data.frame()
new_url <- c("https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIzNiwidHQwMTEzNzkxIiwxMDAwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIzMywidHQwMDU3NjE5IiwxMDA1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIyOSwidHQxMDg3OTA1IiwxMDEwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIyNiwidHQwMDU1Nzk5IiwxMDE1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIyMiwidHQzNjQ0MjU4IiwxMDIwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxOSwidHQyNzM5MDEyIiwxMDI1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxNiwidHQwMTkwMzIyIiwxMDMwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxMywidHQyMzM5NTQ5IiwxMDM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwOSwidHQwMTgxMjg5IiwxMDQwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwNywidHQwMDc2NTc1IiwxMDQ1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwNCwidHQwMDcwNzI3IiwxMDUwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwMSwidHQwMDMzOTY2IiwxMDU1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5NywidHQyODk5MTM2IiwxMDYwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5NSwidHQwMDIyMjM1IiwxMDY1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5MSwidHQxNDI1NjI1IiwxMDcwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4OSwidHQwMDkyMjYwIiwxMDc1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4NiwidHQwMDc3MTMwIiwxMDgwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4MywidHQwMjAyMzYyIiwxMDg1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4MSwidHQwMDI3NDgyIiwxMDkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3OCwidHQwMjA4OTQyIiwxMDk1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3NiwidHQwMDUxNDE2IiwxMTAwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3MywidHQwMjgxMjAzIiwxMTA1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3MSwidHQwMDY0OTQzIiwxMTEwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2OCwidHQwMTE0NDI2IiwxMTE1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2NSwidHQwMDYyNzMwIiwxMTIwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2MywidHQwMTE4ODI4IiwxMTI1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2MSwidHQwMDMwMDEzIiwxMTMwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1OCwidHQxNjA2MjE1IiwxMTM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1NiwidHQwMzE0NTYzIiwxMTQwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1NCwidHQ1MTQ2NjEyIiwxMTQ1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1MiwidHQxMTI5NDMzIiwxMTUwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1MCwidHQ0Njg5ODk4IiwxMTU1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0OCwidHQwMTIxMjU5IiwxMTYwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0NiwidHQyMzc1OTA2IiwxMTY1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0NCwidHQwNDgxNDk3IiwxMTcwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0MiwidHQwMzAwMjkwIiwxMTc1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0MCwidHQwMjM4NTk2IiwxMTgwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzOCwidHQwMTAwMDM5IiwxMTg1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzNiwidHQwMDY0OTc5IiwxMTkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzNCwidHQwMjE0ODUzIiwxMTk1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzMiwidHQwMTY1NzI5IiwxMjAwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzMSwidHQwMTE3MzQ5IiwxMjA1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyOSwidHQwMzczODU3IiwxMjEwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyNywidHQwMTc2MTk4IiwxMjE1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyNSwidHQwMTAzOTIwIiwxMjIwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyMywidHQ1NTQ2OTA0IiwxMjI1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyMiwidHQwMDU0OTEzIiwxMjMwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyMCwidHQwMTIxNTc0IiwxMjM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExOCwidHQwMDkzMDA5IiwxMjQwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExNiwidHQwMzIyOTM4IiwxMjQ1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExNCwidHQwMTQ1MDM4IiwxMjUwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExMiwidHQzNjg2MTY4IiwxMjU1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExMSwidHQwMDUzMTg4IiwxMjYwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwOSwidHQzNzczMjEwIiwxMjY1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwOCwidHQwMTYwNjg2IiwxMjcwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwNywidHQwMDQwOTM2IiwxMjc1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwNSwidHQwNDc1OTc5IiwxMjgwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwNCwidHQwMDgzMjUwIiwxMjg1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwMiwidHQzNzI5ODE4IiwxMjkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwMSwidHQwMjcyNzM1IiwxMjk1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwMCwidHQwMDM5MzI1IiwxMzAwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzk4LCJ0dDI1MDQwODYiLDEzMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzk3LCJ0dDAxNDM5NDQiLDEzMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzk2LCJ0dDAwMzAzMjIiLDEzMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzk0LCJ0dDE4MTcyODkiLDEzMjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzkzLCJ0dDAyMTkzMzYiLDEzMjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzkyLCJ0dDAwNTY4MzgiLDEzMzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzkxLCJ0dDAwMzMyNDQiLDEzMzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzg5LCJ0dDgwMjY1MDYiLDEzNDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzg4LCJ0dDQ2Njc4NzIiLDEzNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzg3LCJ0dDAzMDgzMDYiLDEzNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzg2LCJ0dDAwNTUzNjYiLDEzNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzg1LCJ0dDAwNDEwOTUiLDEzNjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzg0LCJ0dDAwMzE5NjEiLDEzNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgzLCJ0dDAwMzA2MzUiLDEzNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgxLCJ0dDQzMDEwMzQiLDEzNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgwLCJ0dDQ5MzAyOTIiLDEzODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzc5LCJ0dDAzMTQ4NDEiLDEzODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzc4LCJ0dDExMzA1ODk4IiwxMzkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzc3LCJ0dDAzMTE5MjIiLDEzOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzc2LCJ0dDAyODU3MTAiLDE0MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzc1LCJ0dDA0MzAyMzIiLDE0MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wzc0LCJ0dDAyNjY1NDkiLDE0MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzczLCJ0dDAyNzA5ODkiLDE0MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcyLCJ0dDAxOTM2NDUiLDE0MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcxLCJ0dDAxOTkzNDgiLDE0MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcwLCJ0dDAxNTAwOTAiLDE0MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY5LCJ0dDAxMDU4NjUiLDE0MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY4LCJ0dDAxODM2NzYiLDE0NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY3LCJ0dDAzMTk2MjYiLDE0NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY2LCJ0dDI0OTgyODYiLDE0NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY2LCJ0dDAwMjg0MjMiLDE0NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY1LCJ0dDAwMzIwNzYiLDE0NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzY0LCJ0dDAwNTA2NjEiLDE0NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYzLCJ0dDAxMjU1ODQiLDE0NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYyLCJ0dDAyNTc2MjMiLDE0NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYxLCJ0dDQ1NjgxNjYiLDE0ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYxLCJ0dDAwMzg4MjgiLDE0ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYwLCJ0dDAwNjYwODUiLDE0OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU5LCJ0dDAyMzAzMDgiLDE0OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU4LCJ0dDE1ODc4NTciLDE1MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU4LCJ0dDAwMjM4ODMiLDE1MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU3LCJ0dDAxMjE1MTciLDE1MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU2LCJ0dDExMzAwOTAiLDE1MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU2LCJ0dDAwMzc4NTIiLDE1MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU1LCJ0dDAxMTg1OTgiLDE1MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU0LCJ0dDAzNDk0NjIiLDE1MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzU0LCJ0dDAwMjI4MjYiLDE1MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzUzLCJ0dDAwOTU0ODAiLDE1NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzUyLCJ0dDAzODEzNzciLDE1NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzUxLCJ0dDQ0MjA5NDYiLDE1NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzUxLCJ0dDAwNjU5MTIiLDE1NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzUwLCJ0dDAyNDE1NjIiLDE1NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ5LCJ0dDU0OTM3NzYiLDE1NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ5LCJ0dDAxMDQ4MzYiLDE1NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ4LCJ0dDAyMDg0MjciLDE1NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ4LCJ0dDAwMjY0ODciLDE1ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ3LCJ0dDAxMjIyNDEiLDE1ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ2LCJ0dDA0OTkyMTkiLDE1OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ2LCJ0dDAwNDQwMTAiLDE1OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ1LCJ0dDAzMzQ5ODgiLDE2MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ1LCJ0dDAwMzAxMTIiLDE2MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ0LCJ0dDAyODExNDYiLDE2MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQ0LCJ0dDAwMzQxMTEiLDE2MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQzLCJ0dDAyMDM1OTkiLDE2MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQyLCJ0dDIxNjIyNzgiLDE2MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQyLCJ0dDAxMjU1NTQiLDE2MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQxLCJ0dDA4MDYxMDYiLDE2MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQxLCJ0dDAwNDY3NjIiLDE2NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQwLCJ0dDA0NTk4MDQiLDE2NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzQwLCJ0dDAwMzgzNjQiLDE2NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM5LCJ0dDAzNTM5OTMiLDE2NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM5LCJ0dDAwMzA1MjYiLDE2NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM4LCJ0dDAyNzE0NzkiLDE2NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM4LCJ0dDAwMTMzMzYiLDE2NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM3LCJ0dDAyMDE5ODYiLDE2NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM2LCJ0dDMyMjM2MzYiLDE2ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM2LCJ0dDAyNTk5OTQiLDE2ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM2LCJ0dDAwMzI4OTAiLDE2OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM1LCJ0dDAzMTQ4NjEiLDE2OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM1LCJ0dDAwMzA2NDgiLDE3MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM0LCJ0dDA0MTgyMjEiLDE3MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzM0LCJ0dDAwNjg0NzAiLDE3MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMzLCJ0dDA0MzM0NDMiLDE3MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMzLCJ0dDAwNjk3NTkiLDE3MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMyLCJ0dDEzMzc1ODAiLDE3MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMyLCJ0dDAxMjAzNDQiLDE3MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMxLCJ0dDUyNDI3NTQiLDE3MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMxLCJ0dDAyMjU4ODIiLDE3NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMxLCJ0dDAwMzc5MTQiLDE3NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMwLCJ0dDEzMjM4MDUiLDE3NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMwLCJ0dDAxNDE3MzAiLDE3NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzMwLCJ0dDAwMjczODMiLDE3NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI5LCJ0dDAzNTgxNDkiLDE3NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI5LCJ0dDAxMDI4MzQiLDE3NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI4LCJ0dDQxNDY3NjAiLDE3NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI4LCJ0dDAyOTEyOTIiLDE3ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI4LCJ0dDAwNTg3ODgiLDE3ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI3LCJ0dDE3NjkyOTciLDE3OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI3LCJ0dDAxOTUxNTQiLDE3OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI3LCJ0dDAwMjk0NTgiLDE4MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI2LCJ0dDEyOTE1MDUiLDE4MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI2LCJ0dDAxNjUxMjQiLDE4MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI2LCJ0dDAwMjA1MjYiLDE4MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI1LCJ0dDExNzI1MzkiLDE4MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI1LCJ0dDAxNjYzODAiLDE4MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI1LCJ0dDAwMjA3OTIiLDE4MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI0LCJ0dDEwNjcwNDA0IiwxODM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI0LCJ0dDAxNjEwMzYiLDE4NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzI0LCJ0dDAwMjIwNjEiLDE4NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIzLCJ0dDExNTIyODYiLDE4NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIzLCJ0dDAyNDA3MTUiLDE4NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIzLCJ0dDAwNTQ5NjAiLDE4NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIyLCJ0dDMwODgwMTYiLDE4NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIyLCJ0dDAzMjA0MDMiLDE4NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIyLCJ0dDAxMTI5MDUiLDE4NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxLCJ0dDk2MjE1ODAiLDE4ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxLCJ0dDE1MzgyOTAiLDE4ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxLCJ0dDAzMzgzOTUiLDE4OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxLCJ0dDAxMzg1MjEiLDE4OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIxLCJ0dDAwMTk4MzQiLDE5MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwLCJ0dDEzNDU1MjIiLDE5MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwLCJ0dDAyMzc2OTMiLDE5MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzIwLCJ0dDAwNzE5ODkiLDE5MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5LCJ0dDU2NjU2MDAiLDE5MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5LCJ0dDA0NTYwNzIiLDE5MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5LCJ0dDAxOTQ4NjkiLDE5MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE5LCJ0dDAwMzgyOTMiLDE5MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4LCJ0dDQwMTEwMDgiLDE5NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4LCJ0dDAzNzg0NTUiLDE5NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4LCJ0dDAxNzUwMTIiLDE5NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE4LCJ0dDAwNDA1MzkiLDE5NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3LCJ0dDMwODQ3NTIiLDE5NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3LCJ0dDEwNDk5MTA4IiwxOTY1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3LCJ0dDAyODY4MzYiLDE5NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE3LCJ0dDAxMTEzMzEiLDE5NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2LCJ0dDg1NTAyNzYiLDE5ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2LCJ0dDIxNjg0MjAiLDE5ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2LCJ0dDAzNTU1NjgiLDE5OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2LCJ0dDAxODYwMTUiLDE5OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE2LCJ0dDAwMzc1MjEiLDIwMDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1LCJ0dDM0NDM4MTAiLDIwMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1LCJ0dDA5MDE1MTkiLDIwMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1LCJ0dDAyNzc3MTgiLDIwMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE1LCJ0dDAwNjQ5NzciLDIwMjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0LCJ0dDU5MzMwNDAiLDIwMjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0LCJ0dDE1NjA5ODgiLDIwMzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0LCJ0dDAyOTk3MDUiLDIwMzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0LCJ0dDAxODY2NDQiLDIwNDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzE0LCJ0dDAwNTAxNDUiLDIwNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzLCJ0dDU5OTU1MjgiLDIwNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzLCJ0dDE1MTgyMzciLDIwNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzLCJ0dDAzMzc0NDkiLDIwNjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzLCJ0dDAyMTA2NzYiLDIwNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEzLCJ0dDAwNTkxODQiLDIwNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyLCJ0dDcyODk3MjQiLDIwNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyLCJ0dDIyNjUyOTMiLDIwODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyLCJ0dDA3OTYyMzEiLDIwODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyLCJ0dDAyNzIxMjEiLDIwOTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyLCJ0dDAxMDA0NjIiLDIwOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEyLCJ0dDAwMTg2ODUiLDIxMDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExLCJ0dDI1Nzg1NzQiLDIxMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExLCJ0dDEyNTY1MTkiLDIxMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExLCJ0dDAzMzU0MjIiLDIxMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExLCJ0dDAyMjQ0MzQiLDIxMjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzExLCJ0dDAwNzAxMDAiLDIxMjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwLCJ0dDY2ODc3MjAiLDIxMzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwLCJ0dDIzMjU4NzMiLDIxMzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwLCJ0dDEyODUyNTYiLDIxNDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwLCJ0dDAzMzk5OTQiLDIxNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwLCJ0dDAyMzcxNTgiLDIxNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzEwLCJ0dDAwNjMwNDAiLDIxNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzksInR0NjA5Njc4MCIsMjE2MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzksInR0MjczNzc1MCIsMjE2NTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzksInR0MTI3NDI3MCIsMjE3MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzksInR0MDM0MjU0NCIsMjE3NTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzksInR0MDI0MjQ5NCIsMjE4MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzksInR0MDA2MjY5OCIsMjE4NTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgsInR0NjI5OTE4MCIsMjE5MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgsInR0MzE2MjkzNCIsMjE5NTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgsInR0MTY2MTM2OCIsMjIwMDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgsInR0MDQ0NzkyOSIsMjIwNTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgsInR0MDI2MTMwOSIsMjIxMDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzgsInR0MDA5MzQ0NSIsMjIxNTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcsInR0NTgyMTQ2MiIsMjIyMDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcsInR0MjIyNjQxNSIsMjIyNTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcsInR0MTEwNTM4NCIsMjIzMDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcsInR0MDMwOTI1OSIsMjIzNTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzcsInR0MDE3MTEyNiIsMjI0MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYsInR0NjUzNjQ1MCIsMjI0NTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYsInR0MTk0ODYyOSIsMjI1MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzYsInR0MDI2MTM2NCIsMjI1NTFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=WzUsInR0NTgzNzQyMCIsMjI2MDFd&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDk4ODQwMjQiLDIyNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDk3NTAxNzYiLDIyNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDk1OTIyOTIiLDIyNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDk0NTEzNTIiLDIyODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDkzMTQwODYiLDIyODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDkxOTMwNTYiLDIyOTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDkwNDczNzgiLDIyOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDg5Mzg2ODIiLDIzMDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDg4MjcwNDQiLDIzMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDg3MjM0MjIiLDIzMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDg2NTE3MjgiLDIzMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDg1NTk0NTIiLDIzMjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDg0NzYyODQiLDIzMjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDgzODczODIiLDIzMzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDgzMzgyNjQiLDIzMzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDgyNjI2MDgiLDIzNDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDgyMTkxMTAiLDIzNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDgxMTY1NDAiLDIzNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDgwMTA1MzYiLDIzNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDc5MzI5MjIiLDIzNjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDc4MTY5ODYiLDIzNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDc3MjAxMzYiLDIzNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDc2Mjg1MDYiLDIzNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDc1NDAxOTgiLDIzODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDc0Njc0MzQiLDIzODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDczNTg3MzYiLDIzOTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDcyNTg2OTAiLDIzOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDcxNjUzODQiLDI0MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDcxMTE2MjYiLDI0MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDcwNDUyMzYiLDI0MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY5NTE4OTgiLDI0MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY4NzMzODYiLDI0MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY4MDI0MzIiLDI0MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY3MjkwNDgiLDI0MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY2NDg4NjgiLDI0MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY1ODg1MzYiLDI0NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY1MzUzMzQiLDI0NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY0OTAwNTYiLDI0NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDY0MzE4NzAiLDI0NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDYzODkzMjQiLDI0NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDYzNjE3ODAiLDI0NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDYyNzkwMDQiLDI0NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDYxNzk2NzgiLDI0NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDYwOTg3NDAiLDI0ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDYwNDMzMzIiLDI0ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU5NjAyNzYiLDI0OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU4ODE0OTIiLDI0OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU4MjEyNjIiLDI1MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU3NDcxMTIiLDI1MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU2NjE5NTQiLDI1MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU1ODQ3OTAiLDI1MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU1MTY4NzAiLDI1MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDU0NDc0NjAiLDI1MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDUzMjMxNzIiLDI1MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDUyNjE5NjgiLDI1MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDUyMDMyOTQiLDI1NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDUxMzAyNTQiLDI1NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDUwNDc3OTAiLDI1NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ5NTMyNTAiLDI1NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ4NzcwMDQiLDI1NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ3NzA3NjgiLDI1NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ2ODIyMTAiLDI1NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ1ODg2MzIiLDI1NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ1MDc1NDYiLDI1ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQ0MDc5MjAiLDI1ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQzMTM2ODQiLDI1OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQyMTI1MzYiLDI1OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQxMjY5MjAiLDI2MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDQwNTQ1ODIiLDI2MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM5ODEzODIiLDI2MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM4ODI0MDAiLDI2MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM4MjIzMDQiLDI2MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM3NTk5ODYiLDI2MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM2ODkwNzYiLDI2MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM2Mjc5NDIiLDI2MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM1NzcyOTYiLDI2NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM1MDI5NzYiLDI2NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDM0Mzg5MjgiLDI2NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDMzNzk3MzYiLDI2NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDMzMTI2MTIiLDI2NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDMyNDQyNzgiLDI2NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDMxNzkyNDQiLDI2NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDMxMDczODAiLDI2NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDMwMDY4MTAiLDI2ODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDI4OTIwNTgiLDI2ODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDI3MzM2OTQiLDI2OTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDI2MjkwOTQiLDI2OTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDI1MzI0ODgiLDI3MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDI0NDUyNDgiLDI3MDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDIzOTMyMTIiLDI3MTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDIzNTM5MDQiLDI3MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDIyOTI1NTciLDI3MjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDIyMzI0MDAiLDI3MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDIxNTMxMjIiLDI3MzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDIwNzc4MzQiLDI3MzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE5ODg1ODEiLDI3NDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE5MTA1MTkiLDI3NDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE4Mzg3MTAiLDI3NTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE3NzYzMzIiLDI3NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE3MTIwODEiLDI3NjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE2NDMyNDgiLDI3NjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE2MTEyMjUiLDI3NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1ODgxNjUiLDI3NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1NzI5NDI4IiwyNzgwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1NjI1OTAwIiwyNzg1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1NTA1Mjk0IiwyNzkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1NDM3OTY2IiwyNzk1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1MzQ0MDUiLDI4MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1MjQ4ODkyIiwyODA1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1MTcwODMwIiwyODEwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE1MDgwNTgwIiwyODE1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0OTg0Mjc2IiwyODIwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0ODUyNzkyIiwyODI1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0NzM0ODM4IiwyODMwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0NjQ3OTQwIiwyODM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0NTYwMjk0IiwyODQwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0NDQ5NDEwIiwyODQ1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0MzMxMjIwIiwyODUwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0MjQwMTEiLDI4NTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0MTgwNzQ0IiwyODYwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0MDk5Nzk2IiwyODY1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDE0MDA1MTEiLDI4NzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzOTE3OTUiLDI4NzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzODAxMTcwIiwyODgwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzNjg1NDU2IiwyODg1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzNTc0NjgwIiwyODkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzNDcyOTQwIiwyODk1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzMzczNjIiLDI5MDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzMjQxNTQyIiwyOTA1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzMTMwMTkwIiwyOTEwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEzMDI1ODEiLDI5MTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyODgwODA0IiwyOTIwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyNzM4MTQiLDI5MjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyNTkzNTcyIiwyOTMwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyNDQwNzg2IiwyOTM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyMzQyODkwIiwyOTQwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyMTcwMTY4IiwyOTQ1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEyMDYxMzM4IiwyOTUwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExOTQxMjM4IiwyOTU1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExNzkyMzQyIiwyOTYwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExNjU1MjQ0IiwyOTY1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExNTYzNzQwIiwyOTcwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExNDM0MjQ4IiwyOTc1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExMzExNzU0IiwyOTgwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExMjE4MzU4IiwyOTg1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExMTA5NDI2IiwyOTkwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDExMDEyMzE4IiwyOTk1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwOTMxMTE0IiwzMDAwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwODA2MDQwIiwzMDA1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwNjg2NTk2IiwzMDEwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwNTM1MzMyIiwzMDE1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwNDMzMzIyIiwzMDIwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwMzQwODA0IiwzMDI1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwMjUzNTEwIiwzMDMwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwMTc3Mzg4IiwzMDM1MV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDEwMDgwOTk4IiwzMDQwMV0%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA5MTEwMDkiLDMwNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA3ODc1MjEiLDMwNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0OTUyMDQiLDMwNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0NzM1NTQiLDMwNjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0NjExMjciLDMwNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0NDc0MjciLDMwNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0NDIwNTUiLDMwNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0MzQ3ODEiLDMwODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0MjU2NDIiLDMwODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDA0MTAxNzAiLDMwOTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzOTA2NzYiLDMwOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzNzg3MTEiLDMxMDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzNjg5NzgiLDMxMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzNTkyNTAiLDMxMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzNTE2MTkiLDMxMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzNDU1MjIiLDMxMjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzMzkxMzEiLDMxMjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzMzA4MDgiLDMxMzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzMjQ2MDUiLDMxMzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzMTYwMDAiLDMxNDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAzMDgxNzUiLDMxNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyOTQ5MTUiLDMxNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyODk5NTgiLDMxNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyODU3MDciLDMxNjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyNzYxOTIiLDMxNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyNjUzOTAiLDMxNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyNTg4MTgiLDMxNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyNTMxMDMiLDMxODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyMzkxMzgiLDMxODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyMjY0MDQiLDMxOTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyMTY4MDQiLDMxOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyMDcxMDEiLDMyMDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAyMDE4MDQiLDMyMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxOTM4NzQiLDMyMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxODY1NjEiLDMyMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxNzU3MzYiLDMyMjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxNjU4ODYiLDMyMjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxNTg5NDYiLDMyMzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxNTIyNjQiLDMyMzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxMzk4NjAiLDMyNDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAxMjk5MTkiLDMyNDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwOTc2MjIiLDMyNTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwNjcyOTEiLDMyNTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwNTIxNjYiLDMyNjAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwNDE1NzUiLDMyNjUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMzA2NDYiLDMyNzAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMjYzNTEiLDMyNzUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMjI4NjUiLDMyODAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMjAxNDkiLDMyODUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMTgyMjEiLDMyOTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMTYxMjAiLDMyOTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMTM4NDAiLDMzMDAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMTE4MzQiLDMzMDUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMTAzMDMiLDMzMTAxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMDgzMTIiLDMzMTUxXQ%3D%3D&ref_=adv_nxt",
             "https://www.imdb.com/search/title/?title_type=feature&genres=crime&sort=num_votes,desc&after=Wy05MjIzMzcyMDM2ODU0Nzc1ODA4LCJ0dDAwMDYyMTgiLDMzMjAxXQ%3D%3D&ref_=adv_nxt")

#33250 last obs

for (i in new_url){
  url_page <- read_html(i)
  Rank <- as.numeric(gsub(",","",html_nodes(url_page,'.text-primary') %>% html_text()))
  Votes <- ifelse(Rank>=0,html_nodes(url_page,'.sort-num_votes-visible span:nth-child(2)') %>% html_text(),NA)
  Names <- html_nodes(url_page,'.lister-item-header a') %>% html_text()
  Year <- ifelse(Rank>=0,gsub("I","",gsub("II ","",gsub("I ","",gsub("[()]", "",html_text(html_nodes(url_page, '.text-muted.unbold')))))),NA)
  Content <- ifelse(Rank>=0,html_nodes(url_page,'.text-muted .certificate') %>% html_text(),NA)
  Length <- ifelse(Rank>=0,gsub(" min","",html_nodes(url_page,'.text-muted .runtime') %>% html_text()),NA)
  Genre <- str_trim(gsub("\n","",html_nodes(url_page,'.genre') %>% html_text()))
  Rating <- ifelse(Rank>=0,html_nodes(url_page,'.inline-block.ratings-imdb-rating strong') %>% html_text(),NA)
  Metascore <- ifelse(Rank>=0,str_trim(html_nodes(url_page,'.inline-block.ratings-metascore span') %>% html_text()),NA)
  Description <- ifelse(Rank>=0,gsub("\n","",html_nodes(url_page,'.ratings-bar + .text-muted') %>% html_text()),NA)
  Director <- ifelse(Rank>=0,html_nodes(url_page,'.text-muted + p a:nth-child(1)') %>% html_text(),NA)
  Star <- ifelse(Rank>=0,html_nodes(url_page,'.lister-item-content .ghost+ a') %>% html_text(),NA)
  CoStar1 <- ifelse(Rank>=0,html_nodes(url_page,'.lister-item-content .ghost+ a+a') %>% html_text(),NA)
  CoStar2 <- ifelse(Rank>=0,html_nodes(url_page,'.lister-item-content .ghost+ a+a+a') %>% html_text(),NA)
  CoStar3 <- ifelse(Rank>=0,html_nodes(url_page,'.lister-item-content .ghost+ a+a+a+a') %>% html_text(),NA)
  Gross <- ifelse(Rank>=0,substring(gsub("M","",html_nodes(url_page,'.ghost~ .text-muted+ span') %>% html_text()),2,7),NA)
  new_total_data <- rbind(new_total_data,data.frame(Names,Rank,Year,Content,
                                            Length,Genre,Rating,Metascore,Description,
                                            Director,Star,CoStar1,CoStar2,CoStar3,Votes,Gross))
}

#new_total_data2 <- new_total_data[!duplicated(new_total_data),]
setwd("~/Desktop/STAT6440/Text Mining")
crime_org <- read.csv("crime_org.csv")
crime_org$X <- NULL
crime_new_total <- rbind(crime_org,new_total_data)
#write.csv(crime_new_total,"crime_new.csv")
#cri <- data.frame(table(crime_new_total$Rank))
### Notes
# How cluster changes by year?
# Different tokenizing options
# Apply the clustering on everything, no need to group by attribute
# For different clusters, try to find the mean rating or metascore (public preference?)

