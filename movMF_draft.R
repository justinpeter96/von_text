rm(list=ls())
# Start writing on March, wrap  up  the project by February
## Libraries ##

library(dplyr)
library(rvest)
library(stringr)
library(xml2)
library(lattice)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(tm)
library(SnowballC)
library(slam)
library(movMF)
library(tidytext)

setwd("~/Desktop/STAT6440/Text_Mining")
#comedy <- read.csv("comedy_crime.csv")
crime <- read.csv("crime_new.csv")
crime$Content1 <- str_split_fixed(crime$Genre,",",3)[,1]
crime$Content2 <- str_split_fixed(crime$Genre,",",3)[,2]
crime$Content3 <- str_split_fixed(crime$Genre,",",3)[,3]
rating_subset8 <- subset(crime,Rating>8)
rating_subset8 <- subset(rating_subset8,rating_subset8$Description != "        Add a Plot")
draft <- read.csv("draft_crime.csv")
#draft2 <- subset(draft,Include=="y")


#proportion <- read.csv("proportion5.csv")
# Short listed group of words, looking at frequency of words and find it in terms of the appearance in various movies, blanket removal of high or low frequency words, manually checking through the combination of a word and its subs  (blood and bloody)
#crime_rating <- read.csv("crime_rating.csv")
crime_rating2 <- data_frame(Text=rating_subset8_2$Description) %>%
  unnest_tokens(output=word,input=Text) %>%
  count(word,sort=TRUE)
par(mar=c(1, 1, 1, 1))
wordcloud(crime_rating2$word,crime_rating2$n,min.freq = 1,max.words=50)
# CONTINUE on the word selection and then form genre sub groups for every cluster
# Low rate of movies that may give a cluster that connects us with a description in terms of popularity
# Take the entire dataset, cluster to crime only with rating at least 7, test between 6 and up
# Stick with entire data, start with rating > 8, look into frequencies then move into manual word choices
#movie_description <- read.csv("movie_description.csv")
#table_word <- read.csv("merge_words.csv")
#table_word$percentage <- ifelse(table_word$Proportion <0.02,"Rid","Not Rid")
#table_word2 <- subset(table_word,percentage=="Not Rid")
proportion2 <- subset(draft,Include=="")
stopwords <- proportion2[,2]
stopwords2 <- str_to_title(stopwords)
x <- rating_subset8$Description
x2 <- removeWords(x,stopwords)
x3 <- removeWords(x2,stopwords2)
x4 <- removePunctuation(x3)
#x5 <- removeWords(x4,c("friends","criminal","gangster"))
x5 <- gsub("men","man",gsub("policemen","police",gsub("criminal","crime",gsub("friends","friend",x4))))
rating_subset8$Description <- x5
#y <- rating_subset8$Names
#y2 <- removeWords(y,stopwords)
#y3 <- removeWords(y2,stopwords2)
#y4 <- removePunctuation(y3)
#rating_subset8$Names <- y4

#crime_words <- data.frame(Text=crime$Description) %>% unnest_tokens(output=word,input=Text) %>% count(word,sort=TRUE)
my_vec <- character()
for (i in crime_words[,1]){
  my_out <- nrow(crime %>% filter(str_detect(Description,i)))
  my_vec <- c(my_vec,my_out)
}
my_vec2 <- as.data.frame(my_vec)
crime_words_add <- cbind(crime_words,my_vec2)
colnames(crime_words_add)[3] <- "Number of Movies"
crime_words_add$`Number of Movies` <- as.numeric(crime_words_add$`Number of Movies`)
crime_words_add2 <- crime_words_add %>% mutate(Proportion = `Number of Movies`/200)


#crime$Content1 <- str_split_fixed(crime$Genre,",",3)[,1]
#crime$Content2 <- str_split_fixed(crime$Genre,",",3)[,2]
#crime$Content3 <- str_split_fixed(crime$Genre,",",3)[,3]
#crime$Genre <- gsub("Crime","",crime$Genre)
#crime$Description <- gsub("\\..*","",crime$Description)
#year <- subset(crime,Year>=2000 & Year<=2020)
#crime_first <- subset(year,year$Description != "        Add a Plot")
rating_subset8_2 <- rating_subset8[-c(112,126,168,181,198,280,299,312,351,357,366,383,385),]
#crime_titles <- apply(rating_subset8[,c("Names","Description")],1, 
#                      paste, collapse = " ")
#crime_corpus <- VCorpus(VectorSource(crime_titles))
crime_corpus <- VCorpus(VectorSource(rating_subset8_2$Description))
crime_DTM <-DocumentTermMatrix(crime_corpus,control = list(tokenize = "words", stopwords = FALSE, stemming = FALSE,
                                                           wordLengths = c(3, Inf))) 
# Keep stemming = TRUE
ColSums <- col_sums(crime_DTM > 0)
sort(ColSums, decreasing = TRUE)[1:20]
#crime_DTM2 <-crime_DTM[, ColSums >= 0 & ColSums <= 250] 
crime_IDF <- weightTfIdf(crime_DTM)
#rowTotals <- slam::row_sums(crime_IDF)
#crime_IDF <- crime_DTM2[rowTotals > 0, ]

n <- 114
K <- 10
BIC <- rep(NA,K) # BIC - penalized loglik
BIC_ck <- rep(NA,K)
for ( k in 1:K){
  m <- movMF(crime_IDF, k = k, nruns = 20)
  mpar <- dim(m$theta)[1]*dim(m$theta)[2]+(length(m$alpha)-1)
  BIC[k] <- -2*m$ll+mpar*log(n)
  m <- movMF(crime_IDF, k = k, nruns = 20, kappa = list(common = TRUE))
  BIC_ck[k] <- -2*m$ll+(mpar-k+1)*log(n)
}





set.seed(2022)
Ks <- c(1:5,10)
splits <- sample(rep(1:10, length.out = nrow(crime_IDF)))
crime_movMF <- lapply(Ks, function(k) sapply(1:10, function(s) {
    m <- movMF(crime_IDF[splits != s, ], k = k, nruns = 20)
    logLik(m, crime_IDF[splits == s, ])}))

crime_movMF_common <- lapply(Ks, function(k)sapply(1:10, function(s) {
        m <- movMF(crime_IDF[splits != s, ],
                   k = k, nruns = 20, kappa = list(common = TRUE))
        logLik(m, crime_IDF[splits == s, ])}))

logLiks <- data.frame(logLik = c(unlist(crime_movMF),
                                 unlist(crime_movMF_common)),
                      K = c(rep(Ks, sapply(crime_movMF, length)),
                            rep(Ks, sapply(crime_movMF_common, length))),
                      Dataset = seq_len(length(crime_movMF[[1]])),
                      Method = factor(rep(1:2, each = length(unlist(crime_movMF))),
                                      1:2, c("free", "common")))
logLiks$logLik <- logLiks$logLik - rep(rep(with(logLiks, tapply(logLik, Dataset, mean)), length(Ks)), 2)
print(xyplot(logLik ~ K | Method, data = logLiks, groups = Dataset, type = "l", lty = 1, 
             xlab = "Number of components", ylab = "Predictive log-likelihood",           
             strip = strip.custom(factor.levels  = 
                                    expression(paste("Free ", kappa), paste("Common ", kappa)))))

print(xyplot(logLik ~ K | Method, data = logLiks, groups = Dataset, type = "l", lty = 1, 
             xlab = "Number of components", ylab = "Predictive log-likelihood"))

logLiks2 <- logLiks %>% filter(Method=="free")
logLiks2 %>% ggplot(aes(K,logLik,group=Dataset)) + geom_line() + xlab("Number of Components") + 
  ylab("Predictive log-likelihood") + theme_bw(base_size=14) + 
  theme(plot.title = element_text(size = 17, face = "bold",hjust=0.5)) + ggtitle("Optimal Number of Clusters")

print(xyplot(logLik ~ K, data = logLiks2, groups = Dataset, type = "l", lty = 1,
             xlab = "Number of components", ylab = "Predictive log-likelihood")) 

set.seed(0)
best_model <- movMF(crime_IDF, k = 4, nruns = 20,kappa = list(common = TRUE))
apply(coef(best_model)$theta, 1, function(x) colnames(coef(best_model)$theta)[order(x,decreasing = TRUE)[1:10]])
clustering <- predict(best_model)
keywords <- rating_subset8_2[, 7]
rating_subset8_2$Clustering <- clustering
keywords <- sapply(keywords, function(x)sapply(strsplit(x, ", ")[[1]], function(y) strsplit(y, "-")[[1]][1])) 
tab <- table(Genre = unlist(keywords),Cluster = rep(clustering, sapply(keywords, length)))
tab
tab <- table(Genre = unlist(keywords),Year = rep(rating_subset8_2$Year2,sapply(keywords,length)))
round(prop.table(tab,1),2)
rating_subset8_2$Votes <- as.numeric(gsub(",","",rating_subset8_2$Votes))
#rating_subset8_2$Votes <- as.numeric(rating_subset8_2$Votes)
rating_subset821 <- rating_subset8_2 %>% filter(Clustering==1) %>% select(Names,Year,Genre,Votes) %>% arrange(desc(Votes)) %>% head(5)
rating_subset822 <- rating_subset8_2 %>% filter(Clustering==2) %>% select(Names,Year,Genre,Votes) %>% arrange(desc(Votes)) %>% head(5)
rating_subset823 <- rating_subset8_2 %>% filter(Clustering==3) %>% select(Names,Year,Genre,Votes) %>% arrange(desc(Votes)) %>% head(5)
rating_subset824 <- rating_subset8_2 %>% filter(Clustering==4) %>% select(Names,Year,Genre,Votes) %>% arrange(desc(Votes)) %>% head(5)
#legend.box.background = element_rect()
ggplot(b2, aes(Year, Frequency, fill=Genre)) + 
  geom_bar(stat="identity",position="dodge") + coord_flip() + theme_bw(base_size=14) + theme(legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size=10),plot.title = element_text(size = 20, face = "bold",hjust=0.5)) + ggtitle("Genre Frequency by Year")
#ggplot(as.data.frame(tab), aes(Cluster, Freq, fill=Genre)) + 
#  geom_bar(stat="identity",position="dodge") + coord_polar() + facet_grid(.~Cluster)
ggplot(b2, aes(Genre, Frequency,fill=Cluster)) + 
  geom_bar(stat="identity",position="dodge") + coord_flip() + theme_bw(base_size=14) + theme(legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size=10),plot.title = element_text(size = 20, face = "bold",hjust=0.5)) + ggtitle("Genre Frequency by Cluster")
b <- as.data.frame(tab)
b2 <- b %>% filter(Genre != "Crime")
b2 <- b %>% filter(Genre %in% c("Action","Comedy","Drama","Mystery","Thriller"))
colnames(b)[3] <- "Frequency"
b2$Cluster <- ifelse(b2$Cluster==1,"Investigative Journalism/City Crimes",
       ifelse(b2$Cluster==2,"Police Procedural/Family Crimes",
              ifelse(b2$Cluster==3,"Relationships/Courtroom","Organized Crime/Coming of Age")))
rating_subset8_2$Year <- as.numeric(rating_subset8_2$Year)
rating_subset8_2$Year2 <- ifelse(rating_subset8_2$Year == 2021,"After 2020",
                                 ifelse(rating_subset8_2$Year < 2021 & rating_subset8_2$Year > 2009,"2010-2020",
                                        ifelse(rating_subset8_2$Year < 2010 & rating_subset8_2$Year > 1999,"2000-2009",
                                               ifelse(rating_subset8_2$Year < 2000 & rating_subset8_2$Year > 1989,"1990-1999",
                                                      ifelse(rating_subset8_2$Year < 1990 & rating_subset8_2$Year > 1979,"1980-1989",
                                                             ifelse(rating_subset8_2$Year < 1980 & rating_subset8_2$Year > 1969,"1970-1979",
                                                                    ifelse(rating_subset8_2$Year < 1970 & rating_subset8_2$Year > 1959,"1960-1969","Before 1960")))))))

table(rating_subset8_2$Year2,rating_subset8_2$Clustering)[,1:4]/sum(table(rating_subset8_2$Year2,rating_subset8_2$Clustering)[,1:4])
table(rating_subset8_2$Year2,rating_subset8_2$Clustering)[4,]/sum(table(rating_subset8_2$Year2,rating_subset8_2$Clustering)[4,])
round(prop.table(table(rating_subset8_2$Year2,rating_subset8_2$Clustering),1),2)
table(rating_subset8_2$Year2,rating_subset8_2$Clustering)
b <- as.data.frame(table(rating_subset8_2$Year2,rating_subset8_2$Clustering))
colnames(b)[1] <- "Year"
colnames(b)[2] <- "Cluster"
colnames(b)[3] <- "Frequency"
b$Cluster <- ifelse(b$Cluster==1,"Investigative Journalism/City Crimes",
                     ifelse(b$Cluster==2,"Police Procedural/Family Crimes",
                            ifelse(b$Cluster==3,"Relationships/Courtroom","Organized Crime/Coming of Age")))
ggplot(b, aes(Cluster, Frequency, fill=Year)) + 
  geom_bar(stat="identity",position="dodge") + coord_flip() + theme_bw(base_size=14) + theme(legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size=10),plot.title = element_text(size = 20, face = "bold",hjust=0.5)) + ggtitle("Cluster Frequency by Year")
cluster1 <- rating_subset8_2 %>% filter(Clustering==1) 
cluster1 <-  data_frame(Text=cluster1$Description) %>%
  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
  count(word,sort=TRUE) 
cluster1$cluster <- 1
#cluster1_more <-  data_frame(Text=cluster1$Description) %>%
#  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
#  anti_join(stop_words) %>%
#  count(word,sort=TRUE)
cluster2 <- rating_subset8_2 %>% filter(Clustering==2) 
cluster2 <-  data_frame(Text=cluster2$Description) %>%
  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
  count(word,sort=TRUE) 
cluster2$cluster <- 2
#cluster2_more <-  data_frame(Text=cluster2$Description) %>%
#  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
#  anti_join(stop_words) %>%
#  count(word,sort=TRUE)
cluster3 <- rating_subset8_2 %>% filter(Clustering==3) 
cluster3 <-  data_frame(Text=cluster3$Description) %>%
  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
  count(word,sort=TRUE) 
cluster3$cluster <- 3
cluster3_more <-  data_frame(Text=cluster3$Description) %>%
  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
  anti_join(stop_words) %>%
  count(word,sort=TRUE)

#### PAPER OUTLINE
# THESIS = Sectioins(Intro(have what I want, literature survey(what work has been done, past research, like journals or papers)))
# Also in thesis, have methodologies, describe the model, use the paper for reference, explain parameters
# Also, third section includes application or results or both
# Results will have subsectioins that include descrpition of the data, how you scraped the data, second section is more about pre-processing the data
# Pre-processiinig isi all about the low frequency word removal, manual word removal, similar words like blood and bloody being combined, describe it
# Use bullet points to break down the steps in the pre-processing
# Showcase actual results where each cluster is to be described, this part is the main highlights, where the established methodologies lead to that conclusiioin
# Use the frequency/proportion tables to explain the info on each description on the cluster
# Try to include visuals, similar to story tellinig , like the last project
# Conclusion, Actually Discussion, so exactly 5 chapters or 40 pages of writing, maybe including the potential future questions
# Start writing, it will take time, defense will come closer, set date for the writing to be done by fiirst week of April, then use the next days to prepare the slide deck
# Other requests include sending the first version draft of writing, start with the results section to be sent by the end of this week, SAT night, SUN it will be read, MON discussion
# Methodologies will request help and use of various papers in explaining the theory
# Also that part starts with a descriiption of mixture modeling
# Write the abstract
# With the paper sent, read the intro and that gives me a way of writing the intro of the paper
# In the methods sectioin, go to 2.1 from that paper
# We wiill start writing with LATEX (math equations)
# Find if there is a specific template regarding the paper, ask the graduate department



cluster4 <- rating_subset8_2 %>% filter(Clustering==4) 
cluster4 <-  data_frame(Text=cluster4$Description) %>%
  unnest_tokens(output=word,input=Text,token="ngrams",n=2) %>%
  count(word,sort=TRUE) 
cluster4$cluster <- 4
clusters <- rbind(cluster1,cluster2,cluster3,cluster4)
clusters_new <- clusters %>% group_by(word,cluster) %>% summarise(new_n = n)
clusters_new2 <- clusters_new[c(435:445),]
tapply(clusters_new2$new_n,clusters_new2$cluster,FUN=sum)

total_data <- data.frame()
for (i in unlist(data.frame(apply(coef(best_model)$theta, 1, function(x) colnames(coef(best_model)$theta)[order(x,decreasing = TRUE)[1:10]])))){
  b <- data.frame(rep(i,4))
  total_data <- rbind(total_data,data.frame(b))
}
total_data$cluster = rep(c(1,2,3,4),40)
clusters$cluster <- NULL
colnames(total_data)[1] <- "word"
# some frequencies involve combination of words, law and lawyer, girlfriend for friend and girl
# also duplicates of words can be present so for frequencies, one is taken out, so family and life
total_data$n <- c(30,14,5,4,13,1,1,0,10,3,1,9,10,2,3,4,7,5,0,1,
                  4,1,2,0,4,1,0,0,3,2,0,0,3,1,0,1,4,0,1,1,
                  3,37,2,2,2,26,5,9,1,20,3,8,0,18,1,3,0,12,1,1,
                  2,11,6,2,2,11,2,1,2,15,1,1,4,21,6,6,4,17,4,4,
                  6,3,50,7,0,5,40,7,0,1,19,0,0,2,7,1,6,9,16,28,
                  2,10,9,2,0,0,5,1,1,0,5,0,2,4,5,2,1,0,4,0,
                  3,6,4,62,6,9,16,28,5,10,2,22,0,1,1,9,2,4,2,10,
                  2,2,2,7,2,2,1,5,1,20,3,8,0,0,2,6,0,0,0,6)
#write.csv(total_data,"frequency_distributions.csv")
#write.csv(clusters_new,"combined_applied_distributions.csv")

frequencies <- read.csv("frequency_distributions.csv")
frequencies$X <- NULL
frequencies2 <- frequencies[-c(125:128,149:152),]
frequencies3 <- frequencies2[order(frequencies2$cluster,frequencies2$word),]
replace(table(frequencies3$word,frequencies3$cluster),1:152,frequencies3$n)
b <- as.data.frame(frequencies3)
colnames(b)[1] <- "Word"
colnames(b)[2] <- "Cluster"
colnames(b)[3] <- "Frequency"
b$Cluster <- ifelse(b$Cluster==1,"Investigative Journalism/City Crimes",
                    ifelse(b$Cluster==2,"Police Procedural/Family Crimes",
                           ifelse(b$Cluster==3,"Relationships/Courtroom","Organized Crime/Coming of Age")))
b2 <- b %>% filter(Word %in% c("crime","man","young","police","murder","life","story","friend","gang","family"))

ggplot(b2, aes(Word, Frequency, fill=Cluster)) + 
  geom_bar(stat="identity",position="dodge") + coord_flip() + theme_bw(base_size=14) + theme(legend.position = "bottom",legend.title=element_blank(),legend.text = element_text(size=10),plot.title = element_text(size = 20, face = "bold",hjust=0.5)) + ggtitle("Word Frequency by Cluster")

total_data <- data.frame()
for(i in unique(frequencies3[,1])){
  e <- i
  d <- rating_subset8_2[grep(i,rating_subset8_2$Description),][,4]
  total_data <- rbind(total_data,data.frame(d,e))
}
total_data$Year <- ifelse(total_data$d == 2021,"After 2020",
                                 ifelse(total_data$d < 2021 & total_data$d > 2009,"2010-2020",
                                        ifelse(total_data$d < 2010 & total_data$d > 1999,"2000-2009",
                                               ifelse(total_data$d < 2000 & total_data$d > 1989,"1990-1999",
                                                      ifelse(total_data$d < 1990 & total_data$d > 1979,"1980-1989",
                                                             ifelse(total_data$d < 1980 & total_data$d > 1969,"1970-1979",
                                                                    ifelse(total_data$d < 1970 & total_data$d > 1959,"1960-1969","Before 1960")))))))
sum(rowSums(replace(table(frequencies3$word,frequencies3$cluster),1:152,frequencies3$n)))
g <- as.data.frame(table(total_data$e,total_data$Year))
ggplot(g, aes(Var1, Freq,group=Var2,colour=Var2)) + 
  geom_line()

rating_subset8_2 %>% filter(Clustering==1) %>% select(Names,Year,Genre)
rating_subset8_2 %>% filter(Clustering==2) %>% select(Names,Year,Genre)
rating_subset8_2 %>% filter(Clustering==3) %>% select(Names,Year,Genre)
rating_subset8_2 %>% filter(Clustering==4) %>% select(Names,Year,Genre)
