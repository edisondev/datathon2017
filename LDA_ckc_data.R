
library(tm)
setwd('C:\\Users\\Nick\\Dropbox\\Work\\Data Science\\13 - Data for Good Datathon\\scripts')

source("LDA.analysis.R")
source("topicmodels_json_ldavis.R")



df=read.csv('ckc_calgary_mega_export.txt', sep = "\t",
            stringsAsFactors = FALSE)

#Clean Data
#Pull only columns of interest into a new dataframe
dfn=df['nid']
dfn$Title=df$Title
dfn$WhyWeExist=trimws(cleanFun2(cleanFun(df$Why.We.Exist)), which = "both")
dfn$WhatWeDo=trimws(cleanFun2(cleanFun(df$What.We.Do)), which = "both")
dfn$HowWeDoIT=trimws(cleanFun2(cleanFun(df$How.We.Do.It)), which = "both")
dfn$WhatYouCanDo=trimws(cleanFun2(cleanFun(df$What.You.Can.Do)), which = "both")
dfn$Website=df$website
dfn$Email=df$email
dfn$field_charitable_number_value=df$field_charitable_number_value
dfn$Phone=df$field_org_phone_value
dfn$Representative=df$Representative
dfn$Annual_budget=df$field_org_annual_budget_value




dfnew=paste(dfn$WhyWeExist, 
                   dfn$WhatWeDo, 
                   dfn$HowWeDoIT,
                   sep=" ")

docs <- Corpus(VectorSource(dfnew))  
docs <- tm_map(docs, removePunctuation) 
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("french"))
docs <- tm_map(docs, stemDocument) #remove endings
#docs <- tm_map(docs, removeWords, c("program","programs","services","service","provide", "community","support","calgary","alberta","canadian","canada","edmonton"))
dtm <- DocumentTermMatrix(docs) 

#remove duplicate documents:
ui = unique(dtm$i)
dtm.new = dtm[ui,]



# #Caculate optimal number of topics: http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html
library(Rmpfr)
term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))
summary(term_tfidf)
# 
#remove words
llisreduced.dtm <- dtm[,term_tfidf >= 0.005]
summary(slam::col_sums(llisreduced.dtm))
# 
# harmonicMean <- function(logLikelihoods, precision = 2000L) {
#   llMed <- median(logLikelihoods)
#   as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
#                                        prec = precision) + llMed))))
# }
# seqk <- seq(2, 100, 1)
# burnin <- 1000
# iter <- 1000
# keep <- 50
# 
# system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(llisreduced.dtm, k = k,
#                                                                      method = "Gibbs",control = list(burnin = burnin,
#                                                                                                      iter = iter, keep = keep) )))
# logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
# hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
# optimal_topic_number <-  seqk[which.max(hm_many)]  #- optimal topics number
optimal_topic_number <-15
system.time(llis.model <- topicmodels::LDA(llisreduced.dtm, optimal_topic_number, method = "Gibbs", control = list(iter=2000, seed = 0622)))


#Create labels for topics  - join 3 most common words
library(tidyr)
llis.topics <- topicmodels::topics(llis.model, 1)
llis.terms <- as.data.frame(topicmodels::terms(llis.model, 30), stringsAsFactors = FALSE)

topicTerms <- tidyr::gather(llis.terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:30))
topTerms <- dplyr::filter(topicTerms, Rank < 5)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)
topicLabel <- data.frame()
for (i in 1:optimal_topic_number){
  z <- dplyr::filter(topTerms, Topic == i)
  l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2],z[4,2], sep = " " ), stringsAsFactors = FALSE)
  topicLabel <- rbind(topicLabel, l)
}

colnames(topicLabel) <- c("Label")
topicLabel



#Match topics back to organization
theta <- as.data.frame(topicmodels::posterior(llis.model)$topics)
head(theta)


#Map Charity to each Topic
colnames(theta)<-t(topicLabel)
classification=colnames(theta)[max.col(theta,ties.method="first")]
dfn$CharityType=factor(classification, level=topicLabel$Label)


#Make Key and join dataframes
x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
colnames(x) <- c("Key")
x$Key <- as.numeric(x$Key)
theta2 <- cbind(x, theta)

dfn$Key <- row.names(dfn)
dfn$Key<- as.numeric(dfn$Key)
dffull <- dplyr::left_join(dfn,theta2, by = "Key")

write.table(x=dffull, 
            file="ckc_calgary_mega_export.csv", 
            quote=TRUE,
            sep=",")


## Plot the charity distribution
# library(ggplot2)
# 
# 
# dftemp=dffull
# levels(dftemp$CharityType)=names(sort(summary(dftemp$CharityType)))
# 
# ggplot(data=dftemp, aes(x=CharityType)) +
#   geom_bar( stat="count")+
#   coord_flip()
# 
# library(plyr)
# dfcount=count(dftemp, 'CharityType')
# dfcount <- transform(dfcount, variable=reorder(freq, -value) ) 
# 
# dfcount$CharityType=as.character(dfcount$CharityType)
# dfcount=dfcount[order(dfcount$freq),]
# 
# 
# dfcount=dfcount[order(dfcount$freq),]
# rownames(dfcount) <- 1:nrow(dfcount)
#ggplot(data=dfcount, aes(x=CharityType, y=freq)) + geom_point()+coord_flip()

  #geom_bar( stat='identity')+
  #coord_flip()

#k=summary(dffull$CharityType)
#ggplot(aes(x = names(k), y = k)) + theme_bw() + geom_bar(stat = "identity")
