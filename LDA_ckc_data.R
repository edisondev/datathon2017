
library(tm)
setwd('C:\\Users\\Nick\\Dropbox\\Work\\Data Science\\13 - Data for Good Datathon\\scripts\\datathon2017')

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

lda_num_topics=15
lda.mod =LDA.analysis(text_vector = dfnew,num_topics = lda_num_topics)


#Get term weights
require(data.table)
#library(wordcloud)
library(ggplot2)
library(ggrepel)

term_weights=topicmodels::posterior(lda.mod$lda_model)$terms
term_weight=data.table(term_weights)
for (i in 1:dim(n)[1]) {
  words=return_list$topicTerms[,i]
  weights=unname(sort(term_weight[i,],decreasing=TRUE)[1,1:30])
  dfwc=data.frame(words,t(weights))
  png(filename = paste(i, ".png", sep=""))
  wordcloud(dfwc$words, 
            dfwc$t.weights.,
            random.order=FALSE,
            colors=brewer.pal(8, "Dark2"))
  dev.off()
}









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
