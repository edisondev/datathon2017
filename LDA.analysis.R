#Custom Functions
#removes html string
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#removes &...; string
cleanFun2 <- function(htmlString) {
  return(gsub("&.*?;", "", htmlString))
}


LDA.analysis <- function(text_vector, num_topics=10 ) {
  require(tm)
  require(Rmpfr)
  require(tidyr)
  
  docs <- Corpus(VectorSource(text_vector))  
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
  
  return(list("topicLabel"=topicLabel, 
              "topicTerms"=llis.terms,
              "lda_model"=llis.model))
}