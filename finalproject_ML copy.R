library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(sentimentr)
library(dplyr)
library(RCurl)
library(RWekajars)

url <- "https://www.amazon.com/Match-Mens-Wild-Cargo-Pants/product-reviews/B0117RFPCC/ref=cm_cr_arp_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber="
N_pages <- 204 
a <- NULL 
for (j in 1: N_pages){ 
  pant <- read_html(paste0(url, j)) 
  b <- cbind(pant %>% html_nodes(".review-text") %>%     html_text()     ) 
  a <- rbind(a,b) 
} 
tail(a) 

tail(a)
class(a)

.8*3000

a <- as.matrix(a)
datayo <- as.data.frame(a)
str(a)
doc <- Corpus(VectorSource(a))
summary(doc)
doc[[1]]$content

doc_1 <- tm_map(doc,tolower)
doc_1[[1]]$content
doc_2 <- tm_map(doc_1,removeNumbers)
doc_2[[1]]$content
doc_3 <- tm_map(doc_2, removeWords, stopwords("english"))
doc_3[[1]]$content
doc_4 <- tm_map(doc_3, removePunctuation)
doc_4[[1]]$content
doc_5 <- tm_map(doc_4, stripWhitespace)
doc_5[[1]]$content
doc_6 <- tm_map(doc_5, stemDocument, language = "english")
doc_6[[1]]$content

doc_matrix <- DocumentTermMatrix(doc_6)
doc_matrix
dim(doc_matrix)
doc_matrix <- removeSparseTerms(doc_matrix, 0.70)
dim(doc_matrix)
mean_doc <- sort(colMeans(as.matrix(doc_matrix)), decreasing = T)
mean_doc[1:20]
barplot(mean_doc[1:25], xlab = "Top 25 Words", ylab = "Frequency", ylim = c(0,1))
wordcloud(doc_6, scale = c(3,1), colors = brewer.pal(8,"Dark2"), max.words = 50 ,use.r.layout = F)

doc_matrix_trial <- DocumentTermMatrix(doc_6, list(dictionary = pos_lex_doc_vector))
names(doc_matrix_trial)
a <- as.matrix(doc_matrix_trial)
dim(a)
b <- as.data.frame(a)

sum_per_doc <- sort(rowSums(a))
sum_per_doc
datayo$V1 <- as.character(datayo$V1)
sentiment <- with(datayo, sentiment_by(V1))
with(sentiment, hist(ave_sentiment))
head(sentiment)

best_reviews <- slice(datayo, top_n(sentiment, 50, ave_sentiment)$element_id)
with(best_reviews, sentiment_by(V1)) %>% highlight()

worst_reviews <- slice(datayo, top_n(sentiment, 50, -ave_sentiment)$element_id)
with(worst_reviews, sentiment_by(V1)) %>% highlight()


 
positive_url <- getURL('http://ptrckprry.com/course/ssd/data/positive-words.txt',ssl.verifyhost=FALSE,ssl.verifypeer=FALSE)
positive_lexicon <- read.csv(textConnection(positive_url), header = T)
write.csv(positive_lexicon,"positivelex.csv")
pos_lex <- read.csv("positivelex.csv", stringsAsFactors = F)
class(pos_lex)
pos_lex <- as.matrix(pos_lex)
pos_lex_doc <- Corpus(VectorSource(pos_lex))

pos_lex_doc <- tm_map(pos_lex_doc, tolower)
pos_lex_doc[[1]]$content

pos_lex_doc <- tm_map(pos_lex_doc, removeNumbers)
pos_lex_doc[[1]]$content

pos_lex_doc <- tm_map(pos_lex_doc, removeWords, stopwords("english"))
pos_lex_doc[[1]]$content

pos_lex_doc <- tm_map(pos_lex_doc, removePunctuation)
pos_lex_doc[[1]]$content

pos_lex_doc <- tm_map(pos_lex_doc, stripWhitespace)
pos_lex_doc[[1]]$content

pos_lex_doc <- tm_map(pos_lex_doc, stemDocument, language = "english")
pos_lex_doc[[1:20]]$content

pos_lex_doc_df <- rep(NA,2005)
for(i in 1:2005){
pos_lex_doc_df[i] <- pos_lex_doc[[i]]$content
}

pos_lex_doc_df <- t(pos_lex_doc_df)
write.csv(pos_lex_doc_df,"bleh.csv")
pos_lex_doc_df <- read.csv("bleh.csv", stringsAsFactors = F)
str(pos_lex_doc_df)
pos_lex_doc_df[,1] <- as.character(pos_lex_doc_df[,1])
str(pos_lex_doc_df)
pos_lex_doc_vector <- as.vector(pos_lex_doc_df[,1])
pos_lex_doc_vector
wordcloud(pos_lex_doc, scale = c(2,1), colors = brewer.pal(8,"Dark2"),max.words = 50 ,use.r.layout = F)
