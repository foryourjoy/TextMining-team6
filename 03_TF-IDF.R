library(dplyr)
library(stringr)
library(magrittr)
library(tm)
library(tidytext)

# load data
load(file = "preprocessed_RData/microsoft.Rdata")
load(file = "preprocessed_RData/apple.Rdata")
load(file = "preprocessed_RData/dell.Rdata")


# check the data
microsoft_df %>% head()
apple_df %>% head()
dell_df %>% head()

# check the column names
colnames(microsoft_df)
colnames(apple_df)
colnames(dell_df)
## result :  "company" "text" 


# combine the data
data <- rbind(microsoft_df, apple_df, dell_df)

# tokenize the data
tokenized_data <- data %>%
  unnest_tokens(word, text)

tokenized_data %>% head()
tokenized_data %>% tail()

# load stopword data
data("stop_words")

# delete stopwords
filtered_data <- tokenized_data %>%
  anti_join(stop_words, by = "word")

filtered_data %>% head()
filtered_data %>% tail()

# calculate the frequency of data
word_counts <- filtered_data %>%
  count(company, word, sort = TRUE)


###############################################
################# TF-IDF ######################
###############################################

# Make Corpus 
corpus <- VCorpus(VectorSource(data$text))

# set the documents name
names(corpus) <- data$company

# Make Document-Term Matrix 
dtm <- DocumentTermMatrix(corpus)

# check DTM 
inspect(dtm)

## Similarity
doc.cos <-
  dtm %>%
  as.matrix %>%
  proxy::dist(method = "cosine") %>%
  as.matrix

doc.cos

# TF-IDF
dtm_tfidf <- weightTfIdf(dtm)

# check TF-IDF 
inspect(dtm_tfidf)

# weightTfIdf
dtm_tfidf.n <- weightTfIdf(dtm, normalize = FALSE)
dtm_tfidf.n %>% inspect()

dtm_tfidf.mat <- dtm_tfidf.n %>% 
  as.matrix()
  
dtm_tfidf.mat.df <- 0
for(i in 1:nrow(dtm_tfidf.mat)){
  temp <- dtm_tfidf.mat[i,]
  temp <- data.frame(
    doc=rownames(dtm_tfidf.mat)[i],
    word=names(temp),
    tfidf=temp,
    row.names = NULL)
  dtm_tfidf.mat.df <-
    rbind(dtm_tfidf.mat.df, temp)
  rm(temp)
}

dtm_tfidf.mat.df <-
  dtm_tfidf.mat.df[2:nrow(dtm_tfidf.mat.df),]

# dtm_tfidf.mat.df for each company
dtm_tfidf.mat.df %>% filter(doc == "microsoft") %>% 
  arrange(desc(tfidf)) %>%
  head(20)

dtm_tfidf.mat.df %>% filter(doc == "apple") %>% 
  arrange(desc(tfidf)) %>%
  head(20)

dtm_tfidf.mat.df %>% filter(doc == "dell") %>% 
  arrange(desc(tfidf)) %>%
  head(20)


############################
###### Topic modeling ###### 
############################

library(ldatuning)

dtm %>% inspect()

# Find the best number of topics
dtm %>% 
  FindTopicsNumber(topics = seq(from=2, to=30, by=6),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed=1009)) %>% 
  FindTopicsNumber_plot()


dtm %>% 
  FindTopicsNumber(topics = seq(from=2, to=14, by=3),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed=1009)) %>% 
  FindTopicsNumber_plot()



dtm %>% 
  FindTopicsNumber(topics = seq(from=2, to=8, by=1),
                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010","Deveaud2014"),
                   method = "Gibbs",
                   control = list(seed=1009)) %>% 
  FindTopicsNumber_plot()


library(topicmodels)

dtm.lda.6 <-
  dtm %>% 
  LDA(control = list(seed = 1009), k=6)

dtm.lda.6 %>% terms(10)

###### CTM

dtm.ctm.6 <-
  dtm %>% 
  CTM(control = list(seed = 1009), k=6)


dtm.ctm.6 %>% terms(10)


