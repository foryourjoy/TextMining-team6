library(dplyr)
library(stringr)
library(magrittr)
library(tm)
library(tidytext)

# load data
load(file = "Microsoft.Rdata")
load(file = "apple.Rdata")
load(file = "dell.Rdata")


# check the data
Microsoft %>% head()
apple_df %>% head()
dell %>% head()

# check the column names
colnames(Microsoft)
colnames(apple_df)
colnames(dell)
## "company" "text" 


# combine the data
data <- rbind(Microsoft, apple_df, dell)

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

# TF-IDF
dtm_tfidf <- weightTfIdf(dtm)

# check TF-IDF 
inspect(dtm_tfidf)


# removeSparseTerms
dtm_tfidf %>% 
  removeSparseTerms(0.5) %>% 
  inspect()

dtm_tfidf %>% 
  removeSparseTerms(0.6) %>% 
  inspect()

dtm_tfidf %>% 
  removeSparseTerms(0.9) %>% 
  inspect()

########## tdm

tdm <- TermDocumentMatrix(corpus)

tdm %>% inspect()

tdm_tfidf <- weightTfIdf(tdm)

tdm_tfidf %>% inspect()


# removeSparseTerms
tdm_tfidf %>% 
  removeSparseTerms(0.4) %>% 
  inspect()

tdm_tfidf %>% 
  removeSparseTerms(0.5) %>% 
  inspect()

tdm_tfidf %>% 
  removeSparseTerms(0.6) %>% 
  inspect()

tdm_tfidf %>% 
  removeSparseTerms(0.9) %>% 
  inspect()


# check esgterm dataframe

load("esgterm.RData")

esg_terms_df %>% head()
esg_terms_df %>% tail()


## compare esg term between the company
merged_data <- inner_join(filtered_data, esg_terms_df, by = "word")


# inner_join (allow many-to-many relationships)
result <- inner_join(filtered_data, esg_terms_df, by = "word", 
                     relationship = "many-to-many")

result %>% group_by(company, type) %>% 
  count(word) %>% arrange(desc(n))

microsoft <- result %>% filter(company == 'Microsoft')
microsoft_pie <- microsoft %>% group_by(type) %>% count(.)

# Draw Pie chart to check E,S,G ratio for each companies
library(ggplot2)

##### microsoft
# Calculate ratios after ungrouping
microsoft_pie <- microsoft_pie %>%
  ungroup() %>%  
  mutate(percentage = n / sum(n) * 100,  # calculate the ratio
  ) 

# Visualize pie charts
ggplot(microsoft_pie, aes(x = "", y = n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  # add labels
            position = position_stack(vjust = 0.5)) +  
  labs(title = "Microsoft Pie Chart", fill = "Type") +
  theme(plot.title = element_text(hjust = 0.5))


##### apple
apple <- result %>% filter(company == 'apple')
apple_pie <- apple %>% group_by(type) %>% count(.)


apple_pie <- apple_pie %>%
  ungroup() %>%  
  mutate(percentage = n / sum(n) * 100, 
  ) 

ggplot(apple_pie, aes(x = "", y = n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  
            position = position_stack(vjust = 0.5)) +  
  labs(title = "Apple Pie Chart", fill = "Type") +
  theme(plot.title = element_text(hjust = 0.5))


######## dell
dell <- result %>% filter(company == 'Dell')
dell_pie <- dell %>% group_by(type) %>% count(.)

dell_pie <- dell_pie %>%
  ungroup() %>% 
  mutate(percentage = n / sum(n) * 100, 
  ) 

ggplot(dell_pie, aes(x = "", y = n, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +  
  labs(title = "Dell Pie Chart", fill = "Type") +
  theme(plot.title = element_text(hjust = 0.5))

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

dtm.lda.5 <-
  dtm %>% 
  LDA(control = list(seed = 1009), k=5)

dtm.lda.5 %>% terms(10)

dtm.lda.6 <-
  dtm %>% 
  LDA(control = list(seed = 1009), k=6)

dtm.lda.6 %>% terms(10)

###### CTM
dtm.ctm.5 <-
  dtm %>% 
  CTM(control = list(seed = 1009), k=5)

dtm.ctm.6 <-
  dtm %>% 
  CTM(control = list(seed = 1009), k=6)

dtm.ctm.5 %>% terms(10)
dtm.ctm.6 %>% terms(10)


