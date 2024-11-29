#install.packages("pdftools")
library(pdftools)
library(dplyr)
library(stringr)
library(tm)
library(stopwords)
library(textstem)

##########################################
### PDF pre-processing
##########################################

### 1.
### Apple 2022 sustainability report

apple_path <- "./sustainability_report/apple-2022.pdf"
apple <- pdf_text(apple_path)

# remove unnecessary pages
apple <- apple[3:81]

# set the custom stopwords appear in pdf texts.
# such as contents, or repetitive words
apple_stopwords <- c(
  stopwords::stopwords("en", source = "nltk"),          # NLTK stop words
  c("introduction", "climate", "change", "resources", 
    "smarter", "chemistry", "engagement", "appendix",
    "environmental", "progress", "report", "apple")   # Custom stop words
)


# Preprocessing function with extended stop words
clean_text_vector <- function(text_vector) {
  stopword_pattern <- paste0("\\b(", paste(apple_stopwords, collapse = "|"), ")\\b")
  text_vector %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Remove combined stop words
    str_remove_all(stopword_pattern) %>%
    # Remove punctuation
    str_replace_all("[[:punct:]]", "") %>%
    # Remove non-printable characters
    str_replace_all("[^\\x20-\\x7E]", " ") %>%
    # Remove numbers
    str_remove_all("[0-9]") %>%
    # Replace multiple spaces with a single space
    str_squish() %>%
    # Lemmatize strings
    lemmatize_strings()
}

# Apply preprocessing to the apple variable
cleaned_apple <- clean_text_vector(apple)

# View the first few cleaned entries
head(cleaned_apple)

apple_final <- paste(cleaned_apple, collapse = " ")
apple_df <- data.frame(company = "apple", text = apple_final, stringsAsFactors = FALSE)
save(apple_df, file = "apple.RData")



### 2.
### Microsoft 2022 sustainability report

microsoft_path <- "./sustainability_report/microsoft-2022.pdf"
ms <- pdf_text(microsoft_path)
ms <- ms[-c(1:3, 78:81)]
pattern_ms <- "overview sustainability customer sustainability global sustainability carbon | water | waste | ecosystems"


ms <- ms %>% 
  str_remove_all("\n") %>% 
  tolower() %>% 
  str_remove_all("[\\+\\$]+") %>% # +와 $를 모두 제거
  str_remove_all('microsoft')%>% 
  str_remove_all('appendix') %>%
  str_replace_all(pattern_ms, "") %>%
  str_remove_all("[[:punct:]]") %>% 
  str_remove_all('[[:digit:]]')%>% 
  stripWhitespace() %>% 
  lemmatize_strings()%>% 
  removeWords(stopwords('en'))



ms <- ms %>% str_remove_all('appendix')
ms <- ms %>% stripWhitespace()
# remove repetitive pattern of all pages


ms_final <- paste(ms, collapse = " ")
microsoft_df <- data.frame(company = "microsoft", text = ms_final, stringsAsFactors = FALSE)
save(microsoft_df, file = "microsoft.RData")

### 3.
### Dell 2022 sustainability report

dell_path <- "./sustainability_report/dell-2022.pdf"
dell <- pdf_text(dell_path)
dell <- dell[-c(1:3,132:138,148:159)]

dell <- dell %>%
  tolower(text) %>%
  str_replace_all(text, "\\s+", " ") %>%
  str_replace_all(text, "(https?://|www\\.|[a-zA-Z0-9-]+\\.[a-zA-Z]{2,})(/[a-zA-Z0-9-_/]*)?", "") %>%
  str_replace_all(text, "\\d+", " ")  %>%
  str_replace_all(text, "[^[:alnum:][:space:]]", "") %>%
  str_remove_all(text, paste0("\\b(", paste(stopwords("en"), collapse = "|"), ")\\b")) %>%
  str_squish() %>%
  lemmatize_strings()
  

pattern_dell <- "technologies intro plan goals dashboard advancing sustainability cultivating inclusion transforming lives ethics privacy supply chain numbers esg report"
str_replace_all(dell, pattern, "")

irr_word<-c("fy","dell","appendix","cy","will")
dell <- str_remove_all(
  dell, 
  paste0("\\b(", paste(irr_word, collapse = "|"), ")\\b")
)

dell_final <- paste(dell, collapse = " ")
dell_df <- data.frame(company = "dell", text = dell_final, stringsAsFactors = FALSE)
save(dell_df, file = "dell.RData")








##########################################
### TF-IDF
##########################################

library(tidytext)

# Create a data frame from cleaned_apple for text mining
text_df <- data.frame(doc_id = seq_along(cleaned_apple), text = cleaned_apple, stringsAsFactors = FALSE)


# Tokenize the text (split into words)
tidy_text <- text_df %>%
  unnest_tokens(word, text)

word_counts <- tidy_text %>%
  count(doc_id, word, sort = TRUE)

word_counts %>% head(20)

total_word_counts <- tidy_text %>%
  count(word, sort = TRUE)

# View the top 20 most frequent words
head(total_word_counts, 20)

library(ggplot2)

# Plot top 20 most frequent words
total_word_counts %>%
  top_n(20, n) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Words in the Corpus", x = "Word", y = "Frequency") +
  theme_minimal()


tfidf <- word_counts %>%
  bind_tf_idf(word, doc_id, n)

tfidf %>%
  arrange(desc(tf_idf)) %>%
  head(20)

## document 를 다시 설정해야 할 것 같음 

##########################################
### Word Cloud
##########################################
library(wordcloud)
# Create a frequency table of words
word_freq <- tidy_text %>%
  count(word, sort = TRUE)

# Generate the word cloud
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 5,
          scale = c(3,0.5), colors = brewer.pal(8, "Dark2"))



##########################################
### Topic Modeling(LDA)
##########################################

# Install and load the topicmodels package
#install.packages("topicmodels")
library(topicmodels)

# Create a document-term matrix (DTM)
dtm <- DocumentTermMatrix(Corpus(VectorSource(cleaned_apple)))

# Fit the LDA model (choose the number of topics, e.g., 5 topics)
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Show top terms for each topic
terms(lda_model, 10)  # Show top 10 terms for each topic









