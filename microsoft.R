library(dplyr)
library(tm)
library(stringr)
library(pdftools)
library(tidytext)
library(wordcloud)
library(RWeka)
library(textstem)


# 텍스트 파일 읽기
MS <- pdf_text("2022-Environmental-Sustainability-Report.pdf")
head(MS)
# 1~2, 117부터 129페이지를 제외
MS <- MS[-c(1:3, 78:81)]
head(MS)


### 텍스트 전처리 

# 모든 페이지 시작 텍스트를 제거하는 패턴
pattern <- "Overview\\s+Microsoft sustainability\\s+Customer sustainability\\s+Global sustainability\\s+Appendix\\s+\\d+\\s+Carbon \\| Water \\| Waste \\| Ecosystems\\s*(Foreword \\(continued\\)\\s*)?"

# 정규식을 활용해 문구 제거
MS <- str_replace_all(MS, pattern, "")

MS %>% head()

MS <- MS %>% 
  str_remove_all("\n") %>% 
  tolower() %>% 
  str_remove_all("[\\+\\$]+") %>% # +와 $를 모두 제거
  str_remove_all('microsoft')%>% 
  str_remove_all("[[:punct:]]") %>% 
  str_remove_all('[[:digit:]]')%>% 
  stripWhitespace() %>% 
  lemmatize_strings()%>% 
  removeWords(stopwords('en'))

MS <- MS %>%str_remove_all('appendix')

MS %>% head()

MS <- MS %>% stripWhitespace()


MS_combined <- paste(MS, collapse = " ")

# 데이터 프레임 생성
Microsoft <- data.frame(company = "Microsoft", text = MS_combined, stringsAsFactors = FALSE)

Microsoft %>% head()

save(Microsoft, file = "Microsoft.RData")



### TF-IDF
# Document-Term Matrix 생성
dtm <- DocumentTermMatrix(MS)

# TF-IDF 가중치 적용
tfidf <- weightTfIdf(dtm)

# TF-IDF 행렬 확인
inspect(tfidf[1:5, 1:5])  # 첫 5개의 문서와 첫 5개의 단어만 확인

### wordcloud
library(wordcloud)
library(RColorBrewer)

# 워드 클라우드 생성
wordcloud(MS.cor, max.words = 200, 
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))


### topic modeling
#install.packages("topicmodels")
library(topicmodels)

# Document-Term Matrix로부터 LDA 모델 적용
num_topics <- 10  # 원하는 주제 수 지정
lda <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# 각 주제별 상위 단어 확인
topics <- terms(lda, 6)  # 각 주제당 상위 6개 단어 추출
print(topics)

# 1. 숫자, 구두점 제거 및 소문자로 변환
processed_MS <- MS %>%
  str_remove_all("[:digit:]") %>% 
  str_remove_all("[:punct:]") %>% 
  tolower() %>% 
  stripWhitespace()
  

# 2. 지정된 불필요한 구 제거
unwanted_phrases <- c("overview", "microsoft", "microsoft sustainability","customer sustainability", 
                      "global sustainability", "appendix")

processed_MS <- processed_MS %>%
  sapply(function(text) {
    for (phrase in unwanted_phrases) {
      text <- gsub(paste0("\\b", phrase, "\\b"), "", text) # 정확한 단어만 매칭
    }
    return(trimws(gsub("\\s+", " ", text))) # 앞뒤 공백 및 중복 공백 정리
  })

# 3. 결과를 character 벡터로 변환
processed_MS <- as.character(processed_MS)

# 결과 확인
head(processed_MS)

processed_MS %>% unnest_tokens()
