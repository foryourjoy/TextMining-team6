library(pdftools)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)


# PDF 파일 경로
pdf_path <- "./sustainability_report/dell-2022.pdf"

# PDF 텍스트 추출
dell <- pdf_text(pdf_path)

# 데이터 합치기 (전체 텍스트로 합침)
full_text <- paste(dell, collapse = " ")

clean_text_list <- function(texts) {
  processed_list <- lapply(texts, function(text) {
    text <- tolower(text)                         # 소문자로 변환
    text <- str_replace_all(text, "\\s+", " ")    # 공백 정리
    text <- str_replace_all(text, "(https?://|www\\.|[a-zA-Z0-9-]+\\.[a-zA-Z]{2,})(/[a-zA-Z0-9-_/]*)?", "") # URL 제거
    text <- str_replace_all(text, "\\d+", " ")                         # 숫자 제거
    text <- str_replace_all(text, "[^[:alnum:][:space:]]", "")          # 특수문자 제거
    text <- str_remove_all(text, paste0("\\b(", paste(stopwords("en"), collapse = "|"), ")\\b")) # stopword 제거
    text <- str_trim(str_replace_all(text, "\\s+", " "))    # 공백 정리
    text_split <- unlist(str_split(text, "(?<=\\.)\\s+"))   # 문장 단위로 분리
    return(text_split)                                      # 문장 리스트 반환
  })
  return(processed_list)                                    # 전체 텍스트를 리스트로 반환
}

cleaned_list <- clean_text_list(dell)
print(cleaned_list)

#필요없는 페이지 제거(1~3, appendix 제거)
cleaned_list<-cleaned_list[-c(1:3,132:138,148:159)]

cleaned_list[115]
### 데이터 분석에 필요 없는 fy, dell 단어들은 빼주도록 한다 ###

irr_word<-c("fy","dell","appendix","cy","will")
cleaned_list <- str_remove_all(
  cleaned_list, 
  paste0("\\b(", paste(irr_word, collapse = "|"), ")\\b")
)

#체크
cleaned_list[115]

###맨 밑 줄 없애기###

remove_phrase <- "technologies intro plan goals dashboard advancing sustainability cultivating inclusion transforming lives ethics privacy supply chain numbers esg report"
cleaned_list <- lapply(cleaned_list, function(x) {
  # 불필요한 문구 제거
  x <- str_remove(x, paste0(".*", remove_phrase, "$"))
  # 불필요한 단어들 제거
  words <- str_split(x, "\\s+")[[1]]  # 단어 단위로 분리
  words <- words[!(words %in% c("technologies","intro", "plan", "goals", "dashboard", "advancing", "sustainability", 
                                "cultivating", "inclusion", "transforming", "lives", "ethics", "privacy", 
                                "supply", "chain", "numbers", "esg", "report"))]  # 불필요한 단어들 제거
  paste(words, collapse = " ")  # 문장으로 다시 합침
})

# 결과 확인
cleaned_list

###Lemmatization###

library(textstem)


# cleaned_list를 character 벡터로 변환
cleaned_list <- as.character(cleaned_list)

# cleaned_list를 데이터프레임으로 변환
df <- data.frame(text = cleaned_list, stringsAsFactors = FALSE)

# 텍스트를 단어 단위로 분리하고, lemmatization 적용
word_frequencies <- df %>%
  unnest_tokens(word, text) %>%   # 'text' 열에서 단어 단위로 토큰화
  mutate(word = str_to_lower(word)) %>%  # 소문자로 변환
  mutate(word = str_trim(word)) %>%  # 공백 제거
  mutate(word = lemmatize_words(word)) %>%  # lemmatization 적용
  
  count(word, sort = TRUE)       # 단어 빈도 계산 및 정렬

# 결과 출력
head(word_frequencies, 20)

