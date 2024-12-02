library(dplyr)
library(stringr)
library(magrittr)
library(tm)
library(tidytext)

# load data
load(file = "Microsoft.Rdata")
load(file = "apple.Rdata")
load(file = "dell.Rdata")

dell<- dell.Rdata

######네트워크#######

# Bigram 생성
bigrams <- dell %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Bigram 분리 및 개수 세기
bigram_counts <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

######## 그래프 그리기 ######

# 빈도수 기준으로 bigram 필터링
bigram_filtered <- bigram_counts %>% 
  filter(n > 10)  # 임계값(예: 10)을 설정하여 빈도수가 높은 bigram만 필터링

# 필터링된 데이터를 바탕으로 그래프 재구성
bigram_graph <- graph_from_data_frame(bigram_filtered, directed = FALSE)

# igraph를 사용하여 그래프 시각화
set.seed(1234) 
plot(bigram_graph, 
     vertex.label = V(bigram_graph)$name,  # 각 노드의 이름을 라벨로 사용
     vertex.size = 5,                      # 노드 크기 설정
     edge.width = E(bigram_graph)$n / 2,   # 엣지의 두께를 빈도수에 따라 조정
     layout = layout_with_fr,              # Fruchterman-Reingold 레이아웃
     main = "Bigram Network with igraph")   # 제목 설정

# ggraph를 사용하여 그래프 시각화
set.seed(1234) 
ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(width = n), alpha = 0.8, color = "gray") +  # 엣지 두께를 빈도수에 따라 설정
  geom_node_point(size = 5, color = "blue") +                   # 노드 크기와 색상 설정
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +   # 노드 라벨을 밀어서 표시
  theme_void() +                                               # 배경을 제거하여 깔끔하게
  ggtitle("Bigram Network with ggraph") + 
  theme(plot.title = element_text(hjust = 0.5))  # 제목 중앙 정렬

# 다른 레이아웃으로 시도
set.seed(1004) 
ggraph(bigram_graph, layout = "kk") +  # Kamada-Kawai 레이아웃 사용
  geom_edge_link(aes(width = n), alpha = 0.6, color = "gray") +  # 엣지 두께 설정
  geom_node_point(size = 4, color = "blue") +                   # 노드 크기 설정
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +   # 노드 라벨 밀어서 표시
  theme_void() +                                               # 배경 제거
  ggtitle("Filtered Bigram Network with ggraph")  # 제목 설정

