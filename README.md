# TextMining-team6

## Topic: ESG 등급에 따른 기업 sustainability report 차이 분석
Team members: 
- Yunyoung choi
- 나희
- 서진
  
---

### Data
```
...
├── esg_grade_analysis
│   ├── esg_data_EDA.ipynb
│   └── esg_grade_data.csv
│
├── preprocessed_RData
│   ├── apple.RData
│   ├── dell.RData
│   └── microsoft.RData
│
└── sustainability_report
    ├── apple-2022.pdf
    ├── dell-2022.pdf
    └── microsoft-2022.pdf
```
  
     

1. `esg_grade_analysis/esg_grade_data.csv`   
   - 기업별 esg 등급과 점수  

2. `preprocessed_RData/`  
   - Apple, Dell, Microsoft 각 회사의 sustainability report를 전처리한 데이터

3. `sustainability_report/`  
   - apple-2022.pdf: [Apple 2022 sustainability report](https://www.apple.com/pl/environment/pdf/Apple_Environmental_Progress_Report_2022.pdf)  

   - dell-2022.pdf: [Dell 2022 sustainability report](https://www.dell.com/en-us/dt/corporate/social-impact/esg-resources/reports.htm#tab0=0&pdf-overlay=//www.delltechnologies.com/asset/en-us/solutions/business-solutions/briefs-summaries/delltechnologies-fy22-esg-report.pdf)  

   - microsoft-2022.pdf: [Microsoft 2022 sustainability report](https://news.microsoft.com/wp-content/uploads/prod/sites/42/2023/05/2022-Environmental-Sustainability-Report.pdf)   

  

### Code
#### Files
```
├ ...
├── 01_data_preprocessing.R
├── 02_ngram_analysis.R
├── 03_TF-IDF.R
├── 04_topic_modeling.R
├── 05_word_cloud.R
├── 06_text_similarity.R
```

#### Results
- n-gram analysis 
- TF-IDF
- Topic Modeling
- Word Cloud
- Text Similarity


#### Required Packages
```
library(pdftools)
library(dplyr)
library(stringr)
library(tm)
library(stopwords)
library(textstem)

```

---

#### Conclusion
