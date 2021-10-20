#네이버는 400P까지 적용 4000개가 최대기사. 중복되는 항목은 어쩔수없음.  다른사이트 or 최신순? 검색어 등 바꿔야 한다.
library(tidytext); library(N2H4); library(tm)
library(multilinguer); library(KoNLP)
library(rvest); library(dplyr) ; library(XML)
#crawling
basic_url <- 'https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EB%85%B8%EC%9D%B8&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=50&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start='
#start 1, 11, 21 ~ // 10단위로 크롤링
urls1 <- NULL
for(x in 0:100){
  urls1[x] <- paste0(basic_url, x*10+3)
}

#뉴스기사제목
links= NULL
for(url1 in urls1){
  html <- read_html(url1)
  links <- c(links, html %>% html_nodes('.news_area') %>% html_nodes('a') %>% html_attr('title') %>% unique())
}

links <- links[is.na(links)==FALSE]


txts <- NULL
urls2 <- NULL
for(x in 0:100){
  urls2[x] <- paste0(basic_url, x*10-9)
}
for(url2 in urls2){
  html <- read_html(url2)
  txts <- c(txts, html %>% html_nodes('.news_dsc') %>% html_nodes('a') %>% html_text())
}

txts_1 <- data.frame(txts)
links_1 <- data.frame(links)
word <- cbind(links_1, txts_1)


# 1001~2000
urls2 <- NULL
for(x in 101:200){
  urls2[x] <- paste0(basic_url, x*10+3)
}
urls2 <- urls2[101:200]

#뉴스기사제목
links2= NULL
for(url2 in urls2){
  html2 <- read_html(url2)
  links2 <- c(links2, html2 %>% html_nodes('.news_area') %>% html_nodes('a') %>% html_attr('title') %>% unique())
}

links2 <- links2[is.na(links2)==FALSE]


# 2001~3000
urls3 <- NULL
for(x in 201:300){
  urls3[x] <- paste0(basic_url, x*10+3)
}
urls3 <- urls3[201:300]

#뉴스기사제목
links3= NULL
for(url3 in urls3){
  html3 <- read_html(url3)
  links3 <- c(links3, html3 %>% html_nodes('.news_area') %>% html_nodes('a') %>% html_attr('title') %>% unique())
}

links3 <- links3[is.na(links3)==FALSE]



# 3001~4000
urls4 <- NULL
for(x in 301:400){
  urls4[x] <- paste0(basic_url, x*10+3)
}
urls4 <- urls4[301:400]

#뉴스기사제목
links4= NULL
for(url4 in urls4){
  html4 <- read_html(url4)
  links4 <- c(links4, html4 %>% html_nodes('.news_area') %>% html_nodes('a') %>% html_attr('title') %>% unique())
}

links4 <- links4[is.na(links4)==FALSE]


# 4001~5000
urls5 <- NULL
for(x in 401:500){
  urls5[x] <- paste0(basic_url, x*10+3)
}
urls5 <- urls5[401:500]

#뉴스기사제목
links5= NULL
for(url5 in urls5){
  html5 <- read_html(url5)
  links5 <- c(links5, html5 %>% html_nodes('.news_area') %>% html_nodes('a') %>% html_attr('title') %>% unique())
}

links5 <- links5[is.na(links5)==FALSE]


sum(duplicated(links5) == F)


links <- c(links,links2,links3,links4,links5)
links <- links[duplicated(links) == F]
write.csv(links, 'C:/Users/user/Desktop/노인놀이터플잭/크롤링3502.csv')



##### 워드클라우드

library(tidytext); library(N2H4); library(tm)
library(multilinguer); library(KoNLP)
library(rvest); library(dplyr) ; library(XML)


# 불러오기
links <- read.csv('C:/Users/user/Desktop/noinproject/크롤링3502.csv') %>% select(x)







#install_jdk()
useSejongDic()

# 사용자사전
mergeUserDic(data.frame(c('노인정', "노인", "간병","지원센터", '고위험군','전통시장', '노인복지관', '노인복지시설', '일자리', '대피', '부산',
                          '진구', '장애인', '마스크', '송파', '오열', '어눌', '전북', '연합회', '청주', '소방서장', '소방서',
                          '아동', '델타변이'), c("ncn")))


data <- sapply(links, extractNoun, USE.NAMES = F) # 명사추출, 형태소로 분리해도 결과는 같음. 이대로 적용
data_unlist <- unlist(data)
data_unlist <- Filter(function(x){nchar(x)>=2}, data_unlist) #2자 이상만 추출.





data_unlist = gsub("\\(.*?\\)", "", data_unlist) # () 분리
data_unlist = gsub("\\[.*?\\]", "", data_unlist) # [] 분리
data_unlist = gsub("<.*?>", "", data_unlist) # <> 분리
punct = gsub(pattern = "[ㄱ-힣A-Za-z0-9 ]", "", data_unlist)
punct = paste0(unique(punct), collapse = "")
punct = unique(unlist(strsplit(punct, split = "")))
data_unlist = gsub(pattern = "[^ㄱ-힣A-Za-z0-9 ]", "", data_unlist)
words = unlist(strsplit(data_unlist, split = " "))
#대체단어
library(stringr) 
words <- str_replace_all(words, c('인천시' = '인천', '전남도' = '전남', '성남시' = '성남', '동두천시' = '동두천',
                                  '복지시설'='복지', '복지관'='복지', '비대' = '비대면'))


# 불용어
stop_words <- c('노인', '추석', 'bhc', '대한', '2021', '10', '80', '명절', '사랑', '업무협약', '60', '90', '70', '20',
                '0', '시지', '추가', '전담', '포토', '3', '5', '대', '담배셔틀', '업무협', '전달', '간담', '협약', '맞이',
                '지회', '사업', '이낙연', '한가', '셔틀', '담배', '향상', '복지', '일자리', '독거')
write.csv(stop_words,file="C:/Users/user/Desktop/노인놀이터플잭/stopword.csv")
# 불용어 제거
txt <- read.csv("C:/Users/user/Desktop/noinproject/stopword.csv") %>% select(x) 
txt <- txt$x
cnt_txt <- length(txt)

for( i in 1:cnt_txt) {
  words <-gsub((txt[i]),"",words)      
} 

words <- Filter(function(x){nchar(x)>=2}, words)#다시 2자이상 추출. // 공백문자 제외
df_words = as.data.frame(table(words))
df_words = df_words[order(df_words$Freq, decreasing = TRUE), ]
head(df_words)


###############WC---------------
#색깔
#devtools::install_github("jkaupp/nord")
library(nord)
pal = nord(palette = "afternoon_prarie", 200, reverse = T)
#폰트
#install.packages('extrafont')
library(extrafont)
#font_import()
loadfonts(device="win") 
# 워드클라우드
library(wordcloud2)
wordcloud2(df_words[1:50, ], size=0.8,
           fontFamily = "경기천년바탕", 
           color = pal,
           shape = "triangle")#circle, cardioid, diamond, triangle-forward, triangle, pentagon, star

# html 형식으로 저장
library(htmlwidgets) 
saveWidget(WC, "C:/Users/user/Desktop/noinproject/giriboy.html",  selfcontained = F)

# html 형식을 png로 변환
#install.packages('webshot')
#webshot::install_phantomjs() 
library(webshot)
webshot("C:/Users/user/Desktop/noinproject/giriboy.html", "C:/Users/user/Desktop/noinproject/giriboy.jpeg", 
        delay = 5, # 스크린샷을 찍을 때 기다리는 시간, 
        # 워드클라우드 나올 때 걸리는 시간이 있어서 안해주면 내용 다 저장 안됨
        vwidth = 1000, vheight = 1000)





### 연관분석

links <- read.csv('C:/Users/user/Desktop/noinproject/크롤링3502.csv') %>% select(x)
head(links, 1)
unlist(links) %>% head()

#install_jdk()
useSejongDic()
useNIADic()
# 사용자사전
mergeUserDic(data.frame(c('노인정', "노인", "간병","지원센터", '고위험군','전통시장', '노인복지관', '노인복지시설', '일자리', '대피', '부산',
                          '장애인', '마스크', '송파', '오열', '어눌', '전북', '연합회', '청주', '소방서장', '소방서',
                          '아동', '델타변이'), c("ncn")))

library('reshape2')
m_df <- SimplePos09(unlist(links)) %>% melt %>% as_tibble %>% mutate(L1 = substr(L1, 2, nchar(L1))) %>% select(3, 1)

m_count <- m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>%
  filter(str_length(noun)>=2) %>%
  count(noun, sort=TRUE) %>%
  head(100) # 단어별 count 많은순으로 100개 지정. 많아질수록 오래걸리고 알아보기 힘듦.

# 단어 대체
m_count$noun <- str_replace_all(m_count$noun, c('인천시' = '인천', '전남도' = '전남', '성남시' = '성남', '동두천시' = '동두천',
                                  '복지시설'='복지', '복지관'='복지', '비대' = '비대면'))
m_count <- m_count %>% group_by(noun) %>% summarise(n=sum(n)) 



# 불용어
stop_words <- c('노인', '추석', 'bhc', '대한', '2021', '10', '80', '명절', '사랑', '업무협약', '60', '90', '70', '20',
                '0', '시지', '추가', '전담', '포토', '3', '5', '대', '담배셔틀', '업무협', '전달', '간담', '협약', '맞이',
                '지회', '사업', '이낙연', '한가', '셔틀', '담배', '향상', '복지', '일자리', '독거', '어르신', '윤석열',
                # 인천 이외 지역 제외
                '충북', '충남', '경기', '울산', '전남', '남구')
write.csv(stop_words,file="C:/Users/user/Desktop/noinproject/stopword.csv")
# 불용어 제거
txt <- read.csv("C:/Users/user/Desktop/noinproject/stopword.csv") %>% select(x) 
txt <- txt$x
cnt_txt <- length(txt)

for( i in 1:cnt_txt) {
  m_count$noun <-gsub((txt[i]),"",m_count$noun)      
} 
m_count <- m_count %>% filter(noun != '', str_length(noun)>=2) %>% arrange(desc(n)) %>% head(25)


library(stringr)
m_df2 <- m_df %>%
  mutate(noun=str_match(value, '([가-힣]+)/N')[,2]) %>%
  na.omit %>%
  filter(str_length(noun)>=2) %>%
  select(3, 1)

m_df3 <- m_df2 %>%
  filter(noun %in% m_count$noun)

#가구의 경우 1인가구, 독거노인가구, 8가구지원 등 여러가지가 있음.



#의미연결망
library('igraph')
mg <- graph_from_data_frame(m_df3)
V(mg)$type <- bipartite_mapping(mg)$type
mm <- as_incidence_matrix(mg) %*% t(as_incidence_matrix(mg))
diag(mm) <- 0
mg <- graph_from_adjacency_matrix(mm)
plot(mg)

#install.packages('tidygraph')
#install.packages('ggraph')
library('tidygraph')
library('ggraph')
mg %>% as_tbl_graph() %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))









########bigram 결과 다시 만들기
library(tidyr)
# 단어대체
m_df2$noun <- str_replace_all(m_df2$noun, c('인천시' = '인천', '전남도' = '전남', '성남시' = '성남', '동두천시' = '동두천',
                                            '복지시설'='복지', '복지관'='복지', '비대' = '비대면'))


# 불용어제거
m_df2$noun <- str_replace_all(m_df2$noun, c('노인'='', '담배' = '', '셔틀' = '', '담배셔틀' = '', '추석'='',
                                            'bhc'='', '대한'='', '명절'='', '신한은행' = '', 
                                            # 다른 도시 제외
                                            '세종시' = '', '전남' = '', '영등포케어센터' = '', '남구'='',
                                            '광주' = '', '담양군' = '', '신안군' = ''))



m_df2 <- m_df2 %>% filter(noun != '', str_length(noun)>=2)

bigram_df <- m_df2 %>%
  na.omit() %>%
  select(noun) %>%
  mutate(lead=lead(noun)) %>%
  unite(bigram, c(noun, lead), sep=" ") %>%
  count(bigram, sort=TRUE) %>%
  head(50) %>%
  separate(bigram, c('word1', 'word2'), sep=' ')

bigram_df_incheon <- bigram_df %>% filter(word1 =='인천'|
                                            word2 =='인천') %>% head(20)

bigram_df %>%
  as_tbl_graph %>%
  ggraph() +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name))) +
  geom_node_text(aes(label=name))

bigram_df %>%
  as_tbl_graph(directed=FALSE) %>%
  activate(nodes) %>%
  mutate(eigen = centrality_eigen(),
         group = group_infomap()) %>%
  ggraph(layout='nicely') +
  geom_edge_link(color='gray50', alpha=.2) +
  geom_node_point(aes(color=factor(group), size=eigen)) +
  geom_node_text(aes(label=name), size=5, repel=TRUE) +
  theme_graph() +
  theme(legend.position='none')
