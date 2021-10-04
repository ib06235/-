# 단어연관분석

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


# 중점적으로 봐야하는 것? 
# 인천이 많았다 -> 인천은 무엇과 연관성이 있을까? -> 다른 지역에 비해 무엇이 다른가? -> 어쨋든 방향에 맞춰서 끌어가는 것이 필요
# 인천만 볼까?
