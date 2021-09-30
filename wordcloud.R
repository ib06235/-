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



