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
