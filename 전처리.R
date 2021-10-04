#경로당
g <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/인천경로당.csv')
gu_kyung <- g %>% group_by(구명) %>% summarise(gu_kyung = n())
dong_kyung <- g %>% group_by(동명) %>% summarise(dong_kyung = n())
dong_kyung$동명 <- gsub(" ", "", dong_kyung$동명)
dong_kyung <- dong_kyung %>% group_by(동명) %>% summarise(dong_kyung = sum(dong_kyung))


#대학
u <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/인천광역시_노인대학현황_20200312_cng (1).csv')
gu_univ <- u %>% group_by(군구명) %>% summarise(gu_univ = n()) %>% rename(구명=군구명)
dong_univ <- u %>% group_by(동명) %>% summarise(dong_univ = n())
dong_univ$동명 <- gsub(" ", "", dong_univ$동명)


#요양
y <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/합본/Big_project_data/대분류_요양시설_중복제거.csv')
gu_yoyang <- y %>% mutate(구명 = substr(주소, 1, 3)) %>% group_by(구명) %>% summarise(gu_yoyang = n())
gu_yoyang$구명 <- gsub(" ", "", gu_yoyang$구명)
y2 <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/dong_yoyang.csv')
dong_yoyang <- y2 %>% select(EMD_NM, NUMPOINTS) %>% rename(동명 = EMD_NM, dong_yoyang = NUMPOINTS)
dong_yoyang$동명 <- gsub(" ", "", dong_yoyang$동명)
dong_yoyang <- dong_yoyang %>% group_by(동명) %>% summarise(dong_yoyang = sum(dong_yoyang))
#복지
b <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/합본/Big_project_data/대분류_복지시설_중복제거.csv')
gu_bokji <- b %>% mutate(구명 = substr(주소, 1, 3)) %>% group_by(구명) %>% summarise(gu_bokji = n())
gu_bokji$구명 <- gsub(" ", "", gu_bokji$구명)
b2 <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/dong_bokji.csv')
dong_bokji <- b2 %>% select(EMD_NM, NUMPOINTS) %>% rename(동명 = EMD_NM, dong_bokji = NUMPOINTS)
dong_bokji$동명 <- gsub(" ", "", dong_bokji$동명)
dong_bokji <- dong_bokji %>% group_by(동명) %>% summarise(dong_bokji = sum(dong_bokji))
#버스
bus <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/gu_bus.csv')
gu_bus <- bus %>% select(SIG_KOR_NM, NUMPOINTS) %>% rename(구명 = SIG_KOR_NM, gu_bus = NUMPOINTS)
bus2 <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/dong_bus.csv')
dong_bus <- bus2 %>% select(EMD_NM, NUMPOINTS) %>% rename(동명 = EMD_NM, dong_bus = NUMPOINTS)
dong_bus$동명 <- gsub(" ", "", dong_bus$동명)
dong_bus <- dong_bus %>% group_by(동명) %>% summarise(dong_bus = sum(dong_bus))


#지하철
sub <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/Gu_Subway_cnt.csv')
gu_sub <- sub %>% select(SIG_KOR_NM, NUMPOINTS) %>% rename(구명 = SIG_KOR_NM, gu_sub = NUMPOINTS)
sub2 <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/인천시_동별_지하철역_개수.csv')
dong_sub <- sub2 %>% select(EMD_NM, SUBWAY_STATIONS_CNT) %>% rename(동명 = EMD_NM, dong_sub = SUBWAY_STATIONS_CNT)
dong_sub$동명 <- gsub(" ", "", dong_sub$동명)
dong_sub <- dong_sub %>% group_by(동명) %>% summarise(dong_sub = sum(dong_sub))

#구 별로 종합
gu <- merge(gu_kyung, gu_univ, by='구명', all=T)
gu <- merge(gu, gu_yoyang, by='구명', all=T)
gu <- merge(gu, gu_bokji, by='구명', all=T)
gu <- merge(gu, gu_bus, by='구명', all=T)
gu <- merge(gu, gu_sub, by='구명', all=T)
gu[is.na(gu)] <- 0


#동 별로 종합
dong <- merge(dong_kyung, dong_univ, by='동명', all=T)
dong <- merge(dong, dong_yoyang, by='동명', all=T)
dong <- merge(dong, dong_bokji, by='동명', all=T)
dong <- merge(dong, dong_bus, by='동명', all=T)
dong <- merge(dong, dong_sub, by='동명', all=T)
dong[is.na(dong)] <- 0




# dong <- merge(dong_kyung, dong_univ, by='동명', all=T)
# dong <- merge(dong, dong_yoyang, by='동명', all=T)
# dong <- merge(dong, dong_bokji, by='동명', all=T)
# dong1 <- read.csv('C:/Users/user/Desktop/노인놀이터플잭/점위치/동_종합.csv')
# dong2 <- dong %>% group_by(동명) %>% summarise(복지시설 = sum(dong_kyung, dong_univ, dong_yoyang, dong_bokji))
# dong3 <- merge(dong1, dong2)


#동명이 최신과 맞지 않아 엑셀에서 동명을 맞춤
dong2 <- dong %>% group_by(동명) %>% summarise(교통 = sum(dong_bus, dong_sub))
dong <- merge(dong_bus, dong_sub, by='동명', all=T)
# 종합파일 저장
#write.csv(gu, 'C:/Users/user/Desktop/노인놀이터플잭/점위치/구_종합.csv')
#write.csv(dong2, 'C:/Users/user/Desktop/노인놀이터플잭/점위치/동_종합.csv')
#요인묶기
gu2 <- gu %>% group_by(구명) %>% summarise(복지시설 = sum(gu_kyung, gu_univ, gu_yoyang, gu_bokji),
                                      교통 = sum(gu_bus, gu_sub))
dong2 <- dong %>% group_by(동명) %>% summarise(복지시설 = sum(dong_kyung, dong_univ, dong_yoyang, dong_bokji),
                                               교통 = sum(dong_bus, dong_sub))