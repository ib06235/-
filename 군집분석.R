# install.packages(c('tidyverse', 'plotly', 'dplyr', 'fpc', 'NbClust', 'mclust'))
library(mclust);library(NbClust);library(fpc);library(plotly);library(tidyverse)
c <- read.csv('C:/Users/user/Desktop/noinproject/점위치/읍면동_군집변수2.csv')

#동명, 군집분석에 사용할 세 가지 요인 추출
total2 <- c %>% select(동명, 노인수, 복지시설, 교통)


df = total2[,2:4] # 요인만 추출/요인 = 노인수, 복지시설, 교통
total_scale <- scale(total2[-1])#정규화

#kmeans----
nc <- NbClust(total_scale, min.nc = 2, max.nc = 15, method = "kmeans") # 적절한 군집 수 = 3 or 4

df$kmeans_cluster3 = factor(kmeans(total_scale,3)$cluster)#k=3 군집 
df$kmeans_cluster4 = factor(kmeans(total_scale,4)$cluster)#k=4 군집
cluster <- data.frame(c$동명, df)
cluster <- cluster %>% rename(동명 =c.동명)

#3d시각화
#k=3 시각화
p1.1 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
             z=~교통, color=~kmeans_cluster3,
             hoverinfo="text",
             text = paste("노인수 :", cluster$노인수, "<br>",
                          "복지시설 :", cluster$복지시설, "<br>",
                          "교통 :", cluster$교통, "<br>",
                          "동명 :", cluster$동명
             )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()
#k=4 시각화
p1.2 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
             z=~교통, color=~kmeans_cluster4,
             hoverinfo="text",
             text = paste("노인수 :", cluster$노인수, "<br>",
                          "복지시설 :", cluster$복지시설, "<br>",
                          "교통 :", cluster$교통, "<br>",
                          "동명 :", cluster$동명
             )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()

# k=3인 경우 너무 포괄적인 결과가 나타남. k=4가 적당함.
cluster <- cluster %>% select(-kmeans_cluster3) %>% rename(kmeans_cluster = kmeans_cluster4)
p1 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
            z=~교통, color=~kmeans_cluster,
            hoverinfo="text",
            text = paste("노인수 :", cluster$노인수, "<br>",
                         "복지시설 :", cluster$복지시설, "<br>",
                         "교통 :", cluster$교통, "<br>",
                         "동명 :", cluster$동명
            )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()

#k-Medoids ----
# pamk함수에 데이터를 넣어서 분류하고 결과를 pamk.result 변수에 저장하기
df = total2[,2:4] #df에 다시 요인만 저장
pamk.result <- pamk(df) # k-medoids clustering 적용
# number of clusters
pamk.result$nc

Kmedoids_cluster <- data.frame(Kmedoids_cluster = pamk.result$pamobject$clustering) #obs마다 군집배정된 결과를 저장
cluster <- data.frame(cluster, Kmedoids_cluster)
cluster <- cluster %>% mutate(Kmedoids_cluster = factor(Kmedoids_cluster))
#3d시각화
p2 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
              z=~교통, color=~Kmedoids_cluster,
              hoverinfo="text",
              text = paste("노인수 :", cluster$노인수, "<br>",
                           "복지시설 :", cluster$복지시설, "<br>",
                           "교통 :", cluster$교통, "<br>",
                           "동명 :", cluster$동명
              )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()

print(p2)

cluster %>% filter(Kmedoids_cluster == '2')
#hierarchical ----


#hierarchical 
total2_scaled <- scale(total2[-1])# 거리기반이므로 표준화필요

d <- dist(total2_scaled)

as.matrix(d)[1:4,] #거리계산(보기)

fit <- hclust(d, method="average")#군집 방법 average, 거리 euclidean을 적용
par(mfrow = c(1, 2))
plot(fit, hang=-1 , cex=0.8)#덴드로그램 확인(좌)
plot(fit, hang=-1 , cex=0.8)#덴드로그램 확인(우)
rect.hclust(fit, k=7)
#군집 수 확인, 두 그래프 모두 7개가 적당함을 알림
nc <- NbClust(total2_scaled, distance="euclidean", min.nc=2, max.nc=15, method="average")

hier_cluster <- data.frame(hier_cluster = cutree(fit, k=7))# 적용한 모델에 7개의 군집을 설정
cluster <- data.frame(cluster, hier_cluster)
cluster <- cluster %>% mutate(hier_cluster = factor(hier_cluster))

#3d시각화
p3 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
              z=~교통, color=~hier_cluster,
              hoverinfo="text",
              text = paste("노인수 :", cluster$노인수, "<br>",
                           "복지시설 :", cluster$복지시설, "<br>",
                           "교통 :", cluster$교통, "<br>",
                           "동명 :", cluster$동명
              )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()
print(p3)


#gaussian Mclust----
#Mclust 는 매개변수화된 유한 가우시안 혼합 모델에 기초한 모델 기반 군집이다. 모델은 계층적 모델 기반 집적 클러스터링에 의해 초기화된  EM 알고리즘에 의해 추정된다. 그런 다음 BIC에 따라 최적 모델을 선택한다.

summary(Mclust(total2[,2:4], parameters=T))
mclustBIC (total2[,2:4])
#VVE의 형태, 5개의 군집모델이 적합함.


mc <- Mclust(total2[,2:4], G = 5) #군집수 5개의 gaussian모델 적합
summary(mc, parameters = TRUE)
#plot.Mclust(mc)

gauss_cluster <- data.frame(gauss_cluster = mc$classification)
cluster <- data.frame(cluster, gauss_cluster)
cluster <- cluster %>% mutate(gauss_cluster = factor(gauss_cluster))

#3d시각화
p4 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
              z=~교통, color=~gauss_cluster,
              hoverinfo="text",
              text = paste("노인수 :", cluster$노인수, "<br>",
                           "복지시설 :", cluster$복지시설, "<br>",
                           "교통 :", cluster$교통, "<br>",
                           "동명 :", cluster$동명
              )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()
print(p4)

# #dbscan -----
# dbscan(total_scale, eps = .5, MinPts = 2)
# dbscan(total_scale, eps = 1, MinPts = 2)
# dbscan(total_scale, eps = 1.5, MinPts = 2)
# dbscan(total_scale, eps = 2, MinPts = 2)
# dbscan(total_scale, eps = .3, MinPts = 3)#시도
# dbscan(total_scale, eps = 1, MinPts = 3)
# dbscan(total_scale, eps = 1.5, MinPts = 3)
# dbscan(total_scale, eps = 2, MinPts = 3)
# dbscan <- dbscan(total_scale, eps = .5, MinPts = 5)#최선
# dbscan(total_scale, eps = .5, MinPts = 5)
# dbscan(total_scale, eps = 1.5, MinPts = 5)
# dbscan(total_scale, eps = 2, MinPts = 5)
# 
# dbscan$cluster
# dbscan_cluster <- data.frame(dbscan_cluster = dbscan$cluster)
# cluster <- data.frame(cluster, dbscan_cluster)
# cluster <- cluster %>% mutate(dbscan_cluster = factor(dbscan_cluster))
# 
# p5 <- plot_ly(cluster, x=~노인수, y=~복지시설, 
#               z=~교통, color=~dbscan_cluster,
#               hoverinfo="text",
#               text = paste("노인수 :", cluster$노인수, "<br>",
#                            "복지시설 :", cluster$복지시설, "<br>",
#                            "교통 :", cluster$교통, "<br>",
#                            "동명 :", cluster$동명
#               )) %>%
#   add_markers(marker = list(opacity=.9, size=4.5))%>% 
#   layout(title = '') %>%print()

#군집 파악 ---------
# 군집 3d plot 확인 및 적당한 순위 선정
table(cluster$kmeans_cluster);print(p1)   # kmeans       // 1 > 2 > 4 > 3
table(cluster$Kmedoids_cluster);print(p2) # k-medoids    // 2 > 1 > 3
table(cluster$hier_cluster);print(p3)     # hierarchical // 6 > 3 > 7 > 2 > 5 > 1 > 4
table(cluster$gauss_cluster);print(p4)    # gaussian     // 3 > 2 > 5 > 1 > 4
#최적 군집 정하기
d1 <- cluster %>% filter(kmeans_cluster == 1) %>% select(동명)
d2 <- cluster %>% filter(Kmedoids_cluster == 2) %>% select(동명)

# 계층형 군집분석의 경우 너무 세세하게 나눠지는 것을 확인. 이에따라 6,3,2,7번 총 4군집을 선택.
d3 <- cluster %>% filter(hier_cluster == 6 |
                           hier_cluster == 3 |
                           hier_cluster == 2 | 
                           hier_cluster == 7) %>% select(동명)
d4 <- cluster %>% filter(gauss_cluster == 3) %>% select(동명)
# 군집 결과 4가지 군집분석에 모두 선택된 
cluster_result <- rbind(d1, d2, d3, d4) %>% group_by(동명) %>% summarise(포함된수=n()) %>% filter(포함된수>=4)


#0811
# 최종 군집결과 시각화
cluster_final <- merge(cluster, cluster_result, by='동명', all=T)
cluster_final[is.na(cluster_final)] <- 0
cluster_final <- cluster_final %>% mutate(포함된수 = if_else(포함된수==0,'타구역' ,'입지선정구역'))


colors <- c('red', 'skyblue')
p5 <- plot_ly(cluster_final, x=~노인수, y=~복지시설, 
              z=~교통, color=~포함된수, colors = colors,
              hoverinfo="text",
              text = paste("노인수 :", cluster$노인수, "<br>",
                           "복지시설 :", cluster$복지시설, "<br>",
                           "교통 :", cluster$교통, "<br>",
                           "동명 :", cluster$동명
              )) %>%
  add_markers(marker = list(opacity=.9, size=4.5))%>% 
  layout(title = '') %>%print()

#최종 선택된 읍면동
cluster_result
cluster %>% filter(동명 == '간석동'|
                     동명 == '만수동'|
                     동명 == '부평동'|
                     동명 == '산곡동'|
                     동명 == '용현동'|
                     동명 == '주안동') %>%
  select(동명, 노인수, 교통, 복지시설)
