## 코드 작성자: A69055 최원준 -----
## LIBRARY -----
library(PerformanceAnalytics)
library(reshape2)
library(Hmisc)
library(ggmap)
library(tidyverse)
library(sf)
library(readxl)
library(writexl)
library(openxlsx)
library(leaps)
library(car)
library(caret)
library(bootstrap)
library(xgboost)




## DATA      | 데이터 로드 -----
df_float        <- read.csv('input/서울시 상권분석서비스(길단위인구-상권).csv',   fileEncoding = 'EUC-KR')
df_index        <- read.csv('input/서울시 상권분석서비스(상권변화지표-상권).csv', fileEncoding = 'EUC-KR')
df_resident     <- read.csv('input/서울시 상권분석서비스(상주인구-상권).csv',     fileEncoding = 'EUC-KR')
df_income       <- read.csv('input/서울시 상권분석서비스(소득소비-상권).csv',     fileEncoding = 'EUC-KR')
df_apt          <- read.csv('input/서울시 상권분석서비스(아파트-상권).csv',       fileEncoding = 'EUC-KR')
df_area         <- read.csv('input/서울시 상권분석서비스(영역-상권).csv',         fileEncoding = 'EUC-KR')
df_store        <- read.csv('input/서울시 상권분석서비스(점포-상권).csv',         fileEncoding = 'EUC-KR')
df_worker       <- read.csv('input/서울시 상권분석서비스(직장인구-상권).csv',     fileEncoding = 'EUC-KR')
df_fercility    <- read.csv('input/서울시 상권분석서비스(집객시설-상권).csv',     fileEncoding = 'EUC-KR')
df_sales        <- read.csv('input/서울시 상권분석서비스(추정매출-상권).csv',     fileEncoding = 'EUC-KR')
df_rent         <- read_xlsx('input/매장용빌딩_임대료공실률및수익률.xlsx',        sheet = 'cleansing')
df_instar       <- read_xlsx('input/썸트렌드_군집76.xlsx',                        sheet = 'Sheet9')
df_cluster_name <- read_xlsx('input/썸트렌드_군집76.xlsx',                        sheet = '파일명-군집번호')



## EDA       | 전체 데이터 개수 탐색 -----

# 01 > 전체 객체명 추출 (area 제외)
df_name <- c('df_float', 'df_index', 'df_resident', 'df_income', 'df_apt', 
             'df_store', 'df_worker', 'df_fercility', 'df_sales')


# 02 > 전체 객체(df)의 연도별 데이터 수 확인
data_cnt <- get(df_name[1]) %>% 
  select(기준_년분기_코드) %>% 
  mutate(df_name = df_name[1])


for (i in 2:9){
  
  temp <- get(df_name[i]) %>% 
    select(기준_년분기_코드) %>%
    mutate(df_name = df_name[i])
  
  data_cnt <- rbind(data_cnt, temp)
  
}


table(data_cnt$df_name,
      data_cnt$기준_년분기_코드)





## SETTING   | 2022년 데이터만 남기기 -----
df_float     <- df_float      %>% filter(substr(기준_년분기_코드, 1, 4) == '2022')
df_index     <- df_index      %>% filter(substr(기준_년분기_코드, 1, 4) == '2022') 
df_resident  <- df_resident   %>% filter(substr(기준_년분기_코드, 1, 4) == '2022') 
df_income    <- df_income     %>% filter(substr(기준_년분기_코드, 1, 4) == '2022') 
df_apt       <- df_apt        %>% filter(substr(기준_년분기_코드, 1, 4) == '2022')  
df_store     <- df_store      %>% filter(substr(기준_년분기_코드, 1, 4) == '2022') 
df_worker    <- df_worker     %>% filter(substr(기준_년분기_코드, 1, 4) == '2022') 
df_fercility <- df_fercility  %>% filter(substr(기준_년분기_코드, 1, 4) == '2022') 
df_sales     <- df_sales      %>% filter(substr(기준_년분기_코드, 1, 4) == '2022')





## ANALYSIS  | 상권 군집 분석 -----

# 01 > X/Y좌표 -> 위도/경도로 전환
df_coordinate <- st_as_sf(df_area,
                          coords = c('엑스좌표_값', '와이좌표_값'), 
                          crs = 2097)

df_coordinate <- st_transform(df_coordinate, crs = 4326) %>% 
  as.data.frame()

df_area <- df_coordinate %>%
  separate(geometry, into = c('위도', '경도'), sep = ',')

df_area$위도 <- gsub('c', '', df_area$위도)
df_area$위도 <- gsub('\\(', '', df_area$위도)
df_area$경도 <- gsub('\\)', '', df_area$경도)

df_area$위도 <- as.numeric(df_area$위도)
df_area$경도 <- as.numeric(df_area$경도)

rm(df_coordinate)



# 02 > 적절 군집수 탐색
km_area_withnes <- c()
km_area_between <- c()

for (i in 2:1649){
  
  set.seed(123)
  km_area <- kmeans(df_area %>% select(위도, 경도), centers = i)
  km_area_withnes[i-1] <- km_area$tot.withinss
  km_area_between[i-1] <- km_area$betweenss
  
}



# 군집내 분산/군집간 분산 --> 76개 군집 지지 (선택 O)
km_result <- data.frame(km_area_withnes,
                        km_area_between,
                        k = 2:1649)



# 엘보우 기법 --> 8개 군집 지지 (선택 X)
ggplot(km_result %>% 
         filter(k <= 50), 
       aes(x = k, y = km_area_withnes)) +
  geom_line() +
  scale_x_continuous(breaks = 1:50)

ggplot(km_result %>% 
         filter(k <= 50), 
       aes(x = k, y = km_area_between)) +
  geom_line() +
  scale_x_continuous(breaks = 1:50)



## VISUALIZE | 상권 군집 위치 표기-----

# 01 > area에 76개 군집 표기
km_area_k76     <- kmeans(df_area %>% select(위도, 경도), centers = 76)
df_area$cluster <- km_area_k76$cluster
rm(km_area_k76)



# 02 > 위치 표기
register_google(key = 'AIzaSyCuiGvq_Cir6eTombzokDN8AOLQRYEBTcM')
map_seoul <- get_map(location = 'Seoul', zoom = 12, maptype = 'roadmap')

ggmap(map_seoul) + 
  geom_point(data = df_area, aes(x = 위도, y = 경도, color = factor(cluster)))
  




## VISUALIZE | SALES -----
ggplot(df_sales)



## SETTING   | SALES     - 상권별 변수 정리 -----

# 01 > 매장 수 추가
df_sales <- left_join(df_sales, 
                      df_store %>% select(기준_년분기_코드, 상권_코드, 서비스_업종_코드, 점포_수),
                      by = c('기준_년분기_코드', '상권_코드', '서비스_업종_코드'))


# 02 > 상권별 계산
df_sales_dist <- df_sales %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    당월_매출_금액            = map_dbl(data, ~ sum(.x$당월_매출_금액, na.rm = T)),
    당월_매출_건수            = map_dbl(data, ~ sum(.x$당월_매출_건수, na.rm = T)),
    주중_매출_금액            = map_dbl(data, ~ sum(.x$주중_매출_금액, na.rm = T)),
    주말_매출_금액            = map_dbl(data, ~ sum(.x$주말_매출_금액, na.rm = T)),
    월요일_매출_금액          = map_dbl(data, ~ sum(.x$월요일_매출_금액, na.rm = T)),
    화요일_매출_금액          = map_dbl(data, ~ sum(.x$화요일_매출_금액, na.rm = T)),
    수요일_매출_금액          = map_dbl(data, ~ sum(.x$수요일_매출_금액, na.rm = T)),
    목요일_매출_금액          = map_dbl(data, ~ sum(.x$목요일_매출_금액, na.rm = T)),
    금요일_매출_금액          = map_dbl(data, ~ sum(.x$금요일_매출_금액, na.rm = T)),
    토요일_매출_금액          = map_dbl(data, ~ sum(.x$토요일_매출_금액, na.rm = T)),
    일요일_매출_금액          = map_dbl(data, ~ sum(.x$일요일_매출_금액, na.rm = T)),
    시간대_00.06_매출_금액    = map_dbl(data, ~ sum(.x$시간대_00.06_매출_금액, na.rm = T)),
    시간대_06.11_매출_금액    = map_dbl(data, ~ sum(.x$시간대_06.11_매출_금액, na.rm = T)),
    시간대_11.14_매출_금액    = map_dbl(data, ~ sum(.x$시간대_11.14_매출_금액, na.rm = T)),
    시간대_14.17_매출_금액    = map_dbl(data, ~ sum(.x$시간대_14.17_매출_금액, na.rm = T)),
    시간대_17.21_매출_금액    = map_dbl(data, ~ sum(.x$시간대_17.21_매출_금액, na.rm = T)),
    시간대_21.24_매출_금액    = map_dbl(data, ~ sum(.x$시간대_21.24_매출_금액, na.rm = T)),
    남성_매출_금액            = map_dbl(data, ~ sum(.x$남성_매출_금액, na.rm = T)),
    여성_매출_금액            = map_dbl(data, ~ sum(.x$여성_매출_금액, na.rm = T)),
    연령대_10_매출_금액       = map_dbl(data, ~ sum(.x$연령대_10_매출_금액, na.rm = T)),
    연령대_20_매출_금액       = map_dbl(data, ~ sum(.x$연령대_20_매출_금액, na.rm = T)),
    연령대_30_매출_금액       = map_dbl(data, ~ sum(.x$연령대_30_매출_금액, na.rm = T)),
    연령대_40_매출_금액       = map_dbl(data, ~ sum(.x$연령대_40_매출_금액, na.rm = T)),
    연령대_50_매출_금액       = map_dbl(data, ~ sum(.x$연령대_50_매출_금액, na.rm = T)),
    연령대_60_이상_매출_금액  = map_dbl(data, ~ sum(.x$연령대_60_이상_매출_금액, na.rm = T)),
    주중_매출_건수            = map_dbl(data, ~ sum(.x$주중_매출_건수, na.rm = T)),
    주말_매출_건수            = map_dbl(data, ~ sum(.x$주말_매출_건수, na.rm = T)),
    월요일_매출_건수          = map_dbl(data, ~ sum(.x$월요일_매출_건수, na.rm = T)),
    화요일_매출_건수          = map_dbl(data, ~ sum(.x$화요일_매출_건수, na.rm = T)),
    수요일_매출_건수          = map_dbl(data, ~ sum(.x$수요일_매출_건수, na.rm = T)),
    목요일_매출_건수          = map_dbl(data, ~ sum(.x$목요일_매출_건수, na.rm = T)),
    금요일_매출_건수          = map_dbl(data, ~ sum(.x$금요일_매출_건수, na.rm = T)),
    토요일_매출_건수          = map_dbl(data, ~ sum(.x$토요일_매출_건수, na.rm = T)),
    일요일_매출_건수          = map_dbl(data, ~ sum(.x$일요일_매출_건수, na.rm = T)),
    시간대_건수.06_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.06_매출_건수, na.rm = T)),
    시간대_건수.11_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.11_매출_건수, na.rm = T)),
    시간대_건수.14_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.14_매출_건수, na.rm = T)),
    시간대_건수.17_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.17_매출_건수, na.rm = T)),
    시간대_건수.21_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.21_매출_건수, na.rm = T)),
    시간대_건수.24_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.24_매출_건수, na.rm = T)),
    남성_매출_건수            = map_dbl(data, ~ sum(.x$남성_매출_건수, na.rm = T)),
    여성_매출_건수            = map_dbl(data, ~ sum(.x$여성_매출_건수, na.rm = T)),
    연령대_10_매출_건수       = map_dbl(data, ~ sum(.x$연령대_10_매출_건수, na.rm = T)),
    연령대_20_매출_건수       = map_dbl(data, ~ sum(.x$연령대_20_매출_건수, na.rm = T)),
    연령대_30_매출_건수       = map_dbl(data, ~ sum(.x$연령대_30_매출_건수, na.rm = T)),
    연령대_40_매출_건수       = map_dbl(data, ~ sum(.x$연령대_40_매출_건수, na.rm = T)),
    연령대_50_매출_건수       = map_dbl(data, ~ sum(.x$연령대_50_매출_건수, na.rm = T)),
    연령대_60_이상_매출_건수  = map_dbl(data, ~ sum(.x$연령대_60_이상_매출_건수, na.rm = T)),
    
    점포_수                   = map_dbl(data, ~ sum(.x$점포_수)),
    점포_당_매출_금액         = 당월_매출_금액 / 점포_수
    
  ) %>% 
  select(-data, -점포_수)





## SETTING   | SALES     - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_sales <- left_join(df_sales, 
                      df_area %>% select(상권_코드, cluster),
                      by = c('상권_코드'))


# 02 > 군집별 계산
df_sales_cluster <- df_sales %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    당월_매출_금액            = map_dbl(data, ~ sum(.x$당월_매출_금액, na.rm = T)),
    당월_매출_건수            = map_dbl(data, ~ sum(.x$당월_매출_건수, na.rm = T)),
    주중_매출_금액            = map_dbl(data, ~ sum(.x$주중_매출_금액, na.rm = T)),
    주말_매출_금액            = map_dbl(data, ~ sum(.x$주말_매출_금액, na.rm = T)),
    월요일_매출_금액          = map_dbl(data, ~ sum(.x$월요일_매출_금액, na.rm = T)),
    화요일_매출_금액          = map_dbl(data, ~ sum(.x$화요일_매출_금액, na.rm = T)),
    수요일_매출_금액          = map_dbl(data, ~ sum(.x$수요일_매출_금액, na.rm = T)),
    목요일_매출_금액          = map_dbl(data, ~ sum(.x$목요일_매출_금액, na.rm = T)),
    금요일_매출_금액          = map_dbl(data, ~ sum(.x$금요일_매출_금액, na.rm = T)),
    토요일_매출_금액          = map_dbl(data, ~ sum(.x$토요일_매출_금액, na.rm = T)),
    일요일_매출_금액          = map_dbl(data, ~ sum(.x$일요일_매출_금액, na.rm = T)),
    시간대_00.06_매출_금액    = map_dbl(data, ~ sum(.x$시간대_00.06_매출_금액, na.rm = T)),
    시간대_06.11_매출_금액    = map_dbl(data, ~ sum(.x$시간대_06.11_매출_금액, na.rm = T)),
    시간대_11.14_매출_금액    = map_dbl(data, ~ sum(.x$시간대_11.14_매출_금액, na.rm = T)),
    시간대_14.17_매출_금액    = map_dbl(data, ~ sum(.x$시간대_14.17_매출_금액, na.rm = T)),
    시간대_17.21_매출_금액    = map_dbl(data, ~ sum(.x$시간대_17.21_매출_금액, na.rm = T)),
    시간대_21.24_매출_금액    = map_dbl(data, ~ sum(.x$시간대_21.24_매출_금액, na.rm = T)),
    남성_매출_금액            = map_dbl(data, ~ sum(.x$남성_매출_금액, na.rm = T)),
    여성_매출_금액            = map_dbl(data, ~ sum(.x$여성_매출_금액, na.rm = T)),
    연령대_10_매출_금액       = map_dbl(data, ~ sum(.x$연령대_10_매출_금액, na.rm = T)),
    연령대_20_매출_금액       = map_dbl(data, ~ sum(.x$연령대_20_매출_금액, na.rm = T)),
    연령대_30_매출_금액       = map_dbl(data, ~ sum(.x$연령대_30_매출_금액, na.rm = T)),
    연령대_40_매출_금액       = map_dbl(data, ~ sum(.x$연령대_40_매출_금액, na.rm = T)),
    연령대_50_매출_금액       = map_dbl(data, ~ sum(.x$연령대_50_매출_금액, na.rm = T)),
    연령대_60_이상_매출_금액  = map_dbl(data, ~ sum(.x$연령대_60_이상_매출_금액, na.rm = T)),
    
    주중_매출_건수            = map_dbl(data, ~ sum(.x$주중_매출_건수, na.rm = T)),
    주말_매출_건수            = map_dbl(data, ~ sum(.x$주말_매출_건수, na.rm = T)),
    월요일_매출_건수          = map_dbl(data, ~ sum(.x$월요일_매출_건수, na.rm = T)),
    화요일_매출_건수          = map_dbl(data, ~ sum(.x$화요일_매출_건수, na.rm = T)),
    수요일_매출_건수          = map_dbl(data, ~ sum(.x$수요일_매출_건수, na.rm = T)),
    목요일_매출_건수          = map_dbl(data, ~ sum(.x$목요일_매출_건수, na.rm = T)),
    금요일_매출_건수          = map_dbl(data, ~ sum(.x$금요일_매출_건수, na.rm = T)),
    토요일_매출_건수          = map_dbl(data, ~ sum(.x$토요일_매출_건수, na.rm = T)),
    일요일_매출_건수          = map_dbl(data, ~ sum(.x$일요일_매출_건수, na.rm = T)),
    시간대_건수.06_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.06_매출_건수, na.rm = T)),
    시간대_건수.11_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.11_매출_건수, na.rm = T)),
    시간대_건수.14_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.14_매출_건수, na.rm = T)),
    시간대_건수.17_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.17_매출_건수, na.rm = T)),
    시간대_건수.21_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.21_매출_건수, na.rm = T)),
    시간대_건수.24_매출_건수  = map_dbl(data, ~ sum(.x$시간대_건수.24_매출_건수, na.rm = T)),
    남성_매출_건수            = map_dbl(data, ~ sum(.x$남성_매출_건수, na.rm = T)),
    여성_매출_건수            = map_dbl(data, ~ sum(.x$여성_매출_건수, na.rm = T)),
    연령대_10_매출_건수       = map_dbl(data, ~ sum(.x$연령대_10_매출_건수, na.rm = T)),
    연령대_20_매출_건수       = map_dbl(data, ~ sum(.x$연령대_20_매출_건수, na.rm = T)),
    연령대_30_매출_건수       = map_dbl(data, ~ sum(.x$연령대_30_매출_건수, na.rm = T)),
    연령대_40_매출_건수       = map_dbl(data, ~ sum(.x$연령대_40_매출_건수, na.rm = T)),
    연령대_50_매출_건수       = map_dbl(data, ~ sum(.x$연령대_50_매출_건수, na.rm = T)),
    연령대_60_이상_매출_건수  = map_dbl(data, ~ sum(.x$연령대_60_이상_매출_건수, na.rm = T)),
    
    점포_수                   = map_dbl(data, ~ sum(.x$점포_수)),
    점포_당_매출_금액         = 당월_매출_금액 / 점포_수
    
  ) %>% 
  select(-data, -점포_수)






## SETTING   | STORE     - 상권별 변수 정리 -----

# 01 > 분기별-상권별 계산
df_store_dist <- df_store %>% 
  nest(data = -c(기준_년분기_코드, 상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    점포_수            = map_dbl(data, ~ sum(.x$점포_수, na.rm = T)),
    유사_업종_점포_수  = map_dbl(data, ~ sum(.x$유사_업종_점포_수, na.rm = T)),
    개업_점포_수       = map_dbl(data, ~ sum(.x$개업_점포_수, na.rm = T)),
    폐업_점포_수       = map_dbl(data, ~ sum(.x$폐업_점포_수, na.rm = T)),
    프랜차이즈_점포_수 = map_dbl(data, ~ sum(.x$프랜차이즈_점포_수, na.rm = T)),
    개업_율            = map_dbl(data, ~ mean(.x$개업_율, na.rm = T)),
    폐업_률            = map_dbl(data, ~ mean(.x$폐업_률, na.rm = T))

  ) %>% 
  select(-data)



# 02 > 상권별 계산
df_store_dist <- df_store_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    점포_수            = map_dbl(data, ~ mean(.x$점포_수, na.rm = T)),
    유사_업종_점포_수  = map_dbl(data, ~ mean(.x$유사_업종_점포_수, na.rm = T)),
    개업_점포_수       = map_dbl(data, ~ mean(.x$개업_점포_수, na.rm = T)),
    폐업_점포_수       = map_dbl(data, ~ mean(.x$폐업_점포_수, na.rm = T)),
    프랜차이즈_점포_수 = map_dbl(data, ~ mean(.x$프랜차이즈_점포_수, na.rm = T)),
    개업_율            = map_dbl(data, ~ mean(.x$개업_율, na.rm = T)),
    폐업_률            = map_dbl(data, ~ mean(.x$폐업_률, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | STORE     - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_store <- left_join(df_store, 
                      df_area %>% select(상권_코드, cluster),
                      by = c('상권_코드'))


# 02 > 분기별-군집별 계산
df_store_cluster <- df_store %>% 
  nest(data = -c(기준_년분기_코드, cluster)) %>% 
  mutate(
    
    점포_수            = map_dbl(data, ~ sum(.x$점포_수, na.rm = T)),
    유사_업종_점포_수  = map_dbl(data, ~ sum(.x$유사_업종_점포_수, na.rm = T)),
    개업_점포_수       = map_dbl(data, ~ sum(.x$개업_점포_수, na.rm = T)),
    폐업_점포_수       = map_dbl(data, ~ sum(.x$폐업_점포_수, na.rm = T)),
    프랜차이즈_점포_수 = map_dbl(data, ~ sum(.x$프랜차이즈_점포_수, na.rm = T)),
    개업_율            = map_dbl(data, ~ mean(.x$개업_율, na.rm = T)),
    폐업_률            = map_dbl(data, ~ mean(.x$폐업_률, na.rm = T))
    
  ) %>% 
  select(-data)


# 03 > 군집별 계산
df_store_cluster <- df_store_cluster %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    점포_수            = map_dbl(data, ~ mean(.x$점포_수, na.rm = T)),
    유사_업종_점포_수  = map_dbl(data, ~ mean(.x$유사_업종_점포_수, na.rm = T)),
    개업_점포_수       = map_dbl(data, ~ mean(.x$개업_점포_수, na.rm = T)),
    폐업_점포_수       = map_dbl(data, ~ mean(.x$폐업_점포_수, na.rm = T)),
    프랜차이즈_점포_수 = map_dbl(data, ~ mean(.x$프랜차이즈_점포_수, na.rm = T)),
    개업_율            = map_dbl(data, ~ mean(.x$개업_율, na.rm = T)),
    폐업_률            = map_dbl(data, ~ mean(.x$폐업_률, na.rm = T))
    
  ) %>% 
  select(-data)



## SETTING   | APT       - 상권별 변수 정리 -----

# 01 > 분기별-상권별 계산
df_apt_dist <- df_apt %>% 
  nest(data = -c(기준_년분기_코드, 상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    아파트_단지_수                        = map_dbl(data, ~ sum(.x$아파트_단지_수, na.rm = T)),
    아파트_면적_66_제곱미터_미만_세대_수  = map_dbl(data, ~ sum(.x$아파트_면적_66_제곱미터_미만_세대_수, na.rm = T)),
    아파트_면적_66_제곱미터_세대_수       = map_dbl(data, ~ sum(.x$아파트_면적_66_제곱미터_세대_수, na.rm = T)),
    아파트_면적_99_제곱미터_세대_수       = map_dbl(data, ~ sum(.x$아파트_면적_99_제곱미터_세대_수, na.rm = T)),
    아파트_면적_132_제곱미터_세대_수      = map_dbl(data, ~ sum(.x$아파트_면적_132_제곱미터_세대_수, na.rm = T)),
    아파트_면적_165_제곱미터_세대_수      = map_dbl(data, ~ sum(.x$아파트_면적_165_제곱미터_세대_수, na.rm = T)),
    아파트_가격_1_억_미만_세대_수         = map_dbl(data, ~ sum(.x$아파트_가격_1_억_미만_세대_수, na.rm = T)),
    아파트_가격_1_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_1_억_세대_수, na.rm = T)),
    아파트_가격_2_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_2_억_세대_수, na.rm = T)),
    아파트_가격_3_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_3_억_세대_수, na.rm = T)),
    아파트_가격_4_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_4_억_세대_수, na.rm = T)),
    아파트_가격_5_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_5_억_세대_수, na.rm = T)),
    아파트_가격_6_억_이상_세대_수         = map_dbl(data, ~ sum(.x$아파트_가격_6_억_이상_세대_수, na.rm = T)),
    아파트_평균_면적                      = map_dbl(data, ~ mean(.x$아파트_평균_면적, na.rm = T)),
    아파트_평균_시가                      = map_dbl(data, ~ mean(.x$아파트_평균_시가, na.rm = T))

    
  ) %>% 
  select(-data)


# 02 > 상권별 계산
df_apt_dist <- df_apt_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    아파트_단지_수                        = map_dbl(data, ~ mean(.x$아파트_단지_수, na.rm = T)),
    아파트_면적_66_제곱미터_미만_세대_수  = map_dbl(data, ~ mean(.x$아파트_면적_66_제곱미터_미만_세대_수, na.rm = T)),
    아파트_면적_66_제곱미터_세대_수       = map_dbl(data, ~ mean(.x$아파트_면적_66_제곱미터_세대_수, na.rm = T)),
    아파트_면적_99_제곱미터_세대_수       = map_dbl(data, ~ mean(.x$아파트_면적_99_제곱미터_세대_수, na.rm = T)),
    아파트_면적_132_제곱미터_세대_수      = map_dbl(data, ~ mean(.x$아파트_면적_132_제곱미터_세대_수, na.rm = T)),
    아파트_면적_165_제곱미터_세대_수      = map_dbl(data, ~ mean(.x$아파트_면적_165_제곱미터_세대_수, na.rm = T)),
    아파트_가격_1_억_미만_세대_수         = map_dbl(data, ~ mean(.x$아파트_가격_1_억_미만_세대_수, na.rm = T)),
    아파트_가격_1_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_1_억_세대_수, na.rm = T)),
    아파트_가격_2_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_2_억_세대_수, na.rm = T)),
    아파트_가격_3_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_3_억_세대_수, na.rm = T)),
    아파트_가격_4_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_4_억_세대_수, na.rm = T)),
    아파트_가격_5_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_5_억_세대_수, na.rm = T)),
    아파트_가격_6_억_이상_세대_수         = map_dbl(data, ~ mean(.x$아파트_가격_6_억_이상_세대_수, na.rm = T)),
    아파트_평균_면적                      = map_dbl(data, ~ mean(.x$아파트_평균_면적, na.rm = T)),
    아파트_평균_시가                      = map_dbl(data, ~ mean(.x$아파트_평균_시가, na.rm = T))
    
    
  ) %>% 
  select(-data)





## SETTING   | APT       - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_apt <- left_join(df_apt, 
                    df_area %>% select(상권_코드, cluster),
                    by = c('상권_코드'))


# 02 > 분기별-군집별 계산
df_apt_cluster <- df_apt %>% 
  nest(data = -c(기준_년분기_코드, cluster)) %>% 
  mutate(
    
    아파트_단지_수                        = map_dbl(data, ~ sum(.x$아파트_단지_수, na.rm = T)),
    아파트_면적_66_제곱미터_미만_세대_수  = map_dbl(data, ~ sum(.x$아파트_면적_66_제곱미터_미만_세대_수, na.rm = T)),
    아파트_면적_66_제곱미터_세대_수       = map_dbl(data, ~ sum(.x$아파트_면적_66_제곱미터_세대_수, na.rm = T)),
    아파트_면적_99_제곱미터_세대_수       = map_dbl(data, ~ sum(.x$아파트_면적_99_제곱미터_세대_수, na.rm = T)),
    아파트_면적_132_제곱미터_세대_수      = map_dbl(data, ~ sum(.x$아파트_면적_132_제곱미터_세대_수, na.rm = T)),
    아파트_면적_165_제곱미터_세대_수      = map_dbl(data, ~ sum(.x$아파트_면적_165_제곱미터_세대_수, na.rm = T)),
    아파트_가격_1_억_미만_세대_수         = map_dbl(data, ~ sum(.x$아파트_가격_1_억_미만_세대_수, na.rm = T)),
    아파트_가격_1_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_1_억_세대_수, na.rm = T)),
    아파트_가격_2_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_2_억_세대_수, na.rm = T)),
    아파트_가격_3_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_3_억_세대_수, na.rm = T)),
    아파트_가격_4_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_4_억_세대_수, na.rm = T)),
    아파트_가격_5_억_세대_수              = map_dbl(data, ~ sum(.x$아파트_가격_5_억_세대_수, na.rm = T)),
    아파트_가격_6_억_이상_세대_수         = map_dbl(data, ~ sum(.x$아파트_가격_6_억_이상_세대_수, na.rm = T)),
    아파트_평균_면적                      = map_dbl(data, ~ mean(.x$아파트_평균_면적, na.rm = T)),
    아파트_평균_시가                      = map_dbl(data, ~ mean(.x$아파트_평균_시가, na.rm = T))
    
  ) %>% 
  select(-data)


# 03 > 군집별 계산
df_apt_cluster <- df_apt_cluster %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    아파트_단지_수                        = map_dbl(data, ~ mean(.x$아파트_단지_수, na.rm = T)),
    아파트_면적_66_제곱미터_미만_세대_수  = map_dbl(data, ~ mean(.x$아파트_면적_66_제곱미터_미만_세대_수, na.rm = T)),
    아파트_면적_66_제곱미터_세대_수       = map_dbl(data, ~ mean(.x$아파트_면적_66_제곱미터_세대_수, na.rm = T)),
    아파트_면적_99_제곱미터_세대_수       = map_dbl(data, ~ mean(.x$아파트_면적_99_제곱미터_세대_수, na.rm = T)),
    아파트_면적_132_제곱미터_세대_수      = map_dbl(data, ~ mean(.x$아파트_면적_132_제곱미터_세대_수, na.rm = T)),
    아파트_면적_165_제곱미터_세대_수      = map_dbl(data, ~ mean(.x$아파트_면적_165_제곱미터_세대_수, na.rm = T)),
    아파트_가격_1_억_미만_세대_수         = map_dbl(data, ~ mean(.x$아파트_가격_1_억_미만_세대_수, na.rm = T)),
    아파트_가격_1_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_1_억_세대_수, na.rm = T)),
    아파트_가격_2_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_2_억_세대_수, na.rm = T)),
    아파트_가격_3_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_3_억_세대_수, na.rm = T)),
    아파트_가격_4_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_4_억_세대_수, na.rm = T)),
    아파트_가격_5_억_세대_수              = map_dbl(data, ~ mean(.x$아파트_가격_5_억_세대_수, na.rm = T)),
    아파트_가격_6_억_이상_세대_수         = map_dbl(data, ~ mean(.x$아파트_가격_6_억_이상_세대_수, na.rm = T)),
    아파트_평균_면적                      = map_dbl(data, ~ mean(.x$아파트_평균_면적, na.rm = T)),
    아파트_평균_시가                      = map_dbl(data, ~ mean(.x$아파트_평균_시가, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | FERCILITY - 상권별 변수 정리 -----

# 01 > 분기별-상권별 분석
df_fercility_dist <- df_fercility %>% 
  nest(data = -c(기준_년분기_코드, 상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    집객시설_수    = map_dbl(data, ~ sum(.x$집객시설_수, na.rm = T)),
    관공서_수      = map_dbl(data, ~ sum(.x$관공서_수, na.rm = T)),
    은행_수        = map_dbl(data, ~ sum(.x$은행_수, na.rm = T)),
    종합병원_수    = map_dbl(data, ~ sum(.x$종합병원_수, na.rm = T)),
    일반_병원_수   = map_dbl(data, ~ sum(.x$일반_병원_수, na.rm = T)),
    약국_수        = map_dbl(data, ~ sum(.x$약국_수, na.rm = T)),
    유치원_수      = map_dbl(data, ~ sum(.x$유치원_수, na.rm = T)),
    초등학교_수    = map_dbl(data, ~ sum(.x$초등학교_수, na.rm = T)),
    중학교_수      = map_dbl(data, ~ sum(.x$중학교_수, na.rm = T)),
    고등학교_수    = map_dbl(data, ~ sum(.x$고등학교_수, na.rm = T)),
    대학교_수      = map_dbl(data, ~ sum(.x$대학교_수, na.rm = T)),
    백화점_수      = map_dbl(data, ~ sum(.x$백화점_수, na.rm = T)),
    슈퍼마켓_수    = map_dbl(data, ~ sum(.x$슈퍼마켓_수, na.rm = T)),
    극장_수        = map_dbl(data, ~ sum(.x$극장_수, na.rm = T)),
    숙박_시설_수   = map_dbl(data, ~ sum(.x$숙박_시설_수, na.rm = T)),
    공항_수        = map_dbl(data, ~ sum(.x$공항_수, na.rm = T)),
    철도_역_수     = map_dbl(data, ~ sum(.x$철도_역_수, na.rm = T)),
    버스_터미널_수 = map_dbl(data, ~ sum(.x$버스_터미널_수, na.rm = T)),
    지하철_역_수   = map_dbl(data, ~ sum(.x$지하철_역_수, na.rm = T)),
    버스_정거장_수 = map_dbl(data, ~ sum(.x$버스_정거장_수, na.rm = T))

  ) %>% 
  select(-data)


# 02 > 상권별 분석
df_fercility_dist <- df_fercility_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    집객시설_수    = map_dbl(data, ~ mean(.x$집객시설_수, na.rm = T)),
    관공서_수      = map_dbl(data, ~ mean(.x$관공서_수, na.rm = T)),
    은행_수        = map_dbl(data, ~ mean(.x$은행_수, na.rm = T)),
    종합병원_수    = map_dbl(data, ~ mean(.x$종합병원_수, na.rm = T)),
    일반_병원_수   = map_dbl(data, ~ mean(.x$일반_병원_수, na.rm = T)),
    약국_수        = map_dbl(data, ~ mean(.x$약국_수, na.rm = T)),
    유치원_수      = map_dbl(data, ~ mean(.x$유치원_수, na.rm = T)),
    초등학교_수    = map_dbl(data, ~ mean(.x$초등학교_수, na.rm = T)),
    중학교_수      = map_dbl(data, ~ mean(.x$중학교_수, na.rm = T)),
    고등학교_수    = map_dbl(data, ~ mean(.x$고등학교_수, na.rm = T)),
    대학교_수      = map_dbl(data, ~ mean(.x$대학교_수, na.rm = T)),
    백화점_수      = map_dbl(data, ~ mean(.x$백화점_수, na.rm = T)),
    슈퍼마켓_수    = map_dbl(data, ~ mean(.x$슈퍼마켓_수, na.rm = T)),
    극장_수        = map_dbl(data, ~ mean(.x$극장_수, na.rm = T)),
    숙박_시설_수   = map_dbl(data, ~ mean(.x$숙박_시설_수, na.rm = T)),
    공항_수        = map_dbl(data, ~ mean(.x$공항_수, na.rm = T)),
    철도_역_수     = map_dbl(data, ~ mean(.x$철도_역_수, na.rm = T)),
    버스_터미널_수 = map_dbl(data, ~ mean(.x$버스_터미널_수, na.rm = T)),
    지하철_역_수   = map_dbl(data, ~ mean(.x$지하철_역_수, na.rm = T)),
    버스_정거장_수 = map_dbl(data, ~ mean(.x$버스_정거장_수, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | FERCILITY - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_fercility <- left_join(df_fercility, 
                          df_area %>% select(상권_코드, cluster),
                          by = c('상권_코드'))


# 02 > 분기별-군집별 계산
df_fercility_cluster <- df_fercility %>% 
  nest(data = -c(기준_년분기_코드, cluster)) %>% 
  mutate(
    
    집객시설_수    = map_dbl(data, ~ sum(.x$집객시설_수, na.rm = T)),
    관공서_수      = map_dbl(data, ~ sum(.x$관공서_수, na.rm = T)),
    은행_수        = map_dbl(data, ~ sum(.x$은행_수, na.rm = T)),
    종합병원_수    = map_dbl(data, ~ sum(.x$종합병원_수, na.rm = T)),
    일반_병원_수   = map_dbl(data, ~ sum(.x$일반_병원_수, na.rm = T)),
    약국_수        = map_dbl(data, ~ sum(.x$약국_수, na.rm = T)),
    유치원_수      = map_dbl(data, ~ sum(.x$유치원_수, na.rm = T)),
    초등학교_수    = map_dbl(data, ~ sum(.x$초등학교_수, na.rm = T)),
    중학교_수      = map_dbl(data, ~ sum(.x$중학교_수, na.rm = T)),
    고등학교_수    = map_dbl(data, ~ sum(.x$고등학교_수, na.rm = T)),
    대학교_수      = map_dbl(data, ~ sum(.x$대학교_수, na.rm = T)),
    백화점_수      = map_dbl(data, ~ sum(.x$백화점_수, na.rm = T)),
    슈퍼마켓_수    = map_dbl(data, ~ sum(.x$슈퍼마켓_수, na.rm = T)),
    극장_수        = map_dbl(data, ~ sum(.x$극장_수, na.rm = T)),
    숙박_시설_수   = map_dbl(data, ~ sum(.x$숙박_시설_수, na.rm = T)),
    공항_수        = map_dbl(data, ~ sum(.x$공항_수, na.rm = T)),
    철도_역_수     = map_dbl(data, ~ sum(.x$철도_역_수, na.rm = T)),
    버스_터미널_수 = map_dbl(data, ~ sum(.x$버스_터미널_수, na.rm = T)),
    지하철_역_수   = map_dbl(data, ~ sum(.x$지하철_역_수, na.rm = T)),
    버스_정거장_수 = map_dbl(data, ~ sum(.x$버스_정거장_수, na.rm = T))
    
  ) %>% 
  select(-data)


# 03 > 군집별 계산
df_fercility_cluster <- df_fercility_cluster %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    집객시설_수    = map_dbl(data, ~ mean(.x$집객시설_수, na.rm = T)),
    관공서_수      = map_dbl(data, ~ mean(.x$관공서_수, na.rm = T)),
    은행_수        = map_dbl(data, ~ mean(.x$은행_수, na.rm = T)),
    종합병원_수    = map_dbl(data, ~ mean(.x$종합병원_수, na.rm = T)),
    일반_병원_수   = map_dbl(data, ~ mean(.x$일반_병원_수, na.rm = T)),
    약국_수        = map_dbl(data, ~ mean(.x$약국_수, na.rm = T)),
    유치원_수      = map_dbl(data, ~ mean(.x$유치원_수, na.rm = T)),
    초등학교_수    = map_dbl(data, ~ mean(.x$초등학교_수, na.rm = T)),
    중학교_수      = map_dbl(data, ~ mean(.x$중학교_수, na.rm = T)),
    고등학교_수    = map_dbl(data, ~ mean(.x$고등학교_수, na.rm = T)),
    대학교_수      = map_dbl(data, ~ mean(.x$대학교_수, na.rm = T)),
    백화점_수      = map_dbl(data, ~ mean(.x$백화점_수, na.rm = T)),
    슈퍼마켓_수    = map_dbl(data, ~ mean(.x$슈퍼마켓_수, na.rm = T)),
    극장_수        = map_dbl(data, ~ mean(.x$극장_수, na.rm = T)),
    숙박_시설_수   = map_dbl(data, ~ mean(.x$숙박_시설_수, na.rm = T)),
    공항_수        = map_dbl(data, ~ mean(.x$공항_수, na.rm = T)),
    철도_역_수     = map_dbl(data, ~ mean(.x$철도_역_수, na.rm = T)),
    버스_터미널_수 = map_dbl(data, ~ mean(.x$버스_터미널_수, na.rm = T)),
    지하철_역_수   = map_dbl(data, ~ mean(.x$지하철_역_수, na.rm = T)),
    버스_정거장_수 = map_dbl(data, ~ mean(.x$버스_정거장_수, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | FLOAT     - 상권별 변수 정리 -----
df_float_dist <- df_float %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    총_유동인구_수             = map_dbl(data, ~ sum(.x$총_유동인구_수, na.rm = T)),
    남성_유동인구_수           = map_dbl(data, ~ sum(.x$남성_유동인구_수, na.rm = T)),
    여성_유동인구_수           = map_dbl(data, ~ sum(.x$여성_유동인구_수, na.rm = T)),
    연령대_10_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_10_유동인구_수, na.rm = T)),
    연령대_20_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_20_유동인구_수, na.rm = T)),
    연령대_30_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_30_유동인구_수, na.rm = T)),
    연령대_40_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_40_유동인구_수, na.rm = T)),
    연령대_50_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_50_유동인구_수, na.rm = T)),
    연령대_60_이상_유동인구_수 = map_dbl(data, ~ sum(.x$연령대_60_이상_유동인구_수, na.rm = T)),
    시간대_00_06_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_00_06_유동인구_수, na.rm = T)),
    시간대_06_11_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_06_11_유동인구_수, na.rm = T)),
    시간대_11_14_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_11_14_유동인구_수, na.rm = T)),
    시간대_14_17_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_14_17_유동인구_수, na.rm = T)),
    시간대_17_21_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_17_21_유동인구_수, na.rm = T)),
    시간대_21_24_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_21_24_유동인구_수, na.rm = T)),
    월요일_유동인구_수         = map_dbl(data, ~ sum(.x$월요일_유동인구_수, na.rm = T)),
    화요일_유동인구_수         = map_dbl(data, ~ sum(.x$화요일_유동인구_수, na.rm = T)),
    수요일_유동인구_수         = map_dbl(data, ~ sum(.x$수요일_유동인구_수, na.rm = T)),
    목요일_유동인구_수         = map_dbl(data, ~ sum(.x$목요일_유동인구_수, na.rm = T)),
    금요일_유동인구_수         = map_dbl(data, ~ sum(.x$금요일_유동인구_수, na.rm = T)),
    토요일_유동인구_수         = map_dbl(data, ~ sum(.x$토요일_유동인구_수, na.rm = T)),
    일요일_유동인구_수         = map_dbl(data, ~ sum(.x$일요일_유동인구_수, na.rm = T))

  ) %>% 
  select(-data)





## SETTING   | FLOAT     - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_float <- left_join(df_float, 
                          df_area %>% select(상권_코드, cluster),
                          by = c('상권_코드'))


# 02 > 군집별 계산
df_float_cluster <- df_float %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    총_유동인구_수             = map_dbl(data, ~ sum(.x$총_유동인구_수, na.rm = T)),
    남성_유동인구_수           = map_dbl(data, ~ sum(.x$남성_유동인구_수, na.rm = T)),
    여성_유동인구_수           = map_dbl(data, ~ sum(.x$여성_유동인구_수, na.rm = T)),
    연령대_10_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_10_유동인구_수, na.rm = T)),
    연령대_20_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_20_유동인구_수, na.rm = T)),
    연령대_30_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_30_유동인구_수, na.rm = T)),
    연령대_40_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_40_유동인구_수, na.rm = T)),
    연령대_50_유동인구_수      = map_dbl(data, ~ sum(.x$연령대_50_유동인구_수, na.rm = T)),
    연령대_60_이상_유동인구_수 = map_dbl(data, ~ sum(.x$연령대_60_이상_유동인구_수, na.rm = T)),
    시간대_00_06_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_00_06_유동인구_수, na.rm = T)),
    시간대_06_11_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_06_11_유동인구_수, na.rm = T)),
    시간대_11_14_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_11_14_유동인구_수, na.rm = T)),
    시간대_14_17_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_14_17_유동인구_수, na.rm = T)),
    시간대_17_21_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_17_21_유동인구_수, na.rm = T)),
    시간대_21_24_유동인구_수   = map_dbl(data, ~ sum(.x$시간대_21_24_유동인구_수, na.rm = T)),
    월요일_유동인구_수         = map_dbl(data, ~ sum(.x$월요일_유동인구_수, na.rm = T)),
    화요일_유동인구_수         = map_dbl(data, ~ sum(.x$화요일_유동인구_수, na.rm = T)),
    수요일_유동인구_수         = map_dbl(data, ~ sum(.x$수요일_유동인구_수, na.rm = T)),
    목요일_유동인구_수         = map_dbl(data, ~ sum(.x$목요일_유동인구_수, na.rm = T)),
    금요일_유동인구_수         = map_dbl(data, ~ sum(.x$금요일_유동인구_수, na.rm = T)),
    토요일_유동인구_수         = map_dbl(data, ~ sum(.x$토요일_유동인구_수, na.rm = T)),
    일요일_유동인구_수         = map_dbl(data, ~ sum(.x$일요일_유동인구_수, na.rm = T))
    
  ) %>% 
  select(-data)



## SETTING   | INCOME    - 상권별 변수 정리 -----
df_income_dist <- df_income %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    월_평균_소득_금액     = map_dbl(data, ~ mean(.x$월_평균_소득_금액, na.rm = T)),
    소득_구간_코드        = map_dbl(data, ~ mean(.x$소득_구간_코드, na.rm = T)),
    지출_총금액           = map_dbl(data, ~ sum(.x$지출_총금액, na.rm = T)),
    식료품_지출_총금액    = map_dbl(data, ~ sum(.x$식료품_지출_총금액, na.rm = T)),
    의류_신발_지출_총금액 = map_dbl(data, ~ sum(.x$의류_신발_지출_총금액, na.rm = T)),
    생활용품_지출_총금액  = map_dbl(data, ~ sum(.x$생활용품_지출_총금액, na.rm = T)),
    의료비_지출_총금액    = map_dbl(data, ~ sum(.x$의료비_지출_총금액, na.rm = T)),
    교통_지출_총금액      = map_dbl(data, ~ sum(.x$교통_지출_총금액, na.rm = T)),
    여가_지출_총금액      = map_dbl(data, ~ sum(.x$여가_지출_총금액, na.rm = T)),
    문화_지출_총금액      = map_dbl(data, ~ sum(.x$문화_지출_총금액, na.rm = T)),
    교육_지출_총금액      = map_dbl(data, ~ sum(.x$교육_지출_총금액, na.rm = T)),
    유흥_지출_총금액      = map_dbl(data, ~ sum(.x$유흥_지출_총금액, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | INCOME    - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_income <- left_join(df_income, 
                       df_area %>% select(상권_코드, cluster),
                       by = c('상권_코드'))


# 02 > 군집별 계산
df_income_cluster <- df_income %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    월_평균_소득_금액     = map_dbl(data, ~ mean(.x$월_평균_소득_금액, na.rm = T)),
    소득_구간_코드        = map_dbl(data, ~ mean(.x$소득_구간_코드, na.rm = T)),
    지출_총금액           = map_dbl(data, ~ sum(.x$지출_총금액, na.rm = T)),
    식료품_지출_총금액    = map_dbl(data, ~ sum(.x$식료품_지출_총금액, na.rm = T)),
    의류_신발_지출_총금액 = map_dbl(data, ~ sum(.x$의류_신발_지출_총금액, na.rm = T)),
    생활용품_지출_총금액  = map_dbl(data, ~ sum(.x$생활용품_지출_총금액, na.rm = T)),
    의료비_지출_총금액    = map_dbl(data, ~ sum(.x$의료비_지출_총금액, na.rm = T)),
    교통_지출_총금액      = map_dbl(data, ~ sum(.x$교통_지출_총금액, na.rm = T)),
    여가_지출_총금액      = map_dbl(data, ~ sum(.x$여가_지출_총금액, na.rm = T)),
    문화_지출_총금액      = map_dbl(data, ~ sum(.x$문화_지출_총금액, na.rm = T)),
    교육_지출_총금액      = map_dbl(data, ~ sum(.x$교육_지출_총금액, na.rm = T)),
    유흥_지출_총금액      = map_dbl(data, ~ sum(.x$유흥_지출_총금액, na.rm = T))
    
  ) %>% 
  select(-data)






## SETTING   | INDEX     - 상권별 변수 정리 -----
df_index_dist <- df_index %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    운영_영업_개월_평균 = map_dbl(data, ~ mean(.x$운영_영업_개월_평균, na.rm = T)),
    폐업_영업_개월_평균 = map_dbl(data, ~ mean(.x$폐업_영업_개월_평균, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | INDEX     - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_index <- left_join(df_index, 
                       df_area %>% select(상권_코드, cluster),
                       by = c('상권_코드'))


# 02 > 군집별 계산
df_index_cluster <- df_index %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    운영_영업_개월_평균 = map_dbl(data, ~ mean(.x$운영_영업_개월_평균, na.rm = T)),
    폐업_영업_개월_평균 = map_dbl(data, ~ mean(.x$폐업_영업_개월_평균, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | RESIDENT  - 상권별 변수 정리 -----

# 01 > 분기별-상권별 분석
df_resident_dist <- df_resident %>% 
  nest(data = -c(기준_년분기_코드, 상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    총_상주인구_수                 = map_dbl(data, ~ sum(.x$총_상주인구_수, na.rm = T)),
    남성_상주인구_수               = map_dbl(data, ~ sum(.x$남성_상주인구_수, na.rm = T)),
    여성_상주인구_수               = map_dbl(data, ~ sum(.x$여성_상주인구_수, na.rm = T)),
    연령대_10_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_10_상주인구_수, na.rm = T)),
    연령대_20_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_20_상주인구_수, na.rm = T)),
    연령대_30_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_30_상주인구_수, na.rm = T)),
    연령대_40_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_40_상주인구_수, na.rm = T)),
    연령대_50_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_50_상주인구_수, na.rm = T)),
    연령대_60_이상_상주인구_수     = map_dbl(data, ~ sum(.x$연령대_60_이상_상주인구_수, na.rm = T)),
    남성연령대_10_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_10_상주인구_수, na.rm = T)),
    남성연령대_20_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_20_상주인구_수, na.rm = T)),
    남성연령대_30_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_30_상주인구_수, na.rm = T)),
    남성연령대_40_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_40_상주인구_수, na.rm = T)),
    남성연령대_50_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_50_상주인구_수, na.rm = T)),
    남성연령대_60_이상_상주인구_수 = map_dbl(data, ~ sum(.x$남성연령대_60_이상_상주인구_수, na.rm = T)),
    여성연령대_10_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_10_상주인구_수, na.rm = T)),
    여성연령대_20_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_20_상주인구_수, na.rm = T)),
    여성연령대_30_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_30_상주인구_수, na.rm = T)),
    여성연령대_40_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_40_상주인구_수, na.rm = T)),
    여성연령대_50_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_50_상주인구_수, na.rm = T)),
    여성연령대_60_이상_상주인구_수 = map_dbl(data, ~ sum(.x$여성연령대_60_이상_상주인구_수, na.rm = T)),
    총_가구_수                     = map_dbl(data, ~ sum(.x$총_가구_수, na.rm = T)),
    아파트_가구_수                 = map_dbl(data, ~ sum(.x$아파트_가구_수, na.rm = T)),
    비_아파트_가구_수              = map_dbl(data, ~ sum(.x$비_아파트_가구_수, na.rm = T))
    
  ) %>% 
  select(-data)


# 02 > 상권별 분석
df_resident_dist <- df_resident_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    총_상주인구_수                 = map_dbl(data, ~ mean(.x$총_상주인구_수, na.rm = T)),
    남성_상주인구_수               = map_dbl(data, ~ mean(.x$남성_상주인구_수, na.rm = T)),
    여성_상주인구_수               = map_dbl(data, ~ mean(.x$여성_상주인구_수, na.rm = T)),
    연령대_10_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_10_상주인구_수, na.rm = T)),
    연령대_20_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_20_상주인구_수, na.rm = T)),
    연령대_30_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_30_상주인구_수, na.rm = T)),
    연령대_40_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_40_상주인구_수, na.rm = T)),
    연령대_50_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_50_상주인구_수, na.rm = T)),
    연령대_60_이상_상주인구_수     = map_dbl(data, ~ mean(.x$연령대_60_이상_상주인구_수, na.rm = T)),
    남성연령대_10_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_10_상주인구_수, na.rm = T)),
    남성연령대_20_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_20_상주인구_수, na.rm = T)),
    남성연령대_30_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_30_상주인구_수, na.rm = T)),
    남성연령대_40_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_40_상주인구_수, na.rm = T)),
    남성연령대_50_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_50_상주인구_수, na.rm = T)),
    남성연령대_60_이상_상주인구_수 = map_dbl(data, ~ mean(.x$남성연령대_60_이상_상주인구_수, na.rm = T)),
    여성연령대_10_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_10_상주인구_수, na.rm = T)),
    여성연령대_20_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_20_상주인구_수, na.rm = T)),
    여성연령대_30_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_30_상주인구_수, na.rm = T)),
    여성연령대_40_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_40_상주인구_수, na.rm = T)),
    여성연령대_50_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_50_상주인구_수, na.rm = T)),
    여성연령대_60_이상_상주인구_수 = map_dbl(data, ~ mean(.x$여성연령대_60_이상_상주인구_수, na.rm = T)),
    총_가구_수                     = map_dbl(data, ~ mean(.x$총_가구_수, na.rm = T)),
    아파트_가구_수                 = map_dbl(data, ~ mean(.x$아파트_가구_수, na.rm = T)),
    비_아파트_가구_수              = map_dbl(data, ~ mean(.x$비_아파트_가구_수, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | RESIDENT  - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_resident <- left_join(df_resident, 
                      df_area %>% select(상권_코드, cluster),
                      by = c('상권_코드'))


# 02 > 분기별-군집별 계산
df_resident_cluster <- df_resident %>% 
  nest(data = -c(기준_년분기_코드, cluster)) %>% 
  mutate(
    
    총_상주인구_수                 = map_dbl(data, ~ sum(.x$총_상주인구_수, na.rm = T)),
    남성_상주인구_수               = map_dbl(data, ~ sum(.x$남성_상주인구_수, na.rm = T)),
    여성_상주인구_수               = map_dbl(data, ~ sum(.x$여성_상주인구_수, na.rm = T)),
    연령대_10_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_10_상주인구_수, na.rm = T)),
    연령대_20_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_20_상주인구_수, na.rm = T)),
    연령대_30_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_30_상주인구_수, na.rm = T)),
    연령대_40_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_40_상주인구_수, na.rm = T)),
    연령대_50_상주인구_수          = map_dbl(data, ~ sum(.x$연령대_50_상주인구_수, na.rm = T)),
    연령대_60_이상_상주인구_수     = map_dbl(data, ~ sum(.x$연령대_60_이상_상주인구_수, na.rm = T)),
    남성연령대_10_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_10_상주인구_수, na.rm = T)),
    남성연령대_20_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_20_상주인구_수, na.rm = T)),
    남성연령대_30_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_30_상주인구_수, na.rm = T)),
    남성연령대_40_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_40_상주인구_수, na.rm = T)),
    남성연령대_50_상주인구_수      = map_dbl(data, ~ sum(.x$남성연령대_50_상주인구_수, na.rm = T)),
    남성연령대_60_이상_상주인구_수 = map_dbl(data, ~ sum(.x$남성연령대_60_이상_상주인구_수, na.rm = T)),
    여성연령대_10_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_10_상주인구_수, na.rm = T)),
    여성연령대_20_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_20_상주인구_수, na.rm = T)),
    여성연령대_30_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_30_상주인구_수, na.rm = T)),
    여성연령대_40_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_40_상주인구_수, na.rm = T)),
    여성연령대_50_상주인구_수      = map_dbl(data, ~ sum(.x$여성연령대_50_상주인구_수, na.rm = T)),
    여성연령대_60_이상_상주인구_수 = map_dbl(data, ~ sum(.x$여성연령대_60_이상_상주인구_수, na.rm = T)),
    총_가구_수                     = map_dbl(data, ~ sum(.x$총_가구_수, na.rm = T)),
    아파트_가구_수                 = map_dbl(data, ~ sum(.x$아파트_가구_수, na.rm = T)),
    비_아파트_가구_수              = map_dbl(data, ~ sum(.x$비_아파트_가구_수, na.rm = T))
    
  ) %>% 
  select(-data)


# 03 > 군집별 계산
df_resident_cluster <- df_resident_cluster %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    총_상주인구_수                 = map_dbl(data, ~ mean(.x$총_상주인구_수, na.rm = T)),
    남성_상주인구_수               = map_dbl(data, ~ mean(.x$남성_상주인구_수, na.rm = T)),
    여성_상주인구_수               = map_dbl(data, ~ mean(.x$여성_상주인구_수, na.rm = T)),
    연령대_10_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_10_상주인구_수, na.rm = T)),
    연령대_20_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_20_상주인구_수, na.rm = T)),
    연령대_30_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_30_상주인구_수, na.rm = T)),
    연령대_40_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_40_상주인구_수, na.rm = T)),
    연령대_50_상주인구_수          = map_dbl(data, ~ mean(.x$연령대_50_상주인구_수, na.rm = T)),
    연령대_60_이상_상주인구_수     = map_dbl(data, ~ mean(.x$연령대_60_이상_상주인구_수, na.rm = T)),
    남성연령대_10_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_10_상주인구_수, na.rm = T)),
    남성연령대_20_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_20_상주인구_수, na.rm = T)),
    남성연령대_30_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_30_상주인구_수, na.rm = T)),
    남성연령대_40_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_40_상주인구_수, na.rm = T)),
    남성연령대_50_상주인구_수      = map_dbl(data, ~ mean(.x$남성연령대_50_상주인구_수, na.rm = T)),
    남성연령대_60_이상_상주인구_수 = map_dbl(data, ~ mean(.x$남성연령대_60_이상_상주인구_수, na.rm = T)),
    여성연령대_10_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_10_상주인구_수, na.rm = T)),
    여성연령대_20_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_20_상주인구_수, na.rm = T)),
    여성연령대_30_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_30_상주인구_수, na.rm = T)),
    여성연령대_40_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_40_상주인구_수, na.rm = T)),
    여성연령대_50_상주인구_수      = map_dbl(data, ~ mean(.x$여성연령대_50_상주인구_수, na.rm = T)),
    여성연령대_60_이상_상주인구_수 = map_dbl(data, ~ mean(.x$여성연령대_60_이상_상주인구_수, na.rm = T)),
    총_가구_수                     = map_dbl(data, ~ mean(.x$총_가구_수, na.rm = T)),
    아파트_가구_수                 = map_dbl(data, ~ mean(.x$아파트_가구_수, na.rm = T)),
    비_아파트_가구_수              = map_dbl(data, ~ mean(.x$비_아파트_가구_수, na.rm = T))
    
  ) %>% 
  select(-data)





## SETTING   | WORKER    - 상권별 변수 정리 -----

# 01 > 분기별-상권별 분석
df_worker_dist <- df_worker %>% 
  nest(data = -c(기준_년분기_코드, 상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    총_직장_인구_수                 = map_dbl(data, ~ sum(.x$총_직장_인구_수, na.rm = T)),
    남성_직장_인구_수               = map_dbl(data, ~ sum(.x$남성_직장_인구_수, na.rm = T)),
    여성_직장_인구_수               = map_dbl(data, ~ sum(.x$여성_직장_인구_수, na.rm = T)),
    연령대_10_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_10_직장_인구_수, na.rm = T)),
    연령대_20_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_20_직장_인구_수, na.rm = T)),
    연령대_30_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_30_직장_인구_수, na.rm = T)),
    연령대_40_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_40_직장_인구_수, na.rm = T)),
    연령대_50_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_50_직장_인구_수, na.rm = T)),
    연령대_60_이상_직장_인구_수     = map_dbl(data, ~ sum(.x$연령대_60_이상_직장_인구_수, na.rm = T)),
    남성연령대_10_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_10_직장_인구_수, na.rm = T)),
    남성연령대_20_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_20_직장_인구_수, na.rm = T)),
    남성연령대_30_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_30_직장_인구_수, na.rm = T)),
    남성연령대_40_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_40_직장_인구_수, na.rm = T)),
    남성연령대_50_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_50_직장_인구_수, na.rm = T)),
    남성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ sum(.x$남성연령대_60_이상_직장_인구_수, na.rm = T)),
    여성연령대_10_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_10_직장_인구_수, na.rm = T)),
    여성연령대_20_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_20_직장_인구_수, na.rm = T)),
    여성연령대_30_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_30_직장_인구_수, na.rm = T)),
    여성연령대_40_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_40_직장_인구_수, na.rm = T)),
    여성연령대_50_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_50_직장_인구_수, na.rm = T)),
    여성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ sum(.x$여성연령대_60_이상_직장_인구_수, na.rm = T))
    
    
  ) %>% 
  select(-data)


# 02 > 분기별-상권별 분석
df_worker_dist <- df_worker_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    총_직장_인구_수                 = map_dbl(data, ~ mean(.x$총_직장_인구_수, na.rm = T)),
    남성_직장_인구_수               = map_dbl(data, ~ mean(.x$남성_직장_인구_수, na.rm = T)),
    여성_직장_인구_수               = map_dbl(data, ~ mean(.x$여성_직장_인구_수, na.rm = T)),
    연령대_10_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_10_직장_인구_수, na.rm = T)),
    연령대_20_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_20_직장_인구_수, na.rm = T)),
    연령대_30_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_30_직장_인구_수, na.rm = T)),
    연령대_40_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_40_직장_인구_수, na.rm = T)),
    연령대_50_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_50_직장_인구_수, na.rm = T)),
    연령대_60_이상_직장_인구_수     = map_dbl(data, ~ mean(.x$연령대_60_이상_직장_인구_수, na.rm = T)),
    남성연령대_10_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_10_직장_인구_수, na.rm = T)),
    남성연령대_20_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_20_직장_인구_수, na.rm = T)),
    남성연령대_30_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_30_직장_인구_수, na.rm = T)),
    남성연령대_40_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_40_직장_인구_수, na.rm = T)),
    남성연령대_50_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_50_직장_인구_수, na.rm = T)),
    남성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ mean(.x$남성연령대_60_이상_직장_인구_수, na.rm = T)),
    여성연령대_10_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_10_직장_인구_수, na.rm = T)),
    여성연령대_20_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_20_직장_인구_수, na.rm = T)),
    여성연령대_30_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_30_직장_인구_수, na.rm = T)),
    여성연령대_40_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_40_직장_인구_수, na.rm = T)),
    여성연령대_50_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_50_직장_인구_수, na.rm = T)),
    여성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ mean(.x$여성연령대_60_이상_직장_인구_수, na.rm = T))
    
    
  ) %>% 
  select(-data)





## SETTING   | WORKER    - 군집별 변수 정리 -----

# 01 > 군집변수 추가
df_worker <- left_join(df_worker, 
                       df_area %>% select(상권_코드, cluster),
                       by = c('상권_코드'))


# 02 > 분기별-군집별 계산
df_worker_cluster <- df_worker %>% 
  nest(data = -c(기준_년분기_코드, cluster)) %>% 
  mutate(
    
    총_직장_인구_수                 = map_dbl(data, ~ sum(.x$총_직장_인구_수, na.rm = T)),
    남성_직장_인구_수               = map_dbl(data, ~ sum(.x$남성_직장_인구_수, na.rm = T)),
    여성_직장_인구_수               = map_dbl(data, ~ sum(.x$여성_직장_인구_수, na.rm = T)),
    연령대_10_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_10_직장_인구_수, na.rm = T)),
    연령대_20_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_20_직장_인구_수, na.rm = T)),
    연령대_30_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_30_직장_인구_수, na.rm = T)),
    연령대_40_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_40_직장_인구_수, na.rm = T)),
    연령대_50_직장_인구_수          = map_dbl(data, ~ sum(.x$연령대_50_직장_인구_수, na.rm = T)),
    연령대_60_이상_직장_인구_수     = map_dbl(data, ~ sum(.x$연령대_60_이상_직장_인구_수, na.rm = T)),
    남성연령대_10_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_10_직장_인구_수, na.rm = T)),
    남성연령대_20_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_20_직장_인구_수, na.rm = T)),
    남성연령대_30_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_30_직장_인구_수, na.rm = T)),
    남성연령대_40_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_40_직장_인구_수, na.rm = T)),
    남성연령대_50_직장_인구_수      = map_dbl(data, ~ sum(.x$남성연령대_50_직장_인구_수, na.rm = T)),
    남성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ sum(.x$남성연령대_60_이상_직장_인구_수, na.rm = T)),
    여성연령대_10_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_10_직장_인구_수, na.rm = T)),
    여성연령대_20_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_20_직장_인구_수, na.rm = T)),
    여성연령대_30_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_30_직장_인구_수, na.rm = T)),
    여성연령대_40_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_40_직장_인구_수, na.rm = T)),
    여성연령대_50_직장_인구_수      = map_dbl(data, ~ sum(.x$여성연령대_50_직장_인구_수, na.rm = T)),
    여성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ sum(.x$여성연령대_60_이상_직장_인구_수, na.rm = T))
    
  ) %>% 
  select(-data)


# 03 > 군집별 계산
df_worker_cluster <- df_worker_cluster %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    총_직장_인구_수                 = map_dbl(data, ~ mean(.x$총_직장_인구_수, na.rm = T)),
    남성_직장_인구_수               = map_dbl(data, ~ mean(.x$남성_직장_인구_수, na.rm = T)),
    여성_직장_인구_수               = map_dbl(data, ~ mean(.x$여성_직장_인구_수, na.rm = T)),
    연령대_10_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_10_직장_인구_수, na.rm = T)),
    연령대_20_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_20_직장_인구_수, na.rm = T)),
    연령대_30_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_30_직장_인구_수, na.rm = T)),
    연령대_40_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_40_직장_인구_수, na.rm = T)),
    연령대_50_직장_인구_수          = map_dbl(data, ~ mean(.x$연령대_50_직장_인구_수, na.rm = T)),
    연령대_60_이상_직장_인구_수     = map_dbl(data, ~ mean(.x$연령대_60_이상_직장_인구_수, na.rm = T)),
    남성연령대_10_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_10_직장_인구_수, na.rm = T)),
    남성연령대_20_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_20_직장_인구_수, na.rm = T)),
    남성연령대_30_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_30_직장_인구_수, na.rm = T)),
    남성연령대_40_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_40_직장_인구_수, na.rm = T)),
    남성연령대_50_직장_인구_수      = map_dbl(data, ~ mean(.x$남성연령대_50_직장_인구_수, na.rm = T)),
    남성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ mean(.x$남성연령대_60_이상_직장_인구_수, na.rm = T)),
    여성연령대_10_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_10_직장_인구_수, na.rm = T)),
    여성연령대_20_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_20_직장_인구_수, na.rm = T)),
    여성연령대_30_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_30_직장_인구_수, na.rm = T)),
    여성연령대_40_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_40_직장_인구_수, na.rm = T)),
    여성연령대_50_직장_인구_수      = map_dbl(data, ~ mean(.x$여성연령대_50_직장_인구_수, na.rm = T)),
    여성연령대_60_이상_직장_인구_수 = map_dbl(data, ~ mean(.x$여성연령대_60_이상_직장_인구_수, na.rm = T))
    
  ) %>% 
  select(-data)






## SETTING   | DIVERSITY - 상권별 변수 정리 -----

# 01 > 분기 - 서울 전체 점포수, 서울 해당 업종 점포 수 계산
df_diversity_seoul <- df_store %>% 
  nest(data = -c(기준_년분기_코드, 서비스_업종_코드)) %>% 
  mutate(
    
    서울_해당_업종_점포_수 = map_dbl(data, ~ sum(.x$점포_수, na.rm = T)),
    서울_전체_업종_점포_수 = sum(서울_해당_업종_점포_수),
    
  ) %>% 
  select(-data)



# 02 > 연간 - 서울 전체 점포수, 서울 해당 업종 점포 수 계산
df_diversity_seoul <- df_diversity_seoul %>% 
  nest(data = -c(서비스_업종_코드)) %>% 
  mutate(
    
    서울_해당_업종_점포_수 = map_dbl(data, ~ mean(.x$서울_해당_업종_점포_수, na.rm = T)),
    서울_전체_업종_점포_수 = sum(서울_해당_업종_점포_수),
    
  ) %>% 
  select(-data)



# 03 > 분기 - 상권 전체 점포수, 상권 해당 업종 점포 수 계산
df_diversity_dist <- df_store %>% 
  nest(data = -c(기준_년분기_코드, 상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명, 서비스_업종_코드, 서비스_업종_코드_명)) %>% 
  mutate(
    
    상권_해당_업종_점포_수 = map_dbl(data, ~ sum(.x$점포_수, na.rm = T)),
    상권_전체_업종_점포_수 = sum(상권_해당_업종_점포_수),
    
  ) %>% 
  select(-data)


# 04 > 연간 - 상권 전체 점포수, 상권 해당 업종 점포 수 계산
df_diversity_dist <- df_diversity_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명, 서비스_업종_코드, 서비스_업종_코드_명)) %>% 
  mutate(
    
    상권_해당_업종_점포_수 = map_dbl(data, ~ mean(.x$상권_해당_업종_점포_수, na.rm = T)),
    상권_전체_업종_점포_수 = sum(상권_해당_업종_점포_수),
    
  ) %>% 
  select(-data)


# 05 > 연간 파일 통합 및 다양성지수 계산
df_diversity_dist <- left_join(df_diversity_dist, df_diversity_seoul)

df_diversity_dist <- df_diversity_dist %>% 
  mutate(
    
    서울_해당_업종_비율 = 서울_해당_업종_점포_수 / 서울_전체_업종_점포_수,
    상권_해당_업종_비율 = 상권_해당_업종_점포_수 / 상권_전체_업종_점포_수,
    비율차이            = abs(상권_해당_업종_비율 - 서울_해당_업종_비율),
    
  )

df_diversity_dist <- df_diversity_dist %>% 
  nest(data = -c(상권_구분_코드, 상권_구분_코드_명, 상권_코드, 상권_코드_명)) %>% 
  mutate(
    
    다양성지수 = map_dbl(data, ~ 1 / sum(.x$비율차이, na.rm = T))
    
  ) %>% 
  select(-data)

rm(df_diversity_seoul)




## SETTING   | DIVERSITY - 군집별 변수 정리 -----

# 01 > 분기 - 서울 전체 점포수, 서울 해당 업종 점포 수 계산
df_diversity_seoul <- df_store %>% 
  nest(data = -c(기준_년분기_코드, 서비스_업종_코드)) %>% 
  mutate(
    
    서울_해당_업종_점포_수 = map_dbl(data, ~ sum(.x$점포_수, na.rm = T)),
    서울_전체_업종_점포_수 = sum(서울_해당_업종_점포_수),
    
  ) %>% 
  select(-data)



# 02 > 연간 - 서울 전체 점포수, 서울 해당 업종 점포 수 계산
df_diversity_seoul <- df_diversity_seoul %>% 
  nest(data = -c(서비스_업종_코드)) %>% 
  mutate(
    
    서울_해당_업종_점포_수 = map_dbl(data, ~ mean(.x$서울_해당_업종_점포_수, na.rm = T)),
    서울_전체_업종_점포_수 = sum(서울_해당_업종_점포_수),
    
  ) %>% 
  select(-data)



# 03 > 분기 - 군집 전체 점포수, 군집 해당 업종 점포 수 계산
df_diversity_cluster <- df_store %>% 
  nest(data = -c(기준_년분기_코드, cluster, 서비스_업종_코드, 서비스_업종_코드_명)) %>% 
  mutate(
    
    상권_해당_업종_점포_수 = map_dbl(data, ~ sum(.x$점포_수, na.rm = T)),
    상권_전체_업종_점포_수 = sum(상권_해당_업종_점포_수),
    
  ) %>% 
  select(-data)


# 04 > 연간 - 상권 전체 점포수, 상권 해당 업종 점포 수 계산
df_diversity_cluster <- df_diversity_cluster %>% 
  nest(data = -c(cluster, 서비스_업종_코드, 서비스_업종_코드_명)) %>% 
  mutate(
    
    상권_해당_업종_점포_수 = map_dbl(data, ~ mean(.x$상권_해당_업종_점포_수, na.rm = T)),
    상권_전체_업종_점포_수 = sum(상권_해당_업종_점포_수),
    
  ) %>% 
  select(-data)


# 05 > 연간 파일 통합 및 다양성지수 계산
df_diversity_cluster <- left_join(df_diversity_cluster, df_diversity_seoul)

df_diversity_cluster <- df_diversity_cluster %>% 
  mutate(
    
    서울_해당_업종_비율 = 서울_해당_업종_점포_수 / 서울_전체_업종_점포_수,
    상권_해당_업종_비율 = 상권_해당_업종_점포_수 / 상권_전체_업종_점포_수,
    비율차이            = abs(상권_해당_업종_비율 - 서울_해당_업종_비율),
    
  )

df_diversity_cluster <- df_diversity_cluster %>% 
  nest(data = -c(cluster)) %>% 
  mutate(
    
    다양성지수 = map_dbl(data, ~ 1 / sum(.x$비율차이, na.rm = T))
    
  ) %>% 
  select(-data)

rm(df_diversity_seoul)




## SETTING   | RENT      - 상권별 변수 정리 -----

# 01 > 분기-구별 임대료 정리
df_rent_dist <- df_rent %>% 
  select(3:15) %>% 
  melt(id.vars = '자치구_코드_명')
  
colnames(df_rent_dist) <- c('자치구_코드_명', '기준_년분기_코드', '임대료')

df_rent_dist <- df_rent_dist %>% 
  group_by(자치구_코드_명, 기준_년분기_코드) %>% 
  summarise(
    
    임대료 = mean(임대료, na.rm = T)
    
  ) %>% 
  as.data.frame()

df_rent_dist$임대료[is.nan(df_rent_dist$임대료) == T] <- NA


# 02 > 연간-구별 임대료 정리
df_rent_dist <- df_rent_dist %>% 
  filter(substr(기준_년분기_코드, 1, 4) == '2022') %>% 
  group_by(자치구_코드_명) %>% 
  summarise(
    
    임대료 = mean(임대료, na.rm = T)
    
  )
  



## SETTING   | RENT      - 군집별 변수 정리 -----

# 01 > 군집 정보 추가
df_rent_cluster <- df_area %>% 
  select(cluster, 자치구_코드_명) %>% 
  unique() %>% 
  as.data.frame()

df_rent_cluster <- left_join(df_rent_cluster, df_rent_dist, by = '자치구_코드_명')


# 02 > 연간-군집별 임대료 정리
df_rent_cluster <- df_rent_cluster %>% 
  group_by(cluster) %>% 
  summarise(
    
    임대료 = mean(임대료, na.rm = T)
    
  ) %>% 
  as.data.frame()




## SETTING   | INSTAR    - 군집별 변수 정리 -----

# 01 > 언급량 정리
df_instar_cluster <- df_instar %>% 
  select(1, 6, 7)

colnames(df_instar_cluster) <- c('cluster_name', '인스타언급량_연간', '인스타언급량_1분기대비4분기증가율')
colnames(df_cluster_name)   <- c('cluster_name', 'cluster')

df_instar_cluster <- left_join(df_instar_cluster, df_cluster_name)




## SETTING   | 군집별 통합 파일 생성 -----

# 01 > 통합 파일 생성
df_total_cluster <- left_join(df_sales_cluster, df_store_cluster)
df_total_cluster <- left_join(df_total_cluster, df_apt_cluster)
df_total_cluster <- left_join(df_total_cluster, df_diversity_cluster)
df_total_cluster <- left_join(df_total_cluster, df_fercility_cluster)
df_total_cluster <- left_join(df_total_cluster, df_float_cluster)
df_total_cluster <- left_join(df_total_cluster, df_income_cluster)
df_total_cluster <- left_join(df_total_cluster, df_index_cluster)
df_total_cluster <- left_join(df_total_cluster, df_instar_cluster)
df_total_cluster <- left_join(df_total_cluster, df_rent_cluster)
df_total_cluster <- left_join(df_total_cluster, df_resident_cluster)
df_total_cluster <- left_join(df_total_cluster, df_worker_cluster)



# 02 > 통합 표준화(z-score) 파일 생성

# 표준화 함수 정의
z_score_normalize <- function(x){ (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) }


# 표준화 파일 생성
df_total_cluster_z <- as.data.frame(sapply(df_total_cluster,
                                           function(x) { if(is.numeric(x)) z_score_normalize(x) else x}))
  
  





## ANALYSIS  | 군집별 상관분석 -----

# 01 > 정규화 이전 데이터 
result_corr <- rcorr(as.matrix(df_total_cluster %>% select(-cluster_name)))

wb <- createWorkbook()
addWorksheet(wb, 'r')
addWorksheet(wb, 'p')
writeData(wb, 'r', corr_result$r)
writeData(wb, 'p', corr_result$P)

saveWorkbook(wb, 'output/corr_result_raw.xlsx')


# 02 > 정규화 이후 데이터
for (i in 1:178){
  
  df_total_cluster_z[[i]] <- as.numeric(df_total_cluster_z[[i]])
  
}

result_corr <- rcorr(as.matrix(df_total_cluster_z %>% select(-cluster_name)))

wb <- createWorkbook()
addWorksheet(wb, 'r')
addWorksheet(wb, 'p')
writeData(wb, 'r', corr_result$r)
writeData(wb, 'p', corr_result$P)

saveWorkbook(wb, 'output/corr_result_z.xlsx')





## ANALYSIS  | 차원축소 시도 -----

# 01 > apt
result_pca_apt <- prcomp(df_apt_cluster %>% select(-cluster), scale = T)
result_eigenvalues_apt <- result_pca_apt$sdev^2
print(result_eigenvalues_apt)


# 02 > fercility
result_pca_fercility <- prcomp(df_fercility_cluster %>% select(-cluster))
result_eigenvalues_fercility <- result_pca_fercility$sdev^2
print(result_eigenvalues_fercility)


# 03 > float
result_pca_float <- prcomp(df_float_cluster %>% select(-cluster), scale = T)
result_eigenvalues_float <- result_pca_float$sdev^2
print(result_eigenvalues_float)





## SETTING   | 군집별 통합 파일 변수 선택 및 추가 -----

# 01 > 비율 변수 계산
df_total_cluster_clear <- df_total_cluster %>% 
  mutate(
    
    연령대_10.30_매출_금액  = 연령대_10_매출_금액 + 연령대_20_매출_금액 + 연령대_30_매출_금액,
    점포_당_10.30_매출_금액 = 연령대_10.30_매출_금액 / 점포_수,
    
    남성_유동인구_비율           = 남성_유동인구_수           / 총_유동인구_수,
    여성_유동인구_비율           = 여성_유동인구_수           / 총_유동인구_수,
    연령대_10_유동인구_비율      = 연령대_10_유동인구_수      / 총_유동인구_수,
    연령대_20_유동인구_비율      = 연령대_20_유동인구_수      / 총_유동인구_수,
    연령대_30_유동인구_비율      = 연령대_30_유동인구_수      / 총_유동인구_수,
    연령대_40_유동인구_비율      = 연령대_40_유동인구_수      / 총_유동인구_수,
    연령대_50_유동인구_비율      = 연령대_50_유동인구_수      / 총_유동인구_수,
    연령대_60_이상_유동인구_비율 = 연령대_60_이상_유동인구_수 / 총_유동인구_수,
    월요일_유동인구_비율         = 월요일_유동인구_수         / 총_유동인구_수,
    화요일_유동인구_비율         = 화요일_유동인구_수         / 총_유동인구_수,
    수요일_유동인구_비율         = 수요일_유동인구_수         / 총_유동인구_수,
    목요일_유동인구_비율         = 목요일_유동인구_수         / 총_유동인구_수,
    금요일_유동인구_비율         = 금요일_유동인구_수         / 총_유동인구_수,
    토요일_유동인구_비율         = 토요일_유동인구_수         / 총_유동인구_수,
    일요일_유동인구_비율         = 일요일_유동인구_수         / 총_유동인구_수,
    연령대_10.30_유동인구_비율   = (연령대_10_유동인구_수 + 연령대_20_유동인구_수 + 연령대_30_유동인구_수)      / 총_유동인구_수,
    연령대_40.60_유동인구_비율   = (연령대_40_유동인구_수 + 연령대_50_유동인구_수 + 연령대_60_이상_유동인구_수) / 총_유동인구_수,
    주중_유동인구_비율           = (월요일_유동인구_수 + 화요일_유동인구_수 + 수요일_유동인구_수 + 목요일_유동인구_수 + 금요일_유동인구_수) / 총_유동인구_수,
    주말_유동인구_비율           = (토요일_유동인구_수 + 일요일_유동인구_수) / 총_유동인구_수,
    
    남성_상주인구_비율           = 남성_상주인구_수           / 총_상주인구_수,
    여성_상주인구_비율           = 여성_상주인구_수           / 총_상주인구_수,
    연령대_10_상주인구_비율      = 연령대_10_상주인구_수      / 총_상주인구_수,
    연령대_20_상주인구_비율      = 연령대_20_상주인구_수      / 총_상주인구_수,
    연령대_30_상주인구_비율      = 연령대_30_상주인구_수      / 총_상주인구_수,
    연령대_40_상주인구_비율      = 연령대_40_상주인구_수      / 총_상주인구_수,
    연령대_50_상주인구_비율      = 연령대_50_상주인구_수      / 총_상주인구_수,
    연령대_60_이상_상주인구_비율 = 연령대_60_이상_상주인구_수 / 총_상주인구_수,
    연령대_10.30_상주인구_비율   = (연령대_10_상주인구_수 + 연령대_20_상주인구_수 + 연령대_30_상주인구_수) / 총_상주인구_수,
    연령대_40.60_상주인구_비율   = (연령대_40_상주인구_수 + 연령대_50_상주인구_수 + 연령대_60_이상_상주인구_수) / 총_상주인구_수,
    
    
    남성_직장_인구_비율           = 남성_직장_인구_수           / 총_직장_인구_수,
    여성_직장_인구_비율           = 여성_직장_인구_수           / 총_직장_인구_수,
    연령대_10_직장_인구_비율      = 연령대_10_직장_인구_수      / 총_직장_인구_수,
    연령대_20_직장_인구_비율      = 연령대_20_직장_인구_수      / 총_직장_인구_수,
    연령대_30_직장_인구_비율      = 연령대_30_직장_인구_수      / 총_직장_인구_수,
    연령대_40_직장_인구_비율      = 연령대_40_직장_인구_수      / 총_직장_인구_수,
    연령대_50_직장_인구_비율      = 연령대_50_직장_인구_수      / 총_직장_인구_수,
    연령대_60_이상_직장_인구_비율 = 연령대_60_이상_직장_인구_수 / 총_직장_인구_수,
    연령대_10.30_직장_인구_비율   = (연령대_10_직장_인구_수 + 연령대_20_직장_인구_수 + 연령대_30_직장_인구_수)      / 총_직장_인구_수,
    연령대_40.60_직장_인구_비율   = (연령대_40_직장_인구_수 + 연령대_50_직장_인구_수 + 연령대_60_이상_직장_인구_수) / 총_직장_인구_수,
    
    식료품_지출_비율    = 식료품_지출_총금액/ 지출_총금액,
    의류_신발_지출_비율 = 의류_신발_지출_총금액/ 지출_총금액,
    생활용품_지출_비율  = 생활용품_지출_총금액/ 지출_총금액,
    의료비_지출_비율    = 의료비_지출_총금액/ 지출_총금액,
    교통_지출_비율      = 교통_지출_총금액/ 지출_총금액,
    여가_지출_비율      = 여가_지출_총금액/ 지출_총금액,
    문화_지출_비율      = 문화_지출_총금액/ 지출_총금액,
    교육_지출_비율      = 교육_지출_총금액/ 지출_총금액,
    유흥_지출_비율      = 유흥_지출_총금액/ 지출_총금액,

  )
  
# 02 > 논문 및 자료 참고하여 변수 선택
df_total_cluster_clear <- df_total_cluster_clear %>% 
  select(
    
    # sales(종속변수)
    연령대_10.30_매출_금액, 점포_당_10.30_매출_금액,
    
    # apt
    아파트_단지_수, 아파트_평균_시가, 
    
    # diversity
    다양성지수,
    
    # fercility
    집객시설_수, 관공서_수, 은행_수, 종합병원_수, 일반_병원_수, 약국_수, 유치원_수, 초등학교_수,
    중학교_수, 고등학교_수, 대학교_수, 백화점_수, 슈퍼마켓_수, 극장_수, 숙박_시설_수, 공항_수,
    버스_터미널_수, 지하철_역_수, 버스_정거장_수,
    
    # float
    총_유동인구_수, 남성_유동인구_비율, 여성_유동인구_비율, 연령대_10_유동인구_비율, 연령대_20_유동인구_비율, 
    연령대_30_유동인구_비율, 연령대_40_유동인구_비율, 연령대_50_유동인구_비율, 연령대_60_이상_유동인구_비율,
    월요일_유동인구_비율, 화요일_유동인구_비율, 수요일_유동인구_비율, 목요일_유동인구_비율, 금요일_유동인구_비율, 
    토요일_유동인구_비율, 일요일_유동인구_비율, 주중_유동인구_비율, 주말_유동인구_비율,
    
    # income
    월_평균_소득_금액, 지출_총금액, 식료품_지출_비율, 의류_신발_지출_비율, 생활용품_지출_비율,
    의료비_지출_비율, 교통_지출_비율, 여가_지출_비율, 문화_지출_비율, 교육_지출_비율, 유흥_지출_비율,
    
    # index
    운영_영업_개월_평균, 폐업_영업_개월_평균,
    
    # instar
    인스타언급량_연간, 인스타언급량_1분기대비4분기증가율,
    
    # rent
    임대료,
    
    # resident
    총_상주인구_수, 남성_상주인구_비율, 여성_상주인구_비율, 연령대_10_상주인구_비율, 연령대_20_상주인구_비율, 
    연령대_30_상주인구_비율, 연령대_40_상주인구_비율, 연령대_50_상주인구_비율, 연령대_60_이상_상주인구_비율, 총_가구_수,
    
    # store
    점포_수, 유사_업종_점포_수, 개업_점포_수, 폐업_점포_수, 프랜차이즈_점포_수, 개업_율, 폐업_률,
    
    # worker
    총_직장_인구_수, 남성_직장_인구_비율, 여성_직장_인구_비율, 연령대_10_직장_인구_비율, 연령대_20_직장_인구_비율, 연령대_30_직장_인구_비율,
    연령대_40_직장_인구_비율, 연령대_50_직장_인구_비율, 연령대_60_이상_직장_인구_비율
    
  )



# 03 > 점수 정규화
df_total_cluster_clear_z <- as.data.frame(sapply(df_total_cluster_clear, 
                                                 function(x) { if(is.numeric(x)) z_score_normalize(x) else x }))






## ANALYSIS  | 군집별 회귀분석 -> 다중공선성으로 분석 안 됨 -----

# 01 > 단계적 회귀분석 실시
result_reg_z <- lm(점포_당_10.30_매출_금액 ~ ., 
                         df_total_cluster_clear_z %>% select(-연령대_10.30_매출_금액))

summary(result_reg_z)
print(step(result_reg_z, direction = 'both'))


# 02 > VIF 확인
vif(result_reg_z)


# 03 > all subset regression
result_reg_sub_numb_z <- regsubsets(점포_당_10.30_매출_금액 ~ ., 
                                  df_total_cluster_clear_numb_z %>% select(-연령대_10.30_매출_금액))





## ANALYSIS  | 주성분 분석을 통한 차원축소 탐색 --> 고유값 1 기준 17factor이지만, factor loding이 정상적 도출 X -----
result_pca_z <- prcomp(df_total_cluster_clear_z)
result_eigenvalues_z <- result_pca_z$sdev^2
print(result_eigenvalues_z)



## ANALYSIS  | 상관분석을 통한 변수 제거 탐색 -----
result_corr_z <- cor(df_total_cluster_clear_z) %>% as.data.frame()
write_xlsx(result_corr_z, 'output/result_corr_z.xlsx')



## ANALYSIS  | 변수 제거 및 회귀분석 실시(1) - 종속변수 = 점포당 매출 & 회귀계수 기준 변수 필터 -> 설명량 너무 적음 -----

# 00 > 선택 변수 지정
small_list_01 <- c('점포_당_10.30_매출_금액','관공서_수', '초등학교_수', '백화점_수', '극장_수',
                   '남성_유동인구_비율', '연령대_20_유동인구_비율', '연령대_30_유동인구_비율',
                   '주중_유동인구_비율', '월_평균_소득_금액', '여가_지출_비율', '교육_지출_비율',
                   '인스타언급량_1분기대비4분기증가율', '유사_업종_점포_수',
                   '연령대_20_직장_인구_비율', '연령대_40_직장_인구_비율')



# 01 > 단계적 회귀분석 실시
result_reg_z_small_01 <- lm(점포_당_10.30_매출_금액 ~ ., 
                            df_total_cluster_clear_z %>% select(small_list_01))

summary(result_reg_z_small_01)
print(step(result_reg_z_small_01, direction = 'both'))



# 02 > VIF 확인
vif(result_reg_z_small_01)



# 03 > all subset regression
result_reg_sub_z_01 <- regsubsets(점포_당_10.30_매출_금액 ~ ., 
                                  df_total_cluster_clear_z %>% select(small_list_01))

summary(result_reg_sub_z_01)




## ANALYSIS  | 변수 제거 및 회귀분석 실시(2) - 종속변수 = 점포당 매출 & 의미기준 변수 필터      -> 설명량 더 적음 -----

# 00 > 선택 변수 지정
small_list_02 <- c('점포_당_10.30_매출_금액', '관공서_수', '초등학교_수', '백화점_수',
                   '극장_수', '남성_유동인구_비율', '주중_유동인구_비율', '월_평균_소득_금액',
                   '생활용품_지출_비율', '여가_지출_비율', '교육_지출_비율', '유흥_지출_비율',
                   '인스타언급량_1분기대비4분기증가율', '유사_업종_점포_수', '연령대_20_직장_인구_비율',
                   '연령대_40_직장_인구_비율')



# 01 > 단계적 회귀분석 실시
result_reg_z_small_02 <- lm(점포_당_10.30_매출_금액 ~ ., 
                            df_total_cluster_clear_z %>% select(small_list_02))

summary(result_reg_z_small_02)
print(step(result_reg_z_small_02, direction = 'both'))



# 02 > VIF 확인
vif(result_reg_z_small_02)


# 03 > all subset regression
result_reg_sub_z_02 <- regsubsets(점포_당_10.30_매출_금액 ~ ., 
                                    df_total_cluster_clear_numb_z %>% select(-점포_당_10.30_매출_금액))




## ANALYSIS  | 변수 제거 및 회귀분석 실시(3) - 종속변수 = 10.30매출 & 회귀계수 기준 필터        -> 설명량 좋을 듯 -----

# 00 > 선택 변수 지정
small_list_03 <- c('연령대_10.30_매출_금액', '다양성지수', '관공서_수', '일반_병원_수',
                   '초등학교_수', '중학교_수', '고등학교_수', '대학교_수', '백화점_수',
                   '슈퍼마켓_수', '극장_수', '숙박_시설_수', '버스_정거장_수',
                   '남성_유동인구_비율', '연령대_20_유동인구_비율', '연령대_30_유동인구_비율',
                   '연령대_40_유동인구_비율', '주중_유동인구_비율', '월_평균_소득_금액',
                   '교통_지출_비율', '여가_지출_비율', '교육_지출_비율',
                   '인스타언급량_1분기대비4분기증가율', '임대료', '총_가구_수',
                   '유사_업종_점포_수', '폐업_률', '여성_직장_인구_비율', '연령대_10_직장_인구_비율',
                   '연령대_20_직장_인구_비율', '연령대_40_직장_인구_비율')


# 01 > 회귀분석 실시
result_reg_z_small_03 <- lm(연령대_10.30_매출_금액 ~ ., 
                            df_total_cluster_clear_z %>% select(small_list_03))

summary(result_reg_z_small_03)
print(step(result_reg_z_small_03, direction = 'both'))


# 02 > VIF 확인
vif(result_reg_z_small_03)


# 03 > vif 확인 후 10 넘는거 제외하고 계산
small_list_04 <- c('연령대_10.30_매출_금액', '다양성지수', '관공서_수', '일반_병원_수',
                   '초등학교_수', '중학교_수', '고등학교_수', '대학교_수', '백화점_수',
                   '슈퍼마켓_수', '극장_수', '숙박_시설_수', '버스_정거장_수',
                   '남성_유동인구_비율', '연령대_30_유동인구_비율',
                   '연령대_40_유동인구_비율', '주중_유동인구_비율', '월_평균_소득_금액',
                   '교통_지출_비율', '여가_지출_비율', '교육_지출_비율',
                   '인스타언급량_1분기대비4분기증가율', '임대료', '총_가구_수',
                   '유사_업종_점포_수', '폐업_률', '여성_직장_인구_비율', '연령대_10_직장_인구_비율',
                   '연령대_20_직장_인구_비율', '연령대_40_직장_인구_비율')

result_reg_z_small_04 <- lm(연령대_10.30_매출_금액 ~ ., 
                            df_total_cluster_clear_z %>% select(small_list_04))

summary(result_reg_z_small_04)

vif(result_reg_z_small_04)


# 04 > 변수 선택법 적용
print(step(result_reg_z_small_04, direction = 'both'))

result_reg_z_small_05 <- lm(formula = 연령대_10.30_매출_금액 ~ 초등학교_수 + 
                              고등학교_수 + 백화점_수 + 슈퍼마켓_수 + 극장_수 + 
                              숙박_시설_수 + 버스_정거장_수 + 남성_유동인구_비율 + 
                              연령대_30_유동인구_비율 + 연령대_40_유동인구_비율 + 
                              주중_유동인구_비율 + 월_평균_소득_금액 + 교통_지출_비율 + 
                              인스타언급량_1분기대비4분기증가율 + 임대료 + 
                              총_가구_수 + 유사_업종_점포_수 + 폐업_률 + 
                              연령대_10_직장_인구_비율 + 연령대_20_직장_인구_비율 + 
                              연령대_40_직장_인구_비율, data = df_total_cluster_clear_z %>% 
                              select(small_list_04))

summary(result_reg_z_small_05)


# 05 > all subset regression
result_reg_z_small_06 <- regsubsets(연령대_10.30_매출_금액 ~ ., 
                                  df_total_cluster_clear_z %>% select(small_list_04))

summary(result_reg_z_small_06)

plot(result_reg_z_small_06, scale="adjr2")



# 06 > all subset reg 결과로 도출된 독립변수만 투입
result_reg_z_small_07 <- lm(formula = 연령대_10.30_매출_금액 ~ 초등학교_수 + 
                              백화점_수 + 슈퍼마켓_수 +
                              연령대_30_유동인구_비율 + 연령대_40_유동인구_비율 + 
                              교통_지출_비율 + 
                              인스타언급량_1분기대비4분기증가율 + 유사_업종_점포_수 +
                              연령대_40_직장_인구_비율, 
                              data = df_total_cluster_clear_z)

summary(result_reg_z_small_07)

vif(result_reg_z_small_07)






## ANALYSIS  | 머신러닝(1) 회귀분석 -----

# 00 > 투입 변수 설정
small_list_05 <- c('연령대_10.30_매출_금액', '초등학교_수', 
                     '백화점_수', '슈퍼마켓_수', 
                     '연령대_30_유동인구_비율', '연령대_40_유동인구_비율', 
                     '교통_지출_비율', 
                     '인스타언급량_1분기대비4분기증가율', '유사_업종_점포_수', 
                     '연령대_40_직장_인구_비율')

# 01 > 시드 설정
set.seed(1)

# 02 > 폴드 구분
fold <- createFolds(df_total_cluster_clear_z %>% 
                      select(-점포_당_10.30_매출_금액) %>% 
                      select(연령대_10.30_매출_금액),
                    k = 5)

control <- trainControl(method = 'cv', number = 5)


# 03 > 모델 학습 및 평가
model_lm <- train(연령대_10.30_매출_금액 ~ .,
                  data = df_total_cluster_clear_z %>% select(small_list_05),
                  method = 'lm', 
                  trControl = control)

model_lm
summary(model_lm)




## ANALYSIS  | 머신러닝(2) Random Forest -----

# 01 > 모델 학습 및 평가
model_rf <- train(연령대_10.30_매출_금액 ~ .,
                  data = df_total_cluster_clear_z %>% select(small_list_05),
                  method = 'rf', 
                  trControl = control)

model_rf

print(varImp(model_rf))

result_importance_02 <- varImp(model_rf)

ggplot(result_importance_02, aes(x = reorder(Var1, Overall), y = Overall)) +
  geom_bar(stat = "identity") +
  xlab("Variables") + ylab("Importance") +
  coord_flip() + # x와 y축 바꾸기
  theme_minimal()
  




## ANALYSIS  | 머신러닝(3) XGboost -----

# 01 > 모델 학습 및 평가
model_xg <- train(연령대_10.30_매출_금액 ~ .,
                  data = df_total_cluster_clear_z %>% select(small_list_05),
                  method = 'xgbLinear', 
                  trControl = control)

model_xg

print(model_xg$results)


print(xgb.importance(model = model_xg$finalModel))
result_importance_02 <- xgb.importance(model = model_xg$finalModel)


ggplot(result_importance_02, aes(x = Gain, y = Feature)) +
  geom_bar(stat = "identity") +
  xlab("Gain") + ylab("Feature") +
  theme_minimal()









