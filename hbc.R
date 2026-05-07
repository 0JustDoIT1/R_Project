library(dplyr)
library(readr)
library(stringr)
library(tidyr)

getwd()
setwd("D:/Project/AI_class/R_Project_data")

################################################################################
############################# 인프라 ###########################################
################################################################################
infra_hospital <- read.csv("응급기관 수.csv", fileEncoding = "UTF-8")
infra_hospital <- infra_hospital %>% rename(infra_hospital="응급의료기관수")
str(infra_hospital)
head(infra_hospital)
colSums(is.na(infra_hospital))

infra_amb <- read.csv("구급차 수.csv", fileEncoding = "UTF-8")
infra_amb <- infra_amb %>% rename(infra_amb="구급차.수")
str(infra_amb)
head(infra_amb)
colSums(is.na(infra_amb))

infra_people <- read.csv("인구수.csv", fileEncoding = "UTF-8")
infra_people <- infra_people %>% rename(infra_people="총인구수")
str(infra_people)
head(infra_people)
colSums(is.na(infra_people))

infra_area <- read.csv("지역면적.csv", fileEncoding = "UTF-8")
infra_area <- infra_area %>% rename(infra_area="면적")
str(infra_area)
head(infra_area)
colSums(is.na(infra_area))

# 인구수, 면적 데이터 먼저 합치기
infra_base <- infra_people %>%
  left_join(infra_area, by = c("분류", "항목"))

# 응급기관 수, 구급차 수 합치기
infra_all <- infra_hospital %>%
  left_join(infra_amb, by = c("분류", "항목")) %>%
  left_join(infra_base, by = c("분류", "항목"))

# 인구 10만명당 + 면적(km²)당 비율 계산
infra_all <- infra_all %>%
  mutate(
    # 인구 10만명당
    infra_hospital_per100k = infra_hospital / infra_people * 100000,
    infra_amb_per100k      = infra_amb / infra_people * 100000,
    
    # 면적(km²)당
    infra_hospital_per_area = infra_hospital / infra_area,
    infra_amb_per_area      = infra_amb / infra_area
  )

# 비율 값만 남기기기
infra_all <- infra_all %>%
  dplyr::select(분류, 항목,
                infra_hospital_per100k, infra_amb_per100k,
                infra_hospital_per_area, infra_amb_per_area)

str(infra_all)
head(infra_all)
colSums(is.na(infra_all))

################################################################################
############################### 의료 ###########################################
################################################################################
ami_amb <- read.csv("119 구급차 이용률.csv", fileEncoding = "UTF-8")
ami_amb <- ami_amb %>% rename(ami_amb="비율")
str(ami_amb)
head(ami_amb)
colSums(is.na(ami_amb))

ami_heal <- read.csv("입원 치료 제공률.csv", fileEncoding = "UTF-8")
ami_heal <- ami_heal %>% rename(ami_heal="비율")
str(ami_heal)
head(ami_heal)
colSums(is.na(ami_heal))

ami_trans <- read.csv("전원율.csv", fileEncoding = "UTF-8")
ami_trans <- ami_trans %>% rename(ami_trans="비율")
str(ami_trans)
head(ami_trans)
colSums(is.na(ami_trans))

ami_dead <- read.csv("병원 내 사망률.csv", fileEncoding = "UTF-8")
ami_dead <- ami_dead %>% rename(ami_dead="비율")
str(ami_dead)
head(ami_dead)
colSums(is.na(ami_dead))

################################################################################
############################### 지역구조 #######################################
################################################################################
region_old <- read.csv("고령화률.csv", fileEncoding = "UTF-8")
region_old <- region_old %>% rename(region_old="비율")
str(region_old)
head(region_old)
colSums(is.na(region_old))

################################################################################
############################# 2차 전처리 #######################################
################################################################################
# 카테고리별로 나누어진 데이터 하나로 최종 합치기
final_data <- infra_all %>% 
  inner_join(ami_amb, by=c("분류","항목")) %>% 
  inner_join(ami_heal, by=c("분류","항목")) %>% 
  inner_join(ami_trans, by=c("분류","항목")) %>% 
  inner_join(ami_dead, by=c("분류","항목")) %>% 
  inner_join(region_old, by=c("분류", "항목"))
# 컬럼별 NA 개수 확인(join 지역별로 잘됐는지 확인)
colSums(is.na(final_data))

# 컬럼명 변경
final_data <- final_data %>% 
  rename(region="분류", district="항목")

# 수도권 변수 추가
final_data <- final_data %>%
  mutate(
    metro = ifelse(region %in% c("서울","경기","인천"), 1, 0),
  )

# 지역과 수도권은 범주형 데이터로 변환
final_data$region = as.factor(final_data$region)
final_data$metro = factor(final_data$metro, levels=c(0,1), labels=c("비수도권", "수도권"))

dim(final_data)
head(final_data)
summary(final_data)
str(final_data)

# 파일로 추출
write.csv(final_data, "final_data.csv", row.names = FALSE, fileEncoding = "CP949")

