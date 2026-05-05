# install.packages("dplyr")
# install.packages("readr")
# install.packages("stringr")
# install.packages("tidyr")
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

setwd("D:/Project/AI_class/R_Project")
getwd()

################################################################################
############################# 인프라 ###########################################
################################################################################
infra_amb <- read.csv("data/infra/119 구급차 및 배치된 의료인력 (시도별).csv")
infra_hos <- read.csv("data/infra/응급의료기관 및 응급의료시설 (시도별).csv")
infra_bed <- read.csv("data/infra/응급의료기관 병상 수 (시도별).csv")


################################################## 119 구급차 및 배치된 의료인력
str(infra_amb)
head(infra_amb, 10)
# 1차 전처리
infra_amb_clean <- infra_amb %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(y2024 = str_replace_all(y2024, ",", ""),
         y2024 = as.numeric(y2024)) %>%
  # 필요 데이터만 가져오기
  # 119 구급차 총 수 / 인구 십만 명당 119 구급차 수 / 1급 응급구조사 수
  filter(
    (분류 == "119 구급차" & 항목 %in% c("계", "인구 십만 명당 119 구급차 수")) |
      (분류 == "탑승인력" & 항목 == "1급 응급구조사")
  )
# 2차 전처리
# 해당 데이터를 지역을 기준으로 펼치기
infra_amb_final <- infra_amb_clean %>%
  mutate(var_name = case_when(
    분류 == "119 구급차" & 항목 == "계" ~ "amb_total",
    분류 == "119 구급차" & str_detect(항목, "인구") ~ "amb_per_100k",
    분류 == "탑승인력" & 항목 == "1급 응급구조사" ~ "emt_level1"
  )) %>%
  select(지역, var_name, y2024) %>%
  pivot_wider(
    names_from = var_name,
    values_from = y2024
  )

################################################### 응급의료기관 및 응급의료시설
str(infra_hos)
head(infra_hos, 10)
# 1차 전처리
infra_hos_final <- infra_hos %>% 
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(y2024 = str_replace_all(y2024, ",", ""),
         y2024 = as.numeric(y2024)) %>% 
  # 구분되어 있는 의료센터 다 합치기
  group_by(지역) %>% 
  summarise(
    hos_total = sum(y2024, na.rm = TRUE)
  )

######################################################################## 병상 수
str(infra_bed)
head(infra_bed, 10)
# 1차 전처리
infra_bed_final <- infra_bed %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(y2024 = str_replace_all(y2024, ",", ""),
         y2024 = as.numeric(y2024)) %>% 
  # 필요 데이터 선택 : 전체 병상 수
  filter(
    항목 == "전체"
  ) %>% 
  # 필요 컬럼만 선택
  transmute(지역, bed_total = y2024)

###########################
# 최종 인프라 데이터 합치기
###########################

# 일단 left_join을 통해서 지역별로 다 합쳐보기
infra_final <- infra_amb_final %>% 
  left_join(infra_hos_final, by="지역") %>% 
  left_join(infra_bed_final, by="지역") %>% 
  rename(region = 지역)
# 컬럼별 NA 개수 확인(join 지역별로 잘됐는지 확인)
colSums(is.na(infra_final))
head(infra_final)

################################################################################
############################### 의료 ###########################################
################################################################################
ami_react_time <- read.csv("data/medical/급성 심근경색 환자의 119 구급대 반응시간.csv")
ami_cure_time <- read.csv("data/medical/급성 심근경색 환자의 발병 후 입원 치료 제공 소요시간(응급의료기관 주소지 기준).csv")
ami_dead <- read.csv("data/medical/급성 심근경색 환자의 병원 내 사망률.csv")
medical_retrans <- read.csv("data/medical/병원 사유로 인한 재전원율.csv")
medical_emergency <- read.csv("data/medical/시도별 응급실 이용 (성별, 연령별).csv")
medical_stay_time <- read.csv("data/medical/응급실 재실시간 (시도별).csv")
medical_ktas <- read.csv("data/medical/최초 중증도 분류(KTAS) 결과 (시도별).csv")
####################################### 급성 심근경색 환자의 119 구급대 반응시간
str(ami_react_time)
head(ami_react_time, 10)
# 1차 전처리
ami_react_final <- ami_react_time %>%
  # 필요 컬럼만 선택
  select(지역, 항목, y2024) %>% 
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 필요한 데이터만 추출 : 구급대 반응시간(평균), 급성 심근경색 환자 수
  filter(항목 %in% c("구급대 반응시간(평균)", "급성 심근경색 환자 수")) %>%
  # 지역 기준으로 펼치기
  pivot_wider(
    names_from = 항목,
    values_from = y2024
  ) %>% 
  # 컬럼명 변경
  rename(
    ami_react_time = `구급대 반응시간(평균)`,
    ami_count = `급성 심근경색 환자 수`
  )

ami_react_final
# 급성 심근경색 환자의 발병 후 입원 치료 제공 소요시간(응급의료기관 주소지 기준)
str(ami_cure_time)
head(ami_cure_time, 10)
ami_cure_final <- ami_cure_time %>% 
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 필요한 데이터만 추출 : 발병 후 내원 소요시간, 내원 후 퇴실 소요시간간
  filter(
    (항목 == "평균" & 분류 %in% c("발병 후 내원 소요시간", "내원 후 퇴실 소요시간"))
  ) %>% 
  # 컬럼명 변경
  mutate(var_name = case_when(
    분류 == "발병 후 내원 소요시간" ~ "ami_cure_arrival",
    분류 == "내원 후 퇴실 소요시간" ~ "ami_cure_stay"
  )) %>%
  # 필요 컬럼만 선택
  select(지역, var_name, y2024) %>%
  # 지역 기준으로 long 데이터를 wide로 펼치기
  pivot_wider(
    names_from = var_name,
    values_from = y2024
  )
ami_cure_final

############################################ 급성 심근경색 환자의 병원 내 사망률
str(ami_dead)
head(ami_dead, 10)

ami_dead_final <- ami_dead %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 필요 데이터만 추출 : 사망률
  filter(str_detect(항목, "사망률")) %>%
  # 필요 컬럼만 추출
  transmute(
    지역,
    ami_death_rate = y2024
  )

ami_dead_final
###################################################### 병원 사유로 인한 재전원율
str(medical_retrans)
head(medical_retrans, 10)

medical_retrans_final <- medical_retrans %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 필요 데이터만 추출 : 재전원율
  filter(str_detect(항목, "재전원율")) %>%
  # 필요 컬럼만 추출
  transmute(
    지역,
    retrans_rate = y2024
  )

medical_retrans_final

############################################## 시도별 응급실 이용 (성별, 연령별)
str(medical_emergency)
head(medical_emergency, 10)

medical_emergency_final <- medical_emergency %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 지역을 기준으로 그룹화
  group_by(지역) %>%
  # 남녀 더해서 총 응급실 이용 수로 변환
  summarise(
    emergency_total = sum(y2024, na.rm = TRUE)
  )

medical_emergency_final
####################################################### 응급실 재실시간 (시도별)
str(medical_stay_time)
head(medical_stay_time, 10)

medical_stay_final <- medical_stay_time %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 필요 데이터만 추출 : 평균
  filter(항목.1 == "평균") %>%
  # 필요 컬럼만 추출
  transmute(
    지역,
    er_stay_time = y2024
  )

medical_stay_final
########################################### 최초 중증도 분류(KTAS) 결과 (시도별)
str(medical_ktas)
head(medical_ktas, 10)

medical_ktas_final <- medical_ktas %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    y2024 = str_replace_all(y2024, ",", ""),
    y2024 = as.numeric(y2024)
  ) %>%
  # 지역으로 그룹화
  group_by(지역) %>%
  # 전체 수, 중증(1-2), 경증(3-5) 값 합치기
  summarise(
    total = sum(y2024, na.rm = TRUE),
    severe = sum(y2024[항목 %in% c("레벨1", "레벨2")]),
    mild = sum(y2024[항목 %in% c("레벨3", "레벨4", "레벨5")])
  ) %>%
  # 중증/전체 수, 경증/전체 수 로 비율 계산
  mutate(
    severe_ratio = severe / total,
    mild_ratio = mild / total
  ) %>%
  # 필요 컬럼만 추출
  select(지역, severe_ratio, mild_ratio)

medical_ktas_final

#############################
# 최종 심근경색 데이터 합치기
#############################
# 일단 left_join을 통해서 지역별로 다 합쳐보기
ami_final <- ami_react_final %>% 
  left_join(ami_cure_final, by="지역") %>% 
  left_join(ami_dead_final, by="지역") %>% 
  rename(region = 지역)
# 컬럼별 NA 개수 확인(join 지역별로 잘됐는지 확인)
colSums(is.na(ami_final))
head(ami_final)

#########################
# 최종 의료 데이터 합치기
#########################
# 일단 left_join을 통해서 지역별로 다 합쳐보기
medical_final <- medical_retrans_final %>% 
  left_join(medical_emergency_final, by="지역") %>% 
  left_join(medical_stay_final, by="지역") %>% 
  left_join(medical_ktas_final, by="지역") %>% 
  rename(region = 지역)
# 컬럼별 NA 개수 확인(join 지역별로 잘됐는지 확인)
colSums(is.na(medical_final))
head(medical_final)


################################################################################
############################# 지역구조 #########################################
################################################################################
region_old <- read.csv("data/region/고령인구비율 시도 시 군 구.csv")
region_density <- read.csv("data/region/지역별 인구 및 인구밀도.csv")
region_area <- read.csv("data/region/지역별 일반 면적현황.csv")
##################################################### 고령인구비율 시도 시 군 구
str(region_old)
head(region_old, 10)

region_old_final <- region_old %>%
  # 컬럼명 재정의
  rename(
    old_ratio = 고령인구비율,
    old_population = X65세이상인구,
    population = 전체인구
  )

region_old_final

######################################################## 지역별 인구 및 인구밀도
str(region_density)
head(region_density, 10)

region_density_final <- region_density %>%
  # 문자열로 된 숫자 , 제거하고 numeric 타입으로 변환
  mutate(
    인구밀도 = str_replace_all(인구밀도, ",", ""),
    인구밀도 = as.numeric(인구밀도)
  ) %>%
  # 컬럼명 재정의
  rename(
    density = 인구밀도
  ) %>%
  # 필요 컬럼만 추출
  select(지역, density)

region_density_final

########################################################### 지역별 일반 면적현황
str(region_area)
head(region_area, 10)

region_area_final <- region_area %>%
  # 컬럼명 재정의
  rename(area = 면적) %>%
  # 필요 컬럼만 재추출
  select(지역, area)

region_area_final

#############################
# 최종 지역구조 데이터 합치기
#############################
# 일단 left_join을 통해서 지역별로 다 합쳐보기
region_final <- region_old_final %>% 
  left_join(region_density_final, by="지역") %>% 
  left_join(region_area_final, by="지역") %>% 
  rename(region = 지역)
# 컬럼별 NA 개수 확인(join 지역별로 잘됐는지 확인)
colSums(is.na(region_final))
head(region_final)

################################################################################
############################# 2차 전처리 #######################################
################################################################################
head(infra_final)
head(ami_final)
head(medical_final)
head(region_final)

# 카테고리별로 나누어진 데이터 하나로 최종 합치기
final_data <- infra_final %>% 
  inner_join(ami_final, by="region") %>% 
  inner_join(medical_final, by="region") %>% 
  inner_join(region_final, by="region")
# 컬럼별 NA 개수 확인(join 지역별로 잘됐는지 확인)
colSums(is.na(final_data))

final_data
dim(final_data)
head(final_data)
summary(final_data)
str(final_data)


# 절대값을 비율 변수로 만들기 (10만명 당 비율) + 심근경색 환자 비율
final_data <- final_data %>%
  mutate(
    emt_per_ambulance = emt_level1 / amb_total,
    hos_per_100k = hos_total / population * 100000,
    bed_per_100k = bed_total / population * 100000,
    er_per_100k  = emergency_total / population * 100000,
    ami_ratio = ami_count / emergency_total
  )

# 비율 변수로 만든 절대값 컬럼은 삭제
final_data <- final_data %>% 
  select(-amb_total, -emt_level1, -hos_total, -bed_total, -emergency_total, -old_population, -population, -ami_count)

# 수도권 변수 추가
final_data <- final_data %>%
  mutate(
    metro = ifelse(region %in% c("서울","경기","인천"), 1, 0),
  )

# 지역과 수도권은 범주형 데이터로 변환
final_data$region = as.factor(final_data$region)
final_data$metro = factor(final_data$metro, levels=c(0,1), labels=c("비수도권", "수도권"))

# 컬럼명 순으로 정렬 (지역만 맨 앞에)
final_data <- final_data %>%
  select(region, sort(names(.)[names(.) != "region"]))

# # 서로 단위가 다른 데이터 값의 스케일 일치 시키기 (표준화 -> 평균:0, 표준편차:1)
# final_data_scaled <- final_data %>%
#   mutate(across(
#     c(amb_per_100k, hos_per_100k, bed_per_100k,
#       react_time, er_stay_time, density, old_ratio),
#     scale
#   ))

# 파일로 추출
write.csv(final_data, "final_data.csv", row.names = FALSE, fileEncoding = "CP949")
# write.csv(final_data_scaled, "final_data_scaled.csv", row.names = FALSE, fileEncoding = "CP949")

