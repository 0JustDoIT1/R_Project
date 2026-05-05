#####
#데이터 불러오기
#####


infra_ambulance_staff <- read.csv(
  "data/raw/infra_ambulance_staff.csv",
  fileEncoding = "UTF-8",
  header = FALSE
)

View(infra_ambulance_staff)

head(infra_ambulance_staff)
names(infra_ambulance_staff)
str(infra_ambulance_staff)

#####
# 컬럼명 정리
#####

names(infra_ambulance_staff) <- c(
  "region",
  "category",
  "item",
  "unit",
  "count"
)


#####
# 첫 행 제거
#####

infra_ambulance_staff_clean <- infra_ambulance_staff[-1, ]

#####
# 구급차 데이터 추출
#####

ambulance_data <- infra_ambulance_staff_clean[
  infra_ambulance_staff_clean$category == "119 구급차" &
    infra_ambulance_staff_clean$item %in% c("계", "특수", "일반"),
]
#####
# 의료인력 수 추출
#####

staff_data <- infra_ambulance_staff_clean[
  infra_ambulance_staff_clean$category == "탑승인력" &
    infra_ambulance_staff_clean$item == "계",
]


####### 확인 #####
head(ambulance_data)
head(staff_data)


#####
# count 숫자형 변환
#####

ambulance_data$count <- as.numeric(
  gsub(",", "", ambulance_data$count)
)


#####
# 지역별 합산
#####

ambulance_sum <- aggregate(
  count ~ region,
  data = ambulance_data,
  sum
)

names(ambulance_sum) <- c("region", "ambulance_count")

ambulance_sum$area_group <- ifelse(
  ambulance_sum$region %in% c("서울", "경기", "인천"),
  "수도권",
  "비수도권"
)

head(ambulance_sum)
str(ambulance_sum)
nrow(ambulance_sum) #17이어야 정상


###
# 저장
###
write.csv(
  ambulance_sum,
  "data/processed/ambulance_sum.csv",
  row.names = FALSE
)

write.csv(
  staff_sum,
  "data/processed/staff_sum.csv",
  row.names = FALSE
)
