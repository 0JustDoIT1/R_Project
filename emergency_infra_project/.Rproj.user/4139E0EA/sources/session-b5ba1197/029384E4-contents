

#####
#데이터 불러오기
#####


infra_bed <- read.csv(
  "data/raw/infra_bed.csv",
  fileEncoding = "UTF-8",
  header = FALSE
)

View(infra_bed)

head(infra_bed)
names(infra_bed)
str(infra_bed)

#####
# 컬럼명 정리
#####

names(infra_bed) <- c(
  "region",
  "item",
  "unit",
  "bed_count"
)


#####
# 첫 행 제거
#####

infra_bed_clean <- infra_bed[-1, ]

#컬럼 확인
names(infra_bed)


#####
# 병상 수(전체)만 추출
#####

infra_bed_total <- subset(
  infra_bed_clean,
  item == "전체"
)


#####
# 숫자형 변환
#####

infra_bed_total$bed_count <- as.numeric(
  gsub(",", "", infra_bed_total$bed_count)
)


#####
# 지역 그룹 생성
#####

infra_bed_total$area_group <- ifelse(
  infra_bed_total$region %in% c("서울", "경기", "인천"),
  "수도권",
  "비수도권"
)


#####
# 확인
#####

head(infra_bed_total)
str(infra_bed_total)


###
# 정리본 저장
###
write.csv(
  infra_bed_total,
  "data/processed/infra_bed_total.csv",
  row.names = FALSE
)


###
# 시각화
###
boxplot(
  bed_count ~ area_group,
  data = infra_bed_total,
  main = "수도권 vs 비수도권 응급의료기관 병상 수 비교",
  xlab = "지역 그룹",
  ylab = "병상 수"
)


barplot(
  infra_bed_total$bed_count,
  names.arg = infra_bed_total$region,
  main = "지역별 응급의료기관 병상 수",
  ylab = "병상 수",
  las = 2
)
