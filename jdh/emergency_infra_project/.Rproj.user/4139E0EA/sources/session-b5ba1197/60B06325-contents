# =========================
# 1. 작업 경로 확인
# =========================

getwd()


# =========================
# 2. 원본 데이터 불러오기
# =========================

infra_institution <- read.csv(
  "data/raw/infra_institution.csv",
  fileEncoding = "UTF-8",
  header = FALSE
)

View(infra_institution)


# =========================
# 3. 컬럼명 정리
# =========================

names(infra_institution) <- c(
  "region",
  "item",
  "unit",
  "institution_count"
)


# =========================
# 4. 데이터 전처리
# =========================

# 첫 행 제거
infra_institution_clean <- infra_institution[-1, ]

# 숫자형 변환
infra_institution_clean$institution_count <- as.numeric(
  infra_institution_clean$institution_count
)

# 데이터 확인
head(infra_institution_clean)
str(infra_institution_clean)


# =========================
# 5. 지역별 총 기관 수 합산
# =========================

infra_institution_sum <- aggregate(
  institution_count ~ region,
  data = infra_institution_clean,
  sum
)


# =========================
# 6. 수도권 / 비수도권 그룹 생성
# =========================

infra_institution_sum$area_group <- ifelse(
  infra_institution_sum$region %in% c("서울", "경기", "인천"),
  "수도권",
  "비수도권"
)

# 데이터 확인
head(infra_institution_sum)
str(infra_institution_sum)


# =========================
# 7. 정리 데이터 저장
# =========================

write.csv(
  infra_institution_sum,
  "data/processed/infra_institution_sum.csv",
  row.names = FALSE
)


# =========================
# 8. 시각화
# =========================

boxplot(
  institution_count ~ area_group,
  data = infra_institution_sum,
  main = "수도권 vs 비수도권 응급의료기관 수 비교",
  xlab = "지역 그룹",
  ylab = "응급의료기관 수"
)
