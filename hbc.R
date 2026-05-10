getwd() # 현재 작업 디렉토리 확인
setwd("D:/Project/AI_class/R_Project_data") # 작업 디렉토리 지정

# =========================
# 사용 라이브러리 모음
# =========================
library(tidyverse) # 데이터 분석 기본 패키지 묶음
library(coin)     # 비모수 통계 분석 패키지 : wilcox_test()
library(effsize)  # 효과크기 계산 패키지 : cohen.d()
library(ggplot2) # 대표적인 데이터 시각화 패키지
library(patchwork) # 여러 ggplot 그래프를 한 화면에 배치
library(car) # 회귀분석 진단 패키지 : vif()
library(mediation) # 매개효과 분석 패키지(간접효과 분석)
library(broom) # 회귀분석 결과를 데이터프레임 형태로 정리
library(lmtest) # 선형회귀 추가 검정 패키지 : bptest()
library(sf) # 공간데이터(지도/shp) 처리 패키지
library(ggcorrplot) # 상관관계 행렬 시각화

# 전처리된 데이터 불러오기
data <- read.csv("final_data.csv", fileEncoding = "CP949") # 데이터 불러오기 + 인코딩 타입 지정
str(data) # 데이터 구조 파악
head(data) # 데이터 상위 6개 행 확인

# metro 변수 범주형 데이터로 변환
data$metro <- as.factor(data$metro)
str(data)

# 상관행렬 시각화 - 모든 변수 간 관계 한눈에
data %>%
  # 컬럼 선택
  dplyr::select(infra_hospital_per_area, infra_amb_per_area,
                ami_amb, ami_heal, ami_trans, ami_dead, region_old) %>%
  # 상관계수 계산 함수: cor()
  # 결측치(NA) 제외 후 계산, spaearman(순위 기반, 표본 수 적은 경우, 정규성 어려운 경우)
  cor(use = "complete.obs", method = "spearman") %>% 
  # 상관행렬 시각화
  # 정사각형 형태 hitmap, 하단 삼각형만 표시, 상관관계 숫자 표시, 숫자 크기
  ggcorrplot(method = "square", type = "lower", 
             lab = TRUE, lab_size = 3)

# 강한 관계
# ami_heal ↔ ami_trans : -0.73  강한 음의 상관
#   → 입원치료 제공률이 높을수록 전원율이 낮음
#   → 자체 처리 역량이 높으면 전원할 필요가 없다는 해석

# 중간 관계
# infra_hospital_per_area ↔ infra_amb_per_area : +0.53
#   → 병원과 구급차 인프라는 함께 몰려 있음 (수도권)
# infra_amb_per_area ↔ region_old : -0.53
#   → 고령화율 높을수록 구급차 접근성 낮음 (Step 2 결과와 일치)
# infra_hospital_per_area ↔ region_old : -0.47
#   → 고령화율 높을수록 병원 접근성 낮음 (Step 2 결과와 일치)

# 약한 관계 (주목)
# 인프라 ↔ ami_dead : -0.17, 0.01
#   → 인프라와 사망률 관계 매우 약함 → Step 3 결과와 일치
# ami_heal ↔ ami_dead : -0.34
#   → 입원치료↑ 사망률↓ → Step 4 결과와 일치

################################################################################
################################################################################
################################################################################
# 1단계 : 인프라 격차 검증
# H0 : 수도권과 비수도권 응급기관 수 차이 없음
# H1 : 수도권 응급기관 수가 더 많음

# 인구 기준 / 면적 기준 각각 진행
# 1-1. 인구 기준

# EDA (탐색적 분석)
# 기초 통계 요약 정보 : summary()
summary(data[, c("infra_hospital_per100k", "infra_amb_per100k")])

par(mfrow = c(2,2)) # 그래프 출력 영역 분할 (2x2)
# 히스토그램(분포와 치우침 확인)
hist(data$infra_hospital_per100k, main="병원 수 (per100k)", col = "steelblue")
hist(data$infra_amb_per100k, main="구급차 수 (per100k)", col = "steelblue")
# 박스플롯(중앙값, 사분위수, 이상치 확인)
boxplot(data$infra_hospital_per100k, main="병원 수 (per100k)")
boxplot(data$infra_amb_per100k, main="구급차 수 (per100k)")
par(mfrow = c(1,1)) # 그래프 화면 분할 설정 초기화

# 정규성 검정
shapiro.test(data$infra_hospital_per100k) # p-value < 2.2e-16 : 정규분포 아님
shapiro.test(data$infra_amb_per100k) # p-value = 9.019e-15 : 정규분포 아님

# 수도권/비수도권 분포 확인
par(mfrow = c(1, 2))
# 박스플롯 포뮬라 y ~ x : x 그룹 별로 나눠서 그려라, main : 타이틀 , col : 색상(그룹 색상)
boxplot(infra_hospital_per100k ~ metro, data = data,
        main = "병원 수 (per100k)", col = c("#1D9E75", "#378ADD"))
boxplot(infra_amb_per100k ~ metro, data = data,
        main = "구급차 수 (per100k)", col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# Wilcoxon test (정규분포 아니므로 t-test 대신 wilcoxon test, 비모수검정, 순위 검증)
# wilcox 에서 포뮬라 y ~ x : x 그룹 별로 y 비교
wilcox.test(infra_hospital_per100k ~ metro, data = data)
  # p-value = 2.375e-09
  # 귀무가설 기각 : 수도권과 비수도권의 병원 수(인구당) 차이가 유의함
wilcox.test(infra_amb_per100k ~ metro, data = data)
  # p-value = 1.252e-15
  # 귀무가설 기각 : 수도권과 비수도권의 구급차 수(인구당) 차이가 유의함

# Cohen's d — 효과크기 검증
# 0.2 : 작은 차이 / 0.5 : 중간 차이 / 0.8이상 : 큰 차이
# 그룹 순서 확인
levels(data$metro) # [1] "비수도권" "수도권"
cohen.d(infra_hospital_per100k ~ metro, data = data)
  # d estimate: 0.7098288 (medium) : 비수도권 - 수도권
  # 비수도권 > 수도권
cohen.d(infra_amb_per100k ~ metro, data = data)
  # d estimate: 0.9778843 (large) : 비수도권 - 수도권
  # 비수도권 > 수도권

# 그룹별 중앙값 확인 (Wilcoxon은 중앙값 기준)
# 실제 차이 값 비교를 위해서 진행
data %>%
  group_by(metro) %>% # metro로 그룹핑
  summarise( # 그룹별 요약 통계 : 안에 함수에 대한 결과를 요약해서 표로 생성
    hospital_median = round(median(infra_hospital_per100k), 4), # 반올림 처리
    hospital_mean   = round(mean(infra_hospital_per100k), 4),
    amb_median      = round(median(infra_amb_per100k), 4),
    amb_mean        = round(mean(infra_amb_per100k), 4)
  )
  # metro    hospital_median hospital_mean amb_median amb_mean
  # <fct>              <dbl>         <dbl>      <dbl>    <dbl>
  # 1 비수도권           1.24          1.65        6.58     7.97
  # 2 수도권             0.500         0.719       1.85     2.62
  # 병원 : 비수도권이 수도권의 약 2.5배 / 구급차 : 비수도권이 수도권의 약 3.6배

### 결론
### 인구당 기준으로 비수도권의 응급 인프라가 수도권보다 통계적으로 유의미하게 더 많다
### 이는, 우리가 예상한 수도권/비수도권 차이가 유의미함을 증명하지만 방향이 예상과는 다른 결과
### 하지만, 인구가 적은 지역에 인프라가 분산되어 있어 나올 수 있는 착시효과일 수도 있기 때문에
### 면적을 통해 더 확실한 분석 진행행

# 1-2. 면적 기준
# 기술통계
summary(data[, c("infra_hospital_per_area", "infra_amb_per_area")])

# 분포 확인
par(mfrow = c(2, 2))
hist(data$infra_hospital_per_area, main = "병원 수 (per_area)", col = "steelblue")
hist(data$infra_amb_per_area,      main = "구급차 수 (per_area)", col = "steelblue")
boxplot(data$infra_hospital_per_area, main = "병원 수 (per_area)")
boxplot(data$infra_amb_per_area,      main = "구급차 수 (per_area)")
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$infra_hospital_per_area) # p-value < 2.2e-16 : 정규분포 아님
shapiro.test(data$infra_amb_per_area) # p-value < 2.2e-16 : 정규분포 아님

# 그룹별 분포 확인
par(mfrow = c(1, 2))
boxplot(infra_hospital_per_area ~ metro, data = data,
        main = "병원 수 (per_area)", col = c("#1D9E75", "#378ADD"))
boxplot(infra_amb_per_area ~ metro, data = data,
        main = "구급차 수 (per_area)", col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# Wilcoxon test
wilcox.test(infra_hospital_per_area ~ metro, data = data)
  # p-value = 5.074e-09
  # 귀무가설 기각 : 수도권과 비수도권의 병원 수(면적당) 차이가 유의함
wilcox.test(infra_amb_per_area ~ metro, data = data)
  # p-value = 1.656e-10
  # 귀무가설 기각 : 수도권과 비수도권의 구급차 수(면적당) 차이가 유의함

# Cohen's d
cohen.d(infra_hospital_per_area ~ metro, data = data)
  # d estimate: -0.5317112 (medium)
  # 수도권 > 비수도권
cohen.d(infra_amb_per_area ~ metro, data = data)
  # d estimate: -0.3691115 (small)
  # 수도권 > 비수도권

# 그룹별 중앙값
data %>%
  group_by(metro) %>%
  summarise(
    hospital_median = round(median(infra_hospital_per_area), 4),
    hospital_mean   = round(mean(infra_hospital_per_area), 4),
    amb_median      = round(median(infra_amb_per_area), 4),
    amb_mean        = round(mean(infra_amb_per_area), 4)
  )
# metro    hospital_median hospital_mean amb_median amb_mean
# <fct>              <dbl>         <dbl>      <dbl>    <dbl>
# 1 비수도권          0.0023        0.0213     0.0115   0.0589
# 2 수도권            0.0361        0.0534     0.122    0.146
# 병원 : 수도권이 비수도권의 약 15.7배 / 구급차 : 수도권이 비수도권의 약 10.6배

### 결론
### 면적당 기준으로 수도권의 인프라가 비수도권의 인프라보다 통계적으로 유의미하게 더 많다
### 이는, 우리가 예상한 수도권/비수도권 차이가 유의미함을 증명하고 방향까지 일치한다
### 즉, 같은 면적 안에 수도권은 인프라가 밀집되어 있고, 비수도권은 극도로 분산되어 있다

##### 인프라 최종 결론
##### " 비수도권은 인프라 수가 부족한게 아니라, 접근성이 부족하다 "
##### 이는 비수도권의 인프라가 넓은 면적에 흩어져 있어 응급상황 발생 시 실제로 인프라에
##### 도달하기까지의 거리와 시간이 훨씬 길다는 것을 의미할 것으로 예상
##### 진행 과정에 있어서 인구당보다는 면적당이 더 적합함
##### why? 인구당 : 자원 배분의 형평성 / 면적당 : 실제 도달 가능성 => 응급상황은 거리와 시간이 우선인 상황
##### 따라서, 비수도권의 응급의료 문제의 핵심은 인프라 증설 자체도 중요하지만,
##### 배치 최적화와 접근성 개선이 우선 과제로 예상

# 결과 시각화
# 인구당 시각화
data %>%
  group_by(metro) %>%
  summarise(
    병원  = median(infra_hospital_per100k),
    구급차 = median(infra_amb_per100k)
  ) %>%
  # 세로형으로 변환
  # metro 제외한 나머지 컬럼 세로로 변환
  # names_to : 기존 컬럼명을 "변수" 컬럼에 저장
  # values_to : 실제 숫자값을 "중앙값" 컬럼에 저장
  pivot_longer(-metro, names_to = "변수", values_to = "중앙값") %>%
  ggplot(aes(x = 변수, y = 중앙값, fill = metro)) + # 그래프 기본
  # 막대그래프 생성 (dodge : 옆으로 나란히 배치, width: 넓이, alpha : 투명도)
  geom_col(position = "dodge", width = .5, alpha = .85) +
  # 막대 위 숫자 표시 (위치, 크기)
  geom_text(aes(label = round(중앙값, 4)),
            position = position_dodge(.5), vjust = -.5, size = 4) +
  # 그룹별 색상 직접 지정
  scale_fill_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  # 그래프 제목/축 이름 설정
  labs(title    = "인구당 응급 인프라 — 수도권 vs 비수도권",
       subtitle = "비수도권이 병원 2.5배 / 구급차 3.6배 높음",
       x = NULL, y = "중앙값", fill = NULL) +
  theme_minimal(base_size = 12) +
  # 범례 아래쪽 배치, 굵기 색상
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "grey50"))

# 면적당 시각화
data %>%
  group_by(metro) %>%
  summarise(
    병원  = median(infra_hospital_per_area),
    구급차 = median(infra_amb_per_area)
  ) %>%
  pivot_longer(-metro, names_to = "변수", values_to = "중앙값") %>%
  ggplot(aes(x = 변수, y = 중앙값, fill = metro)) +
  geom_col(position = "dodge", width = .5, alpha = .85) +
  geom_text(aes(label = round(중앙값, 4)),
            position = position_dodge(.5), vjust = -.5, size = 4) +
  scale_fill_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "면적당 응급 인프라 — 수도권 vs 비수도권",
       subtitle = "수도권이 병원 15.7배 / 구급차 10.6배 높음",
       x = NULL, y = "중앙값", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold"),
        plot.subtitle   = element_text(color = "grey50"))

################################################################################
################################################################################
################################################################################
# 2단계 : 교란변수 검토 - 고령화율
# 검토 목적
# 인프라와 사망률 양쪽에 동시에 연관되는지 확인
# H0: 고령화율은 인프라, 사망률과 독립적이다 (교란변수 아님)
# H1: 고령화율이 인프라·사망률 양쪽과 동시에 유의한 상관이 있다 (교란변수)

# 기술 통계
summary(data$region_old)

# EDA (탐색적 분석)
par(mfrow = c(1, 2))
hist(data$region_old, main = "고령화율 분포", col = "steelblue")
boxplot(region_old ~ metro, data = data,
        main = "고령화율 (수도권 vs 비수도권)",
        col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$region_old)
# p-value = 5.535e-09 : 정규분포 아님

# cor.test.default(data$region_old, data$infra_hospital_per_area, 에서:
# tie때문에 정확한 p값을 계산할 수 없습니다
# 표본이 250개로 충분히 크기 때문에 exat=FALSE로 근사 p값을 사용해도 신뢰 가능

# cor.test() : 상관계수 + 유의성 검정(p-value)
# 상관분석에서 Pearson 대신 Spearman 사용
# Pearson : 두 변수가 선형 관계로 가정 (정규분포에 사용), 이상치에 민감
# Spearman : 순위 기반, 선형 관계 아니어도 가능 (정규분포 아니어도 됨), 이상치에 강건
# rho : 상관계수 값, p-value : 유의성

# 고령화율 ↔ 인프라 (면적당 기준)
cor.test(data$region_old, data$infra_hospital_per_area, method = "spearman", exact = FALSE)
  # p-value = 2.128e-15, rho = -0.4738613 (중간)
  # 귀무가설 기각 : 고령화율과 병원 면적당 인프라 사이에 관계가 있음
  # (-) 관계 : 고령화율이 높을수록 병원 면적당 인프라가 낮아짐
cor.test(data$region_old, data$infra_amb_per_area, method = "spearman", exact = FALSE)
  # p-value < 2.2e-16, rho = -0.529497 (강함)
  # 귀무가설 기각 : 고령화율과 병원 면적당 인프라 사이에 관계가 있음
  # (-) 관계 : 고령화율이 높을수록 구급차 면적당 인프라가 낮아짐

# 고령화율 ↔ 사망률
cor.test(data$region_old, data$ami_dead, method = "spearman", exact = FALSE)
  # p-value = 0.1187, rho = -0.09894509 (거의 없음)
  # 귀무가설 기각 실패 : 고령화율과 사망률 사이에 관계가 없음

### 결론
### 교란변수가 되려면 둘 다 유의해야 하는데, 사망률에서 유의하지 않음
### 즉, 교란변수가 아님
### 하지만, 인프라와는 유의한 관계가 있기 때문에 통제변수로 사용 필요

################################################################################
################################################################################
################################################################################
# 3단계 : 인프라 -> 치료역량
# H0: 면적당 인프라는 치료역량(구급차 이용률, 입원치료 제공률, 전원율)과 관계 없다
# H1: 면적당 인프라↑ → 구급차 이용률↑, 입원치료 제공률↑, 전원율은 방향 확인 필요

# 기술통계
summary(data[, c("ami_amb", "ami_heal", "ami_trans")])

# EDA (탐색적 분석)
par(mfrow = c(2, 3))
hist(data$ami_amb,   main = "구급차 이용률",   col = "steelblue")
hist(data$ami_heal,  main = "입원치료 제공률", col = "steelblue")
hist(data$ami_trans, main = "전원율",          col = "steelblue")
par(mfrow = c(1, 3))
boxplot(ami_amb   ~ metro, data = data, main = "구급차 이용률",   col = c("#1D9E75", "#378ADD"))
boxplot(ami_heal  ~ metro, data = data, main = "입원치료 제공률", col = c("#1D9E75", "#378ADD"))
boxplot(ami_trans ~ metro, data = data, main = "전원율",          col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$ami_amb) # p-value = 0.0003124 : 정규분포 아님
shapiro.test(data$ami_heal) # p-value < 2.2e-16 : 정규분포 아님
shapiro.test(data$ami_trans) # p-value < 2.2e-16 : 정규분포 아님

# 다중공선성 확인 (VIF)
# 회귀모델 적용 전에 독립변수들 간 상관성이 높은지 확인
# 기본 회귀모델로 VIF 확인
# 1 : 전혀 문제 없음 , 1~5 : 보통 괜찮음 , 5 이상 : 주의 , 10 이상 : 심각한 다중공산성

# lm : 선형회귀 = 독립변수들이 종속변수에 어떤 영향을 주는지 분석
vif_model <- lm(ami_amb ~ infra_hospital_per_area + infra_amb_per_area + region_old,
                data = data)
vif(vif_model)
  # infra_hospital_per_area      infra_amb_per_area              region_old 
  # 1.553027                1.514066                1.044485 
  # 5 이상인 값이 없으므로 동시 사용 가능

# 세 가지 치료역량 변수 각각 회귀
m_amb   <- lm(ami_amb   ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data)
m_heal  <- lm(ami_heal  ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data)
m_trans <- lm(ami_trans ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data)

summary(m_amb)
  # 고령화율만 유의한 관계가 있고, 그 외에는 없음
  # Multiple R-squared:  0.03667,	Adjusted R-squared:  0.02493
  # 설명력 매우 낮음
summary(m_heal)
  # 유의한 영향없음
  # Multiple R-squared:  0.03178,	Adjusted R-squared:  0.01997
  # 설명력 매우 낮음
summary(m_trans)
  # 고령화율만 유의한 관계가 있고, 그 외에는 없음
  # Multiple R-squared:  0.04889,	Adjusted R-squared:  0.03729 
  # 설명력 매우 낮음

# 각 모델의 설명력이 매우 낮음
# 두 인프라 변수 모두 귀무가설 기각 실패 : 인프라가 치료 역량에 유의한 영향 없음
# 이는 예상과는 다른 결과로 존재하는 데이터 자체가 설명력이 부족

# 구급차 이용률
# 수도권: 병원 수↑ → 구급차 이용률↓, 구급차 수↑ → 구급차 이용률↑ (유의)
# 비수도권: 인프라 변수 모두 비유의, 고령화율만 유의

# 입원치료 제공률
# 수도권/비수도권 모두 모든 변수 비유의
# 입원치료 제공률은 인프라로 설명이 안 됨

# 전원율
# 수도권/비수도권 모두 비유의
# 전원율도 인프라로 설명이 안 됨

# 회귀 모델을 박스플롯에 넣으면 자동으로 회귀 진단용 그래프 4개 나옴

# 구급차 이용률
par(mfrow = c(2, 2))
plot(m_amb, main = "잔차 진단 — 구급차 이용률")
par(mfrow = c(1, 1))
shapiro.test(residuals(m_amb))
# 잔차 검증 : 등분산성 체크 => 이분산성이 높으면 결과 신뢰도가 떨어질 가능성 있음
bptest(m_amb)

# 입원치료 제공률
par(mfrow = c(2, 2))
plot(m_heal, main = "잔차 진단 — 입원치료 제공률")
par(mfrow = c(1, 1))
shapiro.test(residuals(m_heal))
bptest(m_heal)

# 전원율
par(mfrow = c(2, 2))
plot(m_trans, main = "잔차 진단 — 전원율")
par(mfrow = c(1, 1))
shapiro.test(residuals(m_trans))
bptest(m_trans)

# 인프라 -> 치료역량 산점도 + 추세선
# 극단값 제거 + 수도권/비수도권 색상 구분
# 구급차 이용률
data %>%
  # filter() : 조건에 맞는 행만 추출
  # quantile() : 데이터를 크기 순으로 정렬했을 때 상위 5% 직전 값
  filter(infra_hospital_per_area < quantile(infra_hospital_per_area, 0.95),
         infra_amb_per_area < quantile(infra_amb_per_area, 0.95)) %>%
  dplyr::select(infra_hospital_per_area, infra_amb_per_area, ami_amb, metro) %>%
  # cols = : 세로형 변환할 컬럼 지정
  pivot_longer(cols = c(infra_hospital_per_area, infra_amb_per_area),
               names_to = "인프라변수", values_to = "인프라값") %>%
  # mutate() : 새로운 컬럼 추가 혹은 기존 컬럼 수정
  # case_when() : 조건문 => 조건 ~ 결과
  mutate(인프라변수 = case_when(
    인프라변수 == "infra_hospital_per_area" ~ "병원(면적당)",
    인프라변수 == "infra_amb_per_area"      ~ "구급차(면적당)"
  )) %>%
  ggplot(aes(x = 인프라값, y = ami_amb, color = metro)) +
  # 산점도(scatter plot) 생성 함수
  geom_point(alpha = .4, size = 2) +
  # 추세선(trend line) 추가 함수
  # method = "lm" : 선형회귀선 사용(직선 추세선)
  # se = FALSE : 신뢰구간 음영 제거
  # aes(group = ) : 그룹별로 따로 회귀선 생성
  geom_smooth(method = "lm", se = FALSE, linewidth = 1,
              aes(group = metro)) +
  # 그래프를 변수별로 나눠서 출력
  # scales = "free_x" : 각 그래프 x축 범위를 독립적으로 설정
  facet_wrap(~인프라변수, scales = "free_x", ncol = 2) +
  # 색상 직접 설정
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 3-1 | 인프라 → 구급차 이용률",
       subtitle = "수도권 vs 비수도권 / 상위 5% 극단값 제거",
       x = "인프라 (면적당)", y = "구급차 이용률 (%)",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        strip.text      = element_text(face = "bold"))

# 입원치료 제공률
data %>%
  filter(infra_hospital_per_area < quantile(infra_hospital_per_area, 0.95),
         infra_amb_per_area < quantile(infra_amb_per_area, 0.95)) %>%
  dplyr::select(infra_hospital_per_area, infra_amb_per_area, ami_heal, metro) %>%
  pivot_longer(cols = c(infra_hospital_per_area, infra_amb_per_area),
               names_to = "인프라변수", values_to = "인프라값") %>%
  mutate(인프라변수 = case_when(
    인프라변수 == "infra_hospital_per_area" ~ "병원(면적당)",
    인프라변수 == "infra_amb_per_area"      ~ "구급차(면적당)"
  )) %>%
  ggplot(aes(x = 인프라값, y = ami_heal, color = metro)) +
  geom_point(alpha = .4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1,
              aes(group = metro)) +
  facet_wrap(~인프라변수, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 3-2 | 인프라 → 입원치료 제공률",
       subtitle = "수도권 vs 비수도권 / 상위 5% 극단값 제거",
       x = "인프라 (면적당)", y = "입원치료 제공률 (%)",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        strip.text      = element_text(face = "bold"))

# 전원율
data %>%
  filter(infra_hospital_per_area < quantile(infra_hospital_per_area, 0.95),
         infra_amb_per_area < quantile(infra_amb_per_area, 0.95)) %>%
  dplyr::select(infra_hospital_per_area, infra_amb_per_area, ami_trans, metro) %>%
  pivot_longer(cols = c(infra_hospital_per_area, infra_amb_per_area),
               names_to = "인프라변수", values_to = "인프라값") %>%
  mutate(인프라변수 = case_when(
    인프라변수 == "infra_hospital_per_area" ~ "병원(면적당)",
    인프라변수 == "infra_amb_per_area"      ~ "구급차(면적당)"
  )) %>%
  ggplot(aes(x = 인프라값, y = ami_trans, color = metro)) +
  geom_point(alpha = .4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1,
              aes(group = metro)) +
  facet_wrap(~인프라변수, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 3-3 | 인프라 → 전원율",
       subtitle = "수도권 vs 비수도권 / 상위 5% 극단값 제거",
       x = "인프라 (면적당)", y = "전원율 (%)",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        strip.text      = element_text(face = "bold"))

### 결론
### 수도권에서만 구급차 이용률 모델이 유의했고, 나머지는 대부분 유의하지 않다
### 전체적으로 R²가 낮고 유의한 변수가 적어서 "인프라 → 치료역량" 경로가 약하다

################################################################################
################################################################################
################################################################################
# 4단계 : 치료역량 → 사망률
# H0: 치료역량 변수들(구급차 이용률, 입원치료 제공률, 전원율)은 사망률과 관계 없다
# H1: 구급차 이용률↑, 입원치료 제공률↑ → 사망률↓ / 전원율은 방향 확인 필요

# 기술통계
summary(data$ami_dead)

# EDA (탐색적 분석)
par(mfrow = c(1, 2))
hist(data$ami_dead, main = "사망률 분포", col = "steelblue")
boxplot(ami_dead ~ metro, data = data,
        main = "사망률 (수도권 vs 비수도권)",
        col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# shp 파일 로드
map <- st_read("sig.shp", options = "ENCODING=CP949") %>%
  st_set_crs(5179)

# 시도 코드 → 시도명 매핑
sido_map <- c(
  "11" = "서울", "26" = "부산", "27" = "대구", "28" = "인천",
  "29" = "광주", "30" = "대전", "31" = "울산", "36" = "세종",
  "41" = "경기", "43" = "충북", "44" = "충남", "45" = "전북",
  "46" = "전남", "47" = "경북", "48" = "경남", "50" = "제주",
  "51" = "강원"
)

# map 조인 키 생성
map <- map %>%
  mutate(
    sido_cd    = substr(SIG_CD, 1, 2), # substr : 문자열 잘라내기
    sido_nm    = sido_map[sido_cd],
    sig_kor_nm = paste0(sido_nm, " ", SIG_KOR_NM) # paste0 : 문자열 이어붙이기
  )

# data 조인 키 생성
data <- data %>%
  mutate(sig_kor_nm = paste0(region, " ", district))

# 조인
map_joined <- map %>%
  left_join(data, by = "sig_kor_nm")

# 수도권 경계 추출
map_metro <- map_joined %>%
  filter(metro == "수도권") %>%
  # 공간 데이터 함수 : 여러 지도 경계를 하나로 합치기 위해서
  st_union()

# 지도 1-1 — 면적당 병원 수 + 수도권 경계
# 로그 스케일 적용
ggplot(map_joined) +
  # 공간 데이터(지도 데이터)를 그리는 함수
  geom_sf(aes(fill = infra_hospital_per_area),
          color = "white", linewidth = 0.05) +
  geom_sf(data = map_metro,
          fill = NA, color = "#FF4444", linewidth = 0.05) +
  # 연속형 숫자 데이터를 색상 그라데이션으로 표현
  # scales::rescale() => 0 ~ 1 사이로 정규화
  # na.value : 결측치 색상 지정
  scale_fill_gradientn(
    colors   = c("#F8FAFC", "#BAE6FD", "#0284C7", "#0C4A6E"),
    values   = scales::rescale(c(0, 0.002, 0.02, 0.4)),
    name     = "병원 수\n(km² 당)",
    na.value = "grey90"
  ) +
  labs(title    = "EDA | 면적당 응급기관 수 분포",
       subtitle = "빨간선: 수도권") +
  # 지도 시각화에 많이 사용 : 축, 배경, 눈금 제거
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle   = element_text(color = "grey50", size = 10, hjust = 0.5),
        legend.position = "right")

# 지도 1-2 — 면적당 구급차 수 + 수도권 경계
ggplot(map_joined) +
  geom_sf(aes(fill = infra_amb_per_area),
          color = "white", linewidth = 0.05) +
  geom_sf(data = map_metro,
          fill = NA, color = "#FF4444", linewidth = 0.05) +
  scale_fill_gradientn(
    colors   = c("#F8FAFC", "#BAE6FD", "#0284C7", "#0C4A6E"),
    values   = scales::rescale(c(0, 0.005, 0.05, 3.3)),
    name     = "구급차 수\n(km² 당)",
    na.value = "grey90"
  ) +
  labs(title    = "EDA | 면적당 구급차 수 분포",
       subtitle = "빨간선: 수도권") +
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle   = element_text(color = "grey50", size = 10, hjust = 0.5),
        legend.position = "right")

# 지도 2 — 응급실 내 사망률 + 수도권 경계
ggplot(map_joined) +
  geom_sf(aes(fill = ami_dead),
          color = "white", linewidth = 0.05) +
  geom_sf(data = map_metro,
          fill = NA, color = "#378ADD", linewidth = 0.05) +
  scale_fill_gradientn(
    colors   = c("#F8FAFC", "#FCA5A5", "#DC2626", "#7F1D1D"),
    values   = scales::rescale(c(0, 0.5, 3, 15.4)),
    name     = "사망률 (%)",
    na.value = "grey90"
  ) +
  labs(title    = "EDA | 응급실 내 사망률 분포",
       subtitle = "파란선: 수도권") +
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle   = element_text(color = "grey50", size = 10, hjust = 0.5),
        legend.position = "right")



# 정규성 검정
shapiro.test(data$ami_dead) # p-value < 2.2e-16 : 정규분포 아님

# 0값 개수 확인
sum(data$ami_dead == 0) # 비교연산 통해서 숫자 더하기(TRUE : 1, FALSE : 0)
table(data$ami_dead == 0) # 빈도표 생성
# 응급실 내 사망자가 없는 지역이 실제로 존재

# 로그 변환 (0값 처리를 위해 +1)
# 자연로그 : 값을 로그 스케일로 변환
# 로그 변환 이유 : 한쪽으로 심하게 치우쳤거나, 극단값이 크거나, 분산 차이가 심할 때
# +1 하는 이유 : 0이 존재해서 계산이 안되기 때문에
data <- data %>%
  mutate(log_ami_dead = log(ami_dead + 1))

# 변환 후 분포 확인
par(mfrow = c(1, 2))
hist(data$log_ami_dead, main = "응급실 내 사망률", col = "steelblue")
boxplot(log_ami_dead ~ metro, data = data,
        main = "사망률 (수도권 vs 비수도권)",
        col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# 정규성 재검정
shapiro.test(data$log_ami_dead) # p-value = 1.628e-09 : 정규분포 아님

# 회귀 모델 (log 변환 적용)
m_dead <- lm(log_ami_dead ~ ami_amb + ami_heal + ami_trans + region_old,
             data = data)
summary(m_dead)

#              Estimate Std. Error t value Pr(>|t|)
# ami_amb     -0.004709   0.002983  -1.578    0.116    
# ami_heal    -0.102748   0.010676  -9.624  < 2e-16 ***
# ami_trans   -0.102144   0.013068  -7.816 1.61e-13 ***
# region_old  -0.005026   0.003411  -1.474    0.142

# Multiple R-squared:  0.3016,	Adjusted R-squared:  0.2902

# ami_amb   (구급차 이용률)   : p = 0.116  비유의 → 사망률과 관계 없음
# ami_heal  (입원치료 제공률) : p < 0.001  유의   → 입원치료↑ log(사망률)↓ (Estimate = -0.103)
# ami_trans (전원율)          : p < 0.001  유의   → 전원율↑  log(사망률)↓ (Estimate = -0.102)
# region_old(고령화율)        : p = 0.142  비유의 → 사망률과 관계 없음
# R² = 0.302 → 치료역량 변수들이 응급실 내 사망률의 30% 설명

# 층화 모델
data_metro    <- data %>% filter(metro == "수도권")
data_nonmetro <- data %>% filter(metro == "비수도권")

m_dead_metro    <- lm(log_ami_dead ~ ami_amb + ami_heal + ami_trans + region_old,
                      data = data_metro)
m_dead_nonmetro <- lm(log_ami_dead ~ ami_amb + ami_heal + ami_trans + region_old,
                      data = data_nonmetro)

summary(m_dead_metro)
summary(m_dead_nonmetro)

# 수도권 (n=77)
# ami_amb   (구급차 이용률)   : p = 0.348  비유의 → 사망률과 관계 없음
# ami_heal  (입원치료 제공률) : p < 0.001  유의   → 입원치료↑ log(사망률)↓ (Estimate = -0.094)
# ami_trans (전원율)          : p < 0.001  유의   → 전원율↑  log(사망률)↓ (Estimate = -0.103)
# region_old(고령화율)        : p = 0.395  비유의 → 사망률과 관계 없음
# R² = 0.252

# 비수도권 (n=173)
# ami_amb   (구급차 이용률)   : p = 0.254  비유의 → 사망률과 관계 없음
# ami_heal  (입원치료 제공률) : p < 0.001  유의   → 입원치료↑ log(사망률)↓ (Estimate = -0.104)
# ami_trans (전원율)          : p < 0.001  유의   → 전원율↑  log(사망률)↓ (Estimate = -0.103)
# region_old(고령화율)        : p = 0.118  비유의 → 사망률과 관계 없음
# R² = 0.318

### 비수도권에서는 치료역량(입원치료, 전원율)이 응급실 내 사망률을 더 강하게 설명
### 즉, 비수도권에서 치료역량 개선이 응급실 내 사망률 감소에 더 직접적인 영향을 미침

# 구급차 이용률 → 사망률
data %>%
  ggplot(aes(x = ami_amb, y = log_ami_dead, color = metro)) +
  geom_point(alpha = .4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1,
              aes(group = metro)) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 4-1 | 구급차 이용률 → 응급실 내 사망률",
       subtitle = "수도권 vs 비수도권 — 선형 추세선 포함",
       x = "구급차 이용률 (%)", y = "응급실 내 사망률",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

# 입원치료 제공률 → 사망률
data %>%
  ggplot(aes(x = ami_heal, y = log_ami_dead, color = metro)) +
  geom_point(alpha = .4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1,
              aes(group = metro)) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 4-2 | 입원치료 제공률 → 응급실 내 사망률",
       subtitle = "수도권 vs 비수도권 — 선형 추세선 포함",
       x = "입원치료 제공률 (%)", y = "응급실 내 사망률",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

# 전원율 → 사망률
data %>%
  ggplot(aes(x = ami_trans, y = log_ami_dead, color = metro)) +
  geom_point(alpha = .4, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1,
              aes(group = metro)) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 4-3 | 전원율 → 응급실 내 사망률",
       subtitle = "수도권 vs 비수도권 — 선형 추세선 포함",
       x = "전원율 (%)", y = "응급실 내 사망률",
       color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

# 핵심 결론
# 치료역량 중 입원치료 제공률, 전원율이 응급실 내 사망률에 유의한 영향을 미침
# R² = 0.302로 치료역량 변수들이 응급실 내 사망률의 약 30% 설명 (양호한 수준)
# 특히 비수도권(R²=0.318)에서 수도권(R²=0.252)보다 설명력이 높음
# → 비수도권에서 치료역량 개선이 응급실 내 사망률 감소에 더 직접적인 영향

# 한계점
# 응급실 내 사망률 외 골든타임, 도착 전 사망 등 핵심 변수 부재
# 나머지 70%는 본 연구에서 포함되지 않은 변수들로 설명될 가능성

################################################################################
################################################################################
################################################################################
# 5단계 : 매개효과 검증
# H0: 입원치료 제공률(ami_heal)은 인프라와 사망률 사이에서 매개 역할을 하지 않는다
# H1: 인프라 → ami_heal → ami_dead 매개 경로가 존재하며, 그 강도가 수도권/비수도권에서 다르다

# 1단계: 인프라 → ami_heal (매개 모델)
med_model <- lm(ami_heal ~ infra_hospital_per_area + infra_amb_per_area + region_old,
                data = data)

# 2단계: 인프라 + ami_heal → ami_dead (결과 모델)
out_model <- lm(log_ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
                  ami_heal + region_old,
                data = data)

# Bootstrap 매개효과 검정
set.seed(42) # 난수 고정 코드 => 랜덤 샘플링 결과 통일
# mediate : 매개효과 분석 함수
# treate = : 독립변수 지정
# mediator = : 매개변수 지정
# boot = TRUE : 부트스트래핑 사용 (데이터를 반복 재표본 추출)
# sims = 1000 : 반복 횟수
med_result <- mediate(med_model, out_model,
                      treat    = "infra_hospital_per_area",
                      mediator = "ami_heal",
                      boot     = TRUE,
                      sims     = 1000)
summary(med_result)

ggplot() +
  # annotate : 그래프 위에 도형, 선, 텍스트 등을 직접 추가 (rect, segment, text 등)
  # 보통 diagram을 그리는 방식
  
  # 노드
  annotate("rect", xmin=0, xmax=2, ymin=1.7, ymax=2.3, fill="#DBEAFE", color="#2563EB") +
  annotate("rect", xmin=4, xmax=6, ymin=2.7, ymax=3.3, fill="#DCFCE7", color="#0F6E56") +
  annotate("rect", xmin=8, xmax=10, ymin=1.7, ymax=2.3, fill="#FEE2E2", color="#DC2626") +
  # 텍스트
  annotate("text", x=1, y=2, label="인프라\n(병원 면적당)", size=3.5, fontface="bold") +
  annotate("text", x=5, y=3, label="입원치료\n제공률", size=3.5, fontface="bold") +
  annotate("text", x=9, y=2, label="응급실 내\n사망률", size=3.5, fontface="bold") +
  # a경로 (인프라→입원치료)
  annotate("segment", x=2, xend=4, y=2.2, yend=2.8,
           arrow=arrow(length=unit(0.3,"cm")), color="#0F6E56", linewidth=1) +
  annotate("text", x=3, y=2.65, label="a경로 (비유의)", size=3, color="#6B7280") +
  # b경로 (입원치료→사망률)
  annotate("segment", x=6, xend=8, y=2.8, yend=2.2,
           arrow=arrow(length=unit(0.3,"cm")), color="#0F6E56", linewidth=1) +
  annotate("text", x=7, y=2.65, label="b경로 (유의***)", size=3, color="#0F6E56", fontface="bold") +
  # c'경로 (직접효과)
  annotate("segment", x=2, xend=8, y=1.9, yend=1.9,
           arrow=arrow(length=unit(0.3,"cm")), color="#6B7280", linewidth=1, linetype="dashed") +
  annotate("text", x=5, y=1.7, label="직접효과 c' (비유의)", size=3, color="#6B7280") +
  # 간접효과 표시
  annotate("text", x=5, y=1.2,
           label="간접효과 (ACME) = −0.393  p=0.016 *",
           size=3.5, color="#2563EB", fontface="bold") +
  xlim(-0.5, 11) + ylim(0.8, 3.8) +
  labs(title    = "Step 5 | 매개효과 경로도",
       subtitle = "인프라 → 입원치료(매개) → 응급실 내 사망률") +
  theme_void() +
  theme(plot.title    = element_text(face="bold", size=13, hjust=0.5),
        plot.subtitle = element_text(color="grey50", size=10, hjust=0.5))

# ACME 비교 플롯 — 전체 / 수도권 / 비수도권
# 표 생성
med_plot_df <- data.frame(
  그룹     = c("전체", "수도권", "비수도권"),
  ACME     = c(-0.393, -0.224, -0.363),
  CI_lower = c(-0.718, -0.593, -0.932),
  CI_upper = c(-0.063,  0.063,  0.063),
  p값      = c("p=0.016 *", "p=0.146", "p=0.094 ."),
  유의     = c("유의", "비유의", "경계선")
) %>%
  mutate(그룹 = factor(그룹, levels = c("전체", "수도권", "비수도권")))

# 매개효과 결과 데이터를 사용
# ymin, ymax 활용해서 신뢰구간 범위 지정
ggplot(med_plot_df, aes(x = 그룹, y = ACME,
                        ymin = CI_lower, ymax = CI_upper,
                        color = 유의)) +
  # 가로 기준선 추가
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # 신뢰구간 막대 생성
  geom_errorbar(width = .15, linewidth = 1) +
  # 점 표시
  geom_point(size = 5) +
  # 텍스트 추가
  geom_text(aes(label = paste0("ACME = ", ACME, "\n", p값)),
            vjust = -1.2, size = 3.5, color = "grey30") +
  scale_color_manual(values = c("유의"   = "#378ADD",
                                "경계선" = "#EF9F27",
                                "비유의" = "#B4B2A9")) +
  # y축 범위 고정
  ylim(-1.2, 0.5) +
  labs(title    = "Step 5 | 매개효과(ACME) 비교",
       subtitle = "인프라 → 입원치료 → 응급실 내 사망률 간접효과 (Bootstrap 95% CI)",
       x = NULL, y = "간접효과 (ACME)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

# ACME (간접효과) : -0.393  p = 0.016 * 유의
#   인프라 → 입원치료 → 응급실 내 사망률 매개경로 존재
#   인프라↑ → 입원치료↑ → log(응급실 내 사망률)↓

# ADE  (직접효과) : -0.305  p = 0.438  비유의
#   인프라가 사망률에 미치는 직접 경로는 유의하지 않음

# Total Effect   : -0.698  p = 0.194  비유의
#   전체 효과는 유의하지 않음

# Prop. Mediated :  0.563  p = 0.206  비유의
#   간접효과 비율은 56.3%이나 통계적으로 유의하지 않음

# 결론
# ACME(간접효과)만 유의 → 매개경로 자체는 존재
# 단, 전체효과와 매개비율은 비유의
# → "인프라 → 입원치료 → 사망률" 간접 경로는 확인되나
#    전체적인 인프라의 사망률 영향은 불확실
# → 매개경로가 존재하지만 강도가 약하다

# 수도권 매개효과
med_model_metro <- lm(ami_heal ~ infra_hospital_per_area + infra_amb_per_area + region_old,
                      data = data_metro)
out_model_metro <- lm(log_ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
                        ami_heal + region_old,
                      data = data_metro)

set.seed(42)
med_metro <- mediate(med_model_metro, out_model_metro,
                     treat    = "infra_hospital_per_area",
                     mediator = "ami_heal",
                     boot     = TRUE, sims = 1000)
summary(med_metro)

# 비수도권 매개효과
med_model_nonmetro <- lm(ami_heal ~ infra_hospital_per_area + infra_amb_per_area + region_old,
                         data = data_nonmetro)
out_model_nonmetro <- lm(log_ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
                           ami_heal + region_old,
                         data = data_nonmetro)

set.seed(42)
med_nonmetro <- mediate(med_model_nonmetro, out_model_nonmetro,
                        treat    = "infra_hospital_per_area",
                        mediator = "ami_heal",
                        boot     = TRUE, sims = 1000)
summary(med_nonmetro)

# 전체 데이터에서는 매개경로가 유의하게 확인됐지만
# 층화 시 수도권/비수도권 각각에서는 유의하지 않음
# → 표본이 작아지면서 검정력(statistical power)이 낮아진 영향
# → 비수도권 p=0.094는 경계선으로 표본이 더 크다면 유의해질 가능성

# 결론:
# 매개 경로 자체는 전체 데이터에서 확인됨 (H0 기각)
# 수도권/비수도권 간 강도 차이는 통계적으로 확인되지 않음
# 단, 비수도권에서 매개 비율(49%)이 수도권(30%)보다 높아
# 실질적 차이 가능성은 존재

################################################################################
################################################################################
################################################################################
# 6단계 : 정책 시뮬레이션
# 인프라는 치료역량에 영향을 미치지 않는 것으로 결과가 도출되었기 때문에,
# 4단계에서 유의하게 나온 치료역량 -> 사망률 경로를 기반으로 시뮬레이션 진행

# ami_heal, ami_trans를 개선했을 때 사망률이 얼마나 감소하는지 예측

# 비수도권 시나리오 수정
# 원하는 시나리오 값으로 데이터 프레임 생성
sim_nonmetro <- data.frame(
  시나리오   = c("현재", "ami_heal +1%", "ami_heal +2%",
             "ami_trans -1%", "ami_trans -2%"),
  ami_amb    = mean(data_nonmetro$ami_amb),
  ami_heal   = c(mean(data_nonmetro$ami_heal),
                 mean(data_nonmetro$ami_heal) + 1,
                 mean(data_nonmetro$ami_heal) + 2,
                 mean(data_nonmetro$ami_heal),
                 mean(data_nonmetro$ami_heal)),
  ami_trans  = c(mean(data_nonmetro$ami_trans),
                 mean(data_nonmetro$ami_trans),
                 mean(data_nonmetro$ami_trans),
                 mean(data_nonmetro$ami_trans) - 1,
                 mean(data_nonmetro$ami_trans) - 2),
  region_old = mean(data_nonmetro$region_old)
)

# 수도권 시나리오 수정
sim_metro <- data.frame(
  시나리오   = c("현재", "ami_heal +1%", "ami_heal +2%",
             "ami_trans -1%", "ami_trans -2%"),
  ami_amb    = mean(data_metro$ami_amb),
  ami_heal   = c(mean(data_metro$ami_heal),
                 mean(data_metro$ami_heal) + 1,
                 mean(data_metro$ami_heal) + 2,
                 mean(data_metro$ami_heal),
                 mean(data_metro$ami_heal)),
  ami_trans  = c(mean(data_metro$ami_trans),
                 mean(data_metro$ami_trans),
                 mean(data_metro$ami_trans),
                 mean(data_metro$ami_trans) - 1,
                 mean(data_metro$ami_trans) - 2),
  region_old = mean(data_metro$region_old)
)

# 사망률 예측 후 역변환
# predict : 예측값 계산
# exp : 로그를 되돌리는 함수
sim_nonmetro$예측사망률 <- round(
  exp(predict(m_dead_nonmetro, newdata = sim_nonmetro)) - 1, 4)
sim_metro$예측사망률    <- round(
  exp(predict(m_dead_metro,    newdata = sim_metro)) - 1, 4)

# 두 그룹 합치기
sim_nonmetro$그룹 <- "비수도권"
sim_metro$그룹    <- "수도권"
# rbind : 행 기준으로 합치기
sim_all <- rbind(sim_nonmetro, sim_metro)

# 선그래프
# 입원치료 제공률 개선 시나리오
sim_all %>%
  filter(시나리오 %in% c("현재", "ami_heal +1%", "ami_heal +2%")) %>%
  mutate(시나리오 = factor(시나리오,
                       levels = c("현재", "ami_heal +1%", "ami_heal +2%"))) %>%
  ggplot(aes(x = 시나리오, y = 예측사망률,
             color = 그룹, group = 그룹)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(예측사망률, "%")),
            vjust = -1, size = 3.5) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  ylim(0, max(sim_all$예측사망률) * 1.15) +
  labs(title    = "Step 6-1 | 입원치료 제공률 개선 시뮬레이션",
       subtitle = "입원치료 제공률 개선 시 예측 사망률 변화",
       x = NULL, y = "예측 사망률 (%)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        axis.text.x     = element_text(size = 10))

# 전원율 시나리오
sim_all %>%
  filter(시나리오 %in% c("현재", "ami_trans -1%", "ami_trans -2%")) %>%
  mutate(시나리오 = factor(시나리오,
                       levels = c("현재", "ami_trans -1%", "ami_trans -2%"))) %>%
  ggplot(aes(x = 시나리오, y = 예측사망률,
             color = 그룹, group = 그룹)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(예측사망률, "%")),
            vjust = -1, size = 3.5) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  ylim(0, max(sim_all$예측사망률) * 1.15) +
  labs(title    = "Step 6-2 | 전원율 시뮬레이션",
       subtitle = "전원율 감소 시 예측 사망률 변화 — 억제 시 역효과 확인",
       x = NULL, y = "예측 사망률 (%)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        axis.text.x     = element_text(size = 10))

# ami_heal 개선 시
# +1%  → 비수도권 1.1165% → 0.9066% (약 18.8% 감소)
# +2%  → 비수도권 1.1165% → 0.7175% (약 35.7% 감소)
# → 입원치료 제공률 소폭 개선만으로도 응급실 내 사망률 감소 효과 확인

# ami_trans 개선 시 (전원율 감소)
# -1% → 오히려 사망률 증가 (1.1165% → 1.3458%)
# -2% → 더 증가 (1.60%)
# → 전원율을 억지로 낮추면 역효과
# → 전원은 필요한 의료적 판단 과정 — 낮추는 것이 목표가 아님

# 수도권 vs 비수도권
# 두 그룹 패턴 동일 → 치료역량 개선 효과는 지역 무관하게 일관됨


################################################################################
################################################################################
################################################################################

### 최종 정책 결론
# 1. 비수도권 응급의료 문제의 본질은 인프라 수 부족이 아닌 접근성(면적당) 부족
# 2. 인프라 증설만으로는 치료역량 개선으로 이어지지 않음
# 3. 사망률 감소를 위해서는 입원치료 제공 역량 강화가 최우선 과제
# 4. 전원율은 낮추는 것이 목표가 아닌 적절한 전원 체계 구축이 필요
# 5. 비수도권은 인프라 배치 최적화 + 치료역량 강화 동시 추진 필요

### 한계점
# 1. 핵심 변수 부재
#    응급의료에서 중요한 변수(골든타임, 도착 전 사망, 이송 시간 등)는
#    자료 접근의 한계 및 시도별 데이터로만 제공되어 표본이 적어 분석에 포함 불가
#    → 사망률 설명력(R²)이 낮은 주요 원인

# 2. 설명력 한계
#    Step 3 R² = 0.037 ~ 0.049 → 인프라 → 치료역량 설명력 낮음
#    Step 4 R² = 0.252 ~ 0.318 → 치료역량 → 사망률 설명력 양호
#    → 인프라 변수의 설명력 한계가 존재

# 3. 횡단면 데이터의 한계
#    1개 연도 데이터만 사용 → 인과관계 주장에 한계
#    시계열/패널 데이터였다면 더 강한 인과 추론 가능

# 4. 인프라 → 치료역량 경로 미확인
#    Step 3에서 인프라 변수가 치료역량에 유의한 영향을 미치지 못함
#    → 인프라 외 운영적 요인(인력 배치, 프로토콜 등)이 더 중요할 수 있으나
#       해당 변수 데이터 부재

# 5. 전원율(ami_trans) 해석의 모호성
#    전원율이 높다고 나쁜 것인지, 낮다고 좋은 것인지 판단 어려움
#    중증도 보정 없이는 전원율의 방향성 해석에 한계

# 6. 수도권/비수도권 이분법의 단순화
#    수도권 내에서도 지역 간 차이 존재 (서울 vs 경기 외곽)
#    비수도권도 광역시와 농촌 지역을 동일하게 취급 → 세부 격차 반영 못함

####################
### 추가 시각화

# "단순히 비수도권이 취약한 게 아니라, 가장 응급의료가 필요한 사람들이 사는 곳에 인프라가 가장 없다"
# 고령화율 높음 → 심근경색 등 응급상황 발생 가능성 높음
# +
#   면적당 인프라 낮음 → 응급상황 발생 시 인프라 도달 시간 김
# ↓
# 이중으로 취약한 지역 = 가장 위험한 지역

# 단순히 전국에 인프라를 고르게 늘리거나 비수도권에 막연하게 늘리는게 아니라,
# 이중 취약 지역에 선택적으로 집중 투자해야 한다는 근거
# 어디에 먼저 투자해야 하는가?에 대한 시각화

# 고령화율 상위 25% + 인프라 하위 25% → 이중 취약 지역
map_joined <- map_joined %>%
  mutate(
    이중취약 = case_when(
      region_old >= quantile(region_old, 0.75, na.rm = TRUE) &
        infra_hospital_per_area <= quantile(infra_hospital_per_area, 0.25, na.rm = TRUE) ~ "이중 취약 지역",
      TRUE ~ "일반 지역"
    )
  )

# 이중 취약 지역 지도
ggplot(map_joined) +
  geom_sf(aes(fill = 이중취약), color = "white", linewidth = 0.01) +
  geom_sf(data = map_metro,
          fill = NA, color = "#FF4444", linewidth = 0.05) +
  scale_fill_manual(
    values = c("이중 취약 지역" = "#7F1D1D", "일반 지역" = "#DBEAFE"),
    na.value = "grey90",
    name = NULL
  ) +
  labs(title    = "고령화율 높고 인프라 접근성 낮은 이중 취약 지역",
       subtitle = "고령화율 상위 25% + 면적당 병원 수 하위 25% / 빨간선: 수도권") +
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle   = element_text(color = "grey50", size = 10, hjust = 0.5),
        legend.position = "right")

# 우선순위 투자 지역 분류
map_joined <- map_joined %>%
  mutate(
    우선순위 = case_when(
      이중취약 == "이중 취약 지역" & ami_dead >= median(ami_dead, na.rm = TRUE) ~ "최우선 투자 지역",
      이중취약 == "이중 취약 지역" & ami_dead < median(ami_dead, na.rm = TRUE)  ~ "우선 투자 지역",
      TRUE ~ "일반 지역"
    )
  )

# 우선순위 투자 지역 지도
ggplot(map_joined) +
  geom_sf(aes(fill = 우선순위), color = "white", linewidth = 0.05) +
  geom_sf(data = map_metro,
          fill = NA, color = "#FF4444", linewidth = 0.05) +
  scale_fill_manual(
    values = c("최우선 투자 지역" = "#7F1D1D",
               "우선 투자 지역"   = "#FCA5A5",
               "일반 지역"        = "#DBEAFE"),
    na.value = "grey90",
    name = NULL
  ) +
  labs(title    = "정책 제언 | 응급의료 인프라 투자 우선순위 지역",
       subtitle = "진한 빨강: 최우선 (이중취약 + 사망률 높음) / 연한 빨강: 우선 (이중취약) / 빨간선: 수도권") +
  theme_void() +
  theme(plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle   = element_text(color = "grey50", size = 9, hjust = 0.5),
        legend.position = "right")

# 목록 확인
map_joined %>%
  # sf 객체에서 지도 정보 제거 => 일반 데이터프레임으로 변환
  st_drop_geometry() %>%
  filter(이중취약 == "이중 취약 지역") %>%
  dplyr::select(sido_nm, SIG_KOR_NM, region_old,
                infra_hospital_per_area, ami_dead) %>%
  # 정렬 함수
  arrange(desc(ami_dead)) %>%
  # 컬럼 이름 변경
  rename(시도 = sido_nm, 시군구 = SIG_KOR_NM,
         고령화율 = region_old,
         병원접근성 = infra_hospital_per_area,
         사망률 = ami_dead)
