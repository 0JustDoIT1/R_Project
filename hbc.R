getwd()
setwd("D:/Project/AI_class/R_Project_data")

# 사용라이브러리 모음
library(tidyverse)
library(coin)     # wilcox_test()
library(effsize)  # cohen.d()
library(ggplot2)
library(patchwork)
library(car)
library(mediation)
library(broom)

# 전처리된 데이터 불러오기
data <- read.csv("final_data.csv", fileEncoding = "CP949")
str(data)
head(data)

# metro 변수 범주형 데이터로 변환
data$metro <- as.factor(data$metro)
str(data)

################################################################################
################################################################################
################################################################################
# 1단계 : 인프라 격차 검증
# H0 : 수도권과 비수도권 응급기관 수 차이 없음
# H1 : 수도권 응급기관 수가 더 많음

# 인구 기준 / 면적 기준 각각 진행
# 1-1. 인구 기준

# EDA (탐색적 분석)
summary(data[, c("infra_hospital_per100k", "infra_amb_per100k")])

par(mfrow = c(2,2))
hist(data$infra_hospital_per100k, main="병원 수 (per100k)", col = "steelblue")
hist(data$infra_amb_per100k, main="구급차 수 (per100k)", col = "steelblue")
boxplot(data$infra_amb_per100k, main="병원 수 (per100k)")
boxplot(data$infra_amb_per100k, main="구급차 수 (per100k)")
par(mfrow = c(1,1))

# 정규성 검정
shapiro.test(data$infra_hospital_per100k) # p-value < 2.2e-16 : 정규분포 아님
shapiro.test(data$infra_amb_per100k) # p-value = 9.019e-15 : 정규분포 아님

# 수도권/비수도권 분포 확인
par(mfrow = c(1, 2))
boxplot(infra_hospital_per100k ~ metro, data = data,
        main = "병원 수 (per100k)", col = c("#1D9E75", "#378ADD"))
boxplot(infra_amb_per100k ~ metro, data = data,
        main = "구급차 수 (per100k)", col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))

# Wilcoxon test (정규분포 아니므로 t-test 대신 wilcoxon test)
wilcox.test(infra_hospital_per100k ~ metro, data = data)
  # p-value = 2.375e-09
  # 귀무가설 기각 : 수도권과 비수도권의 병원 수(인구당) 차이가 유의함
wilcox.test(infra_amb_per100k ~ metro, data = data)
  # p-value = 1.252e-15
  # 귀무가설 기각 : 수도권과 비수도권의 구급차 수(인구당) 차이가 유의함

# Cohen's d — 효과크기
# 그룹 순서 확인
levels(data$metro) # [1] "비수도권" "수도권"
cohen.d(infra_hospital_per100k ~ metro, data = data)
  # d estimate: 0.7098288 (medium) : 비수도권 - 수도권
  # 비수도권 > 수도권
cohen.d(infra_amb_per100k ~ metro, data = data)
  # d estimate: 0.9778843 (large) : 수도권 - 비수도권
  # 비수도권 > 수도권

# 그룹별 중앙값 확인 (Wilcoxon은 중앙값 기준)
# 실제 차이 값 비교를 위해서 진행
data %>%
  group_by(metro) %>%
  summarise(
    hospital_median = round(median(infra_hospital_per100k), 4),
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
# 면적당 수치 중앙값 기준으로
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
# 상관분석에서 Pearson 대신 Spearman 사용
# Pearson : 두 변수가 선형 관계로 가정 (정규분포에 사용), 이상치에 민감
# Spearman : 순위 기반, 선형 관계 아니어도 가능 (정규분포 아니어도 됨), 이상치에 강건

# cor.test.default(data$region_old, data$infra_hospital_per_area, 에서:
# tie때문에 정확한 p값을 계산할 수 없습니다
# 표본이 250개로 충분히 크기 때문에 exat=FALSE로 근사 p값을 사용해도 신뢰 가능

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
  # p-value = 0.3121, rho = -0.064 (거의 없음)
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
# 이는 예상과는 다른 결과로 존재하는 데이터 자체가 설명력이 부족하거나,
# 전체 데이터를 통해 판단했기 때문일 수도 있음
# 이를 판단하기 위해 수도권/비수도권으로 층화해서 각각 진행

# 데이터 분리
data_metro    <- data %>% filter(metro == "수도권")
data_nonmetro <- data %>% filter(metro == "비수도권")

# 수도권 회귀
m_amb_metro    <- lm(ami_amb   ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data_metro)
m_heal_metro   <- lm(ami_heal  ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data_metro)
m_trans_metro  <- lm(ami_trans ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data_metro)

summary(m_amb_metro)
summary(m_heal_metro)
summary(m_trans_metro)

# 비수도권 회귀
m_amb_nonmetro   <- lm(ami_amb   ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data_nonmetro)
m_heal_nonmetro  <- lm(ami_heal  ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data_nonmetro)
m_trans_nonmetro <- lm(ami_trans ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = data_nonmetro)

summary(m_amb_nonmetro)
summary(m_heal_nonmetro)
summary(m_trans_nonmetro)

# 구급차 이용률
# 수도권: 병원 수↑ → 구급차 이용률↓, 구급차 수↑ → 구급차 이용률↑ (유의)
# 비수도권: 인프라 변수 모두 비유의, 고령화율만 유의

# 입원치료 제공률
# 수도권/비수도권 모두 모든 변수 비유의
# 입원치료 제공률은 인프라로 설명이 안 됨

# 전원율
# 수도권/비수도권 모두 비유의
# 전원율도 인프라로 설명이 안 됨

# 세 모델 계수 추출
coef_all <- bind_rows(
  tidy(m_amb,   conf.int = TRUE) %>% mutate(model = "구급차 이용률"),
  tidy(m_heal,  conf.int = TRUE) %>% mutate(model = "입원치료 제공률"),
  tidy(m_trans, conf.int = TRUE) %>% mutate(model = "전원율")
) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = dplyr::recode(term,
                       infra_hospital_per_area = "병원(면적당)",
                       infra_amb_per_area      = "구급차(면적당)",
                       region_old              = "고령화율"),
         유의 = ifelse(p.value < 0.05, "유의 (p<.05)", "비유의"))

# 계수 플롯
ggplot(coef_all, aes(x = estimate, y = term, color = 유의,
                     xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(height = .25, linewidth = .8) +
  geom_point(size = 4) +
  facet_wrap(~model, ncol = 3) +
  scale_color_manual(values = c("유의 (p<.05)" = "#378ADD",
                                "비유의"        = "#B4B2A9")) +
  labs(title    = "Step 3 | 인프라 → 치료역량 회귀계수",
       subtitle = "전체 모델 — 점: 계수 / 선: 95% CI / 점선: 0 기준",
       x = "회귀계수 (Estimate)", y = NULL, color = NULL) +
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

# 정규성 검정
shapiro.test(data$ami_dead) # p-value = 2.949e-07 : 정규분포 아님

# 회귀 모델 수립
m_dead <- lm(ami_dead ~ ami_amb + ami_heal + ami_trans + region_old, data = data)
summary(m_dead)

#              Estimate Std. Error t value Pr(>|t|)
# ami_amb      0.008279   0.023219   0.357 0.721724    
# ami_heal    -0.343845   0.083084  -4.139 4.81e-05 ***
# ami_trans   -0.368731   0.101706  -3.625 0.000351 ***
# region_old  -0.005420   0.026545  -0.204 0.838378

# Multiple R-squared:  0.06022,	Adjusted R-squared:  0.03785

# 전체 모델 계수 추출
coef_dead <- tidy(m_dead, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = case_when(
    term == "ami_amb"    ~ "구급차 이용률",
    term == "ami_heal"   ~ "입원치료 제공률",
    term == "ami_trans"  ~ "전원율",
    term == "region_old" ~ "고령화율"
  ),
  유의 = ifelse(p.value < 0.05, "유의 (p<.05)", "비유의"))

# 계수 플롯
ggplot(coef_dead, aes(x = estimate, y = term, color = 유의,
                      xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(height = .25, linewidth = .8) +
  geom_point(size = 4) +
  scale_color_manual(values = c("유의 (p<.05)" = "#378ADD",
                                "비유의"        = "#B4B2A9")) +
  labs(title    = "Step 4 | 치료역량 → 사망률 회귀계수",
       subtitle = "전체 모델 — 점: 계수 / 선: 95% CI / 점선: 0 기준",
       x = "회귀계수 (Estimate)", y = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

### 한계점
# "본 연구의 데이터는 치료역량·인프라 변수만 포함하여 설명력에 한계가 있으나,
# 입원치료 제공률과 전원율은 사망률에 유의한 영향을 미치는 것으로 확인됐다"

### 결론
# ami_amb   (구급차 이용률)   : p = 0.721724  비유의 → 사망률과 관계 없음
# ami_heal  (입원치료 제공률) : p = 4.81e-05  유의   → 입원치료↑ 사망률↓ (Estimate = -0.344)
# ami_trans (전원율)          : p = 0.000351  유의   → 전원율↑  사망률↓ (Estimate = -0.369)
# region_old(고령화율)        : p = 0.838378  비유의 → 사망률과 관계 없음

# R² = 0.066 → 설명력 낮음 (데이터 한계 — 중증도, 의료진 수 등 변수 부재)

# 핵심 결론
# 치료역량 중 입원치료 제공률, 전원율이 사망률에 유의한 영향을 미침
# 단, R²가 낮아 모델 설명력에 한계가 있음을 명시 필요

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
out_model <- lm(ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
                  ami_heal + region_old,
                data = data)

# Bootstrap 매개효과 검정
set.seed(42)
med_result <- mediate(med_model, out_model,
                      treat    = "infra_hospital_per_area",
                      mediator = "ami_heal",
                      boot     = TRUE,
                      sims     = 1000)
summary(med_result)

# ACME (간접효과)  : -0.951  p = 0.184  비유의 → 매개효과 없음
# ADE  (직접효과)  : -0.951  p = 0.724  비유의 → 직접효과도 없음
# Total Effect     : -1.902  p = 0.586  비유의 → 전체효과도 없음
# Prop. Mediated   :  0.500  p = 0.686  비유의 → 매개 비율 의미 없음

# 95% CI가 모두 0을 포함 → 통계적으로 유의하지 않음
# → 귀무가설 기각 실패 : 매개효과 확인되지 않음

################################################################################
################################################################################
################################################################################
# 6단계 : 정책 시뮬레이션
# 인프라는 치료역량에 영향을 미치지 않는 것으로 결과가 도출되었기 때문에,
# 4단계에서 유의하게 나온 치료역량 -> 사망률 경로를 기반으로 시뮬레이션 진행

# ami_heal, ami_trans를 개선했을 때 사망률이 얼마나 감소하는지 예측

# 비수도권 시나리오
sim_nonmetro <- data.frame(
  시나리오   = c("현재", "ami_heal +5%", "ami_heal +10%",
             "ami_trans -1%", "ami_trans -2%"),
  ami_amb    = mean(data_nonmetro$ami_amb),
  ami_heal   = c(mean(data_nonmetro$ami_heal),
                 mean(data_nonmetro$ami_heal) + 5,
                 mean(data_nonmetro$ami_heal) + 10,
                 mean(data_nonmetro$ami_heal),
                 mean(data_nonmetro$ami_heal)),
  ami_trans  = c(mean(data_nonmetro$ami_trans),
                 mean(data_nonmetro$ami_trans),
                 mean(data_nonmetro$ami_trans),
                 mean(data_nonmetro$ami_trans) - 1,
                 mean(data_nonmetro$ami_trans) - 2),
  region_old = mean(data_nonmetro$region_old)
)

# 수도권 시나리오
sim_metro <- data.frame(
  시나리오   = c("현재", "ami_heal +5%", "ami_heal +10%",
             "ami_trans -1%", "ami_trans -2%"),
  ami_amb    = mean(data_metro$ami_amb),
  ami_heal   = c(mean(data_metro$ami_heal),
                 mean(data_metro$ami_heal) + 5,
                 mean(data_metro$ami_heal) + 10,
                 mean(data_metro$ami_heal),
                 mean(data_metro$ami_heal)),
  ami_trans  = c(mean(data_metro$ami_trans),
                 mean(data_metro$ami_trans),
                 mean(data_metro$ami_trans),
                 mean(data_metro$ami_trans) - 1,
                 mean(data_metro$ami_trans) - 2),
  region_old = mean(data_metro$region_old)
)

# 사망률 예측
sim_nonmetro$예측사망률 <- round(predict(m_dead_nonmetro, newdata = sim_nonmetro), 4)
sim_metro$예측사망률    <- round(predict(m_dead_metro,    newdata = sim_metro), 4)

# 두 그룹 합치기
sim_nonmetro$그룹 <- "비수도권"
sim_metro$그룹    <- "수도권"
sim_all <- rbind(sim_nonmetro, sim_metro)

# 시나리오 순서 고정
sim_all$시나리오 <- factor(sim_all$시나리오,
                       levels = c("현재", "ami_heal +5%", "ami_heal +10%",
                                  "ami_trans -1%", "ami_trans -2%"))

# 선그래프
ggplot(sim_all, aes(x = 시나리오, y = 예측사망률,
                    color = 그룹, group = 그룹)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(예측사망률, "%")),
            vjust = -1, size = 3.5) +
  scale_color_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  ylim(0, max(sim_all$예측사망률) * 1.15) +
  labs(title    = "Step 6 | 치료역량 개선 시뮬레이션",
       subtitle = "수도권 vs 비수도권 — 시나리오별 예측 사망률 변화",
       x = NULL, y = "예측 사망률 (%)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        axis.text.x     = element_text(size = 10))

### 결론
### 입원치료 제공률을 높일수록 사망률이 선형으로 크게 감소 : 가장 효과적
### 전원율을 낮추면 오히려 사망률이 상승
### 전원이 단순히 나쁜 게 아니라 중증 환자를 적절한 병원으로 보내는 필요한 과정

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

# 2. 낮은 설명력 (R²)
#    Step 3 R² = 0.037 ~ 0.049 / Step 4 R² = 0.060 ~ 0.152
#    인프라·치료역량 변수만으로는 사망률 변동을 충분히 설명하지 못함
#    → 의료진 수, 장비 수준, 환자 중증도 등 추가 변수 필요

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