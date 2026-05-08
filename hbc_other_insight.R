# 새로운 방향 3. "고령화가 심한 지역일수록 접근성이 낮다"
# 핵심 주제:
#   고령화율이 높은 지역 = 인프라 접근성이 낮은 지역
# → 가장 응급의료가 필요한 인구가 가장 접근하기 어려운 곳에 있다
# 
# 분석 구조:
#   고령화율 ↔ 면적당 인프라 (Step 2에서 이미 유의 확인)
# 고령화율 높은 지역의 치료역량과 사망률은?
#   
#   시뮬레이션:
#   고령화율 상위 지역(예: 상위 25%)에
# 인프라 또는 치료역량을 집중 투입했을 때
# 전체 사망률이 얼마나 감소하는가?
#   → "선택적 집중 투자" 정책 근거 도출

# Step 1. 고령화율 기준 지역 분류
# 고령화율 상위 25% (취약지역) vs 하위 25% (양호지역)
# 
# Step 2. 취약지역의 인프라 접근성 확인
# 고령화 취약지역 = 면적당 인프라가 낮은가?
#   (Wilcoxon + Cohen's d)
# 
# Step 3. 취약지역의 치료역량 및 사망률 확인
#          고령화 취약지역 = 입원치료↓, 사망률↑ 인가?
# 
# Step 4. 인프라·치료역량 → 사망률 회귀
#          취약지역 vs 양호지역 층화 회귀
#          어느 변수가 사망률에 더 강하게 영향을 미치는가?
# 
# Step 5. 정책 시뮬레이션
#          시나리오 A: 취약지역 면적당 인프라를 양호지역 수준으로
#          시나리오 B: 취약지역 입원치료 제공률을 양호지역 수준으로
#          시나리오 C: A + B 동시 개선
#          → 사망률이 얼마나 감소하는가?

library(tidyverse)
library(ggplot2)
library(patchwork)
library(car)
library(broom)
library(effsize)

getwd()
setwd("D:/Project/AI_class/R_Project_data")

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
# 1단계 : 고령화율 기준 지역 분류

# 기술통계
summary(data$region_old)

# EDA (탐색적 분석)
# 분포 확인
par(mfrow = c(1, 2))
hist(data$region_old, main = "고령화율 분포", col = "steelblue")
boxplot(region_old ~ metro, data = data,
        main = "고령화율 (수도권 vs 비수도권)",
        col = c("#1D9E75", "#378ADD"))
par(mfrow = c(1, 1))


# 고령화율 상위 25% / 하위 25% 분류
quantile(data$region_old, c(0.25, 0.75))
  # 고령화율 기준점
  # 하위 25% 기준: 18.2% 이하 → 양호지역
  # 상위 25% 기준: 33.4% 이상 → 취약지역

data <- data %>%
  mutate(old_group = case_when(
    region_old >= quantile(region_old, 0.75) ~ "고령화 취약지역 (상위 25%)",
    region_old <= quantile(region_old, 0.25) ~ "고령화 양호지역 (하위 25%)",
    TRUE ~ "중간"
  ))

# 분포 확인
table(data$old_group)

# 수도권/비수도권과 교차
table(data$old_group, data$metro)

# 결과 시각화
# 고령화율 분포 + 그룹 기준선
ggplot(data, aes(x = region_old, fill = old_group)) +
  geom_histogram(bins = 25, alpha = .8, color = "white", linewidth = .3) +
  geom_vline(xintercept = 18.2, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = 33.4, linetype = "dashed", color = "grey30") +
  annotate("text", x = 18.2, y = Inf, label = "하위 25%\n(18.2%)",
           vjust = 1.5, hjust = 1.2, size = 3.5, color = "grey30") +
  annotate("text", x = 33.4, y = Inf, label = "상위 25%\n(33.4%)",
           vjust = 1.5, hjust = -.2, size = 3.5, color = "grey30") +
  scale_fill_manual(values = c(
    "고령화 취약지역 (상위 25%)" = "#993C1D",
    "고령화 양호지역 (하위 25%)" = "#378ADD",
    "중간"                       = "#B4B2A9"
  )) +
  labs(title    = "Step 1 | 고령화율 분포 및 그룹 분류",
       subtitle = "상위 25% = 취약지역 / 하위 25% = 양호지역",
       x = "고령화율 (%)", y = "count", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

# 수도권/비수도권 교차 시각화
data %>%
  filter(old_group != "중간") %>%
  count(old_group, metro) %>%
  group_by(old_group) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = old_group, y = pct, fill = metro)) +
  geom_col(width = .5, alpha = .85) +
  geom_text(aes(label = paste0(pct, "%\n(n=", n, ")")),
            position = position_stack(vjust = .5),
            size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "Step 1 | 고령화 그룹별 수도권/비수도권 구성",
       subtitle = "취약지역의 97%가 비수도권",
       x = NULL, y = "비율 (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

### 결론
### 고령화 취약지역 (상위 25%, n=64)
###  비수도권 62개 (97%) / 수도권 2개 (3%)
###   → 취약지역 거의 대부분이 비수도권

### 고령화 양호지역 (하위 25%, n=65)
###   비수도권 26개 (40%) / 수도권 39개 (60%)
###   → 양호지역은 수도권이 더 많음

### 고령화 취약지역의 97%가 비수도권 → 응급의료 수요가 가장 높은 인구가 비수도권에 집중되어 있음

################################################################################
################################################################################
################################################################################
# 2단계 : 취약지역 인프라 접근성 확인
# H0: 고령화 취약지역과 양호지역의 면적당 인프라 수준 차이 없다
# H1: 고령화 취약지역의 면적당 인프라가 양호지역보다 낮다

# EDA
# 기술통계
data %>%
  filter(old_group != "중간") %>%
  group_by(old_group) %>%
  summarise(
    hospital_median = round(median(infra_hospital_per_area), 4),
    hospital_mean   = round(mean(infra_hospital_per_area), 4),
    amb_median      = round(median(infra_amb_per_area), 4),
    amb_mean        = round(mean(infra_amb_per_area), 4)
  )
# old_group                  hospital_median hospital_mean amb_median amb_mean
# <chr>                                <dbl>         <dbl>      <dbl>    <dbl>
# 1 고령화 양호지역 (하위 25%)          0.0098        0.0254     0.0406   0.0736
# 2 고령화 취약지역 (상위 25%)          0.0016        0.0037     0.0075   0.0081

# 병원(면적당) : 양호 지역이 6.1배 높음 / 구급차(면적당) : 양호지역이 5.4배 높음

# 분포 확인
par(mfrow = c(1, 2))
boxplot(infra_hospital_per_area ~ old_group,
        data = data %>% filter(old_group != "중간"),
        main = "병원 수 (면적당)", col = c("#378ADD", "#993C1D"))
boxplot(infra_amb_per_area ~ old_group,
        data = data %>% filter(old_group != "중간"),
        main = "구급차 수 (면적당)", col = c("#378ADD", "#993C1D"))
par(mfrow = c(1, 1))

# 분석용 데이터 (중간 제외)
data_old <- data %>% filter(old_group != "중간")

# Wilcoxon test
wilcox.test(infra_hospital_per_area ~ old_group, data = data_old)
  # p-value = 2.264e-11
  # 귀무가설 기각 : 고령화 취약지역과 양호지역의 면적당 인프라 수준 차이가 있다 
wilcox.test(infra_amb_per_area ~ old_group, data = data_old)
  # p-value = 2.219e-13
  # 귀무가설 기각 : 고령화 취약지역과 양호지역의 면적당 인프라 수준 차이가 있다

# Cohen's d
cohen.d(infra_hospital_per_area ~ old_group, data = data_old)
  # d estimate: 0.7643867 (medium) : 양호지역 > 취약지역
cohen.d(infra_amb_per_area ~ old_group, data = data_old)
  # d estimate: 1.190632 (large) : 양호지역 > 취약지역

# 계수 비교 데이터 준비
coef_step2 <- data.frame(
  variable = c("병원(면적당)", "구급차(면적당)"),
  d        = c(0.764, 1.191),
  p        = c("p<0.001***", "p<0.001***"),
  양호지역  = c(0.0098, 0.0406),
  취약지역  = c(0.0016, 0.0075)
)

# 중앙값 비교 막대그래프
data_old %>%
  group_by(old_group) %>%
  summarise(
    `병원(면적당)`  = median(infra_hospital_per_area),
    `구급차(면적당)` = median(infra_amb_per_area)
  ) %>%
  pivot_longer(-old_group, names_to = "변수", values_to = "중앙값") %>%
  ggplot(aes(x = 변수, y = 중앙값, fill = old_group)) +
  geom_col(position = "dodge", width = .5, alpha = .85) +
  geom_text(aes(label = round(중앙값, 4)),
            position = position_dodge(.5), vjust = -.5,
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c(
    "고령화 양호지역 (하위 25%)" = "#378ADD",
    "고령화 취약지역 (상위 25%)" = "#993C1D"
  )) +
  labs(title    = "Step 2 | 고령화 그룹별 면적당 인프라 비교",
       subtitle = "중앙값 기준 / 취약지역이 양호지역 대비 병원 6.1배 · 구급차 5.4배 낮음",
       x = NULL, y = "중앙값", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"))

### 결론
### 고령화 취약지역은 양호지역 대비 면적당 인프라가 현저히 낮음
### 병원 : medium 수준 격차 / 구급차 : large 수준 격차
### → "가장 응급의료가 필요한 지역에 인프라가 가장 부족하다" 통계적으로 확인

################################################################################
################################################################################
################################################################################
# 3단계 : 취약지역 치료역량 및 사망률 확인
# H0: 고령화 취약지역과 양호지역의 치료역량·사망률 차이 없다
# H1: 고령화 취약지역은 양호지역보다 치료역량 낮고 사망률 높다

# 기술통계
data_old %>%
  group_by(old_group) %>%
  summarise(
    amb_median   = round(median(ami_amb),   4),
    heal_median  = round(median(ami_heal),  4),
    trans_median = round(median(ami_trans), 4),
    dead_median  = round(median(ami_dead),  4)
  )

# 분포 확인
par(mfrow = c(2, 2))
boxplot(ami_amb   ~ old_group, data = data_old,
        main = "구급차 이용률",   col = c("#378ADD", "#993C1D"))
boxplot(ami_heal  ~ old_group, data = data_old,
        main = "입원치료 제공률", col = c("#378ADD", "#993C1D"))
boxplot(ami_trans ~ old_group, data = data_old,
        main = "전원율",          col = c("#378ADD", "#993C1D"))
boxplot(ami_dead  ~ old_group, data = data_old,
        main = "사망률",          col = c("#378ADD", "#993C1D"))
par(mfrow = c(1, 1))

# Wilcoxon test
wilcox.test(ami_amb   ~ old_group, data = data_old)
wilcox.test(ami_heal  ~ old_group, data = data_old)
wilcox.test(ami_trans ~ old_group, data = data_old)
wilcox.test(ami_dead  ~ old_group, data = data_old)

# Cohen's d
cohen.d(ami_amb   ~ old_group, data = data_old)
cohen.d(ami_heal  ~ old_group, data = data_old)
cohen.d(ami_trans ~ old_group, data = data_old)
cohen.d(ami_dead  ~ old_group, data = data_old)

# 구급차 이용률 (ami_amb)
# p = 0.060 / 경계선 / d = -0.414 (small) → 취약지역 > 양호지역
# → 유의하지 않지만 취약지역에서 구급차 이용률이 높은 경향

# 입원치료 제공률 (ami_heal)
# p = 0.284 / 비유의 / d = +0.288 (small) → 양호지역 > 취약지역
# → 두 그룹 간 입원치료 제공률 차이 없음

# 전원율 (ami_trans)
# p = 0.048 / 유의 / d = -0.431 (small) → 취약지역 > 양호지역
# → 취약지역에서 전원율이 유의미하게 높음

# 사망률 (ami_dead)
# p = 0.366 / 비유의 / d = +0.036 (negligible) → 거의 차이 없음
# → 두 그룹 간 사망률 차이 없음

# H0 대부분 채택
# 고령화 취약지역이 인프라 접근성은 현저히 낮지만
# 치료역량과 사망률은 양호지역과 큰 차이가 없음
# → 인프라 격차가 치료역량·사망률 격차로 이어지지 않는 패턴
#    (기존 분석 Step 3 결과와 동일한 흐름)
# → 단, 전원율은 취약지역이 유의하게 높음
#    인프라 부족 → 자체 처리 한계 → 전원 증가로 해석 가능

# 치료역량 + 사망률 그룹별 중앙값 비교
data_old %>%
  group_by(old_group) %>%
  summarise(
    `구급차 이용률`   = median(ami_amb),
    `입원치료 제공률` = median(ami_heal),
    `전원율`          = median(ami_trans),
    `사망률`          = median(ami_dead)
  ) %>%
  pivot_longer(-old_group, names_to = "변수", values_to = "중앙값") %>%
  mutate(변수 = factor(변수, levels = c("구급차 이용률", "입원치료 제공률",
                                    "전원율", "사망률"))) %>%
  ggplot(aes(x = old_group, y = 중앙값, fill = old_group)) +
  geom_col(width = .5, alpha = .85) +
  geom_text(aes(label = round(중앙값, 2)),
            vjust = -.5, size = 3.5, fontface = "bold") +
  facet_wrap(~변수, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(
    "고령화 양호지역 (하위 25%)" = "#378ADD",
    "고령화 취약지역 (상위 25%)" = "#993C1D"
  )) +
  labs(title    = "Step 3 | 고령화 그룹별 치료역량 및 사망률 비교",
       subtitle = "중앙값 기준 / 유의: 전원율(p=0.048) / 경계선: 구급차 이용률(p=0.060)",
       x = NULL, y = "중앙값", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position  = "bottom",
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(color = "grey50"),
        strip.text       = element_text(face = "bold"),
        axis.text.x      = element_blank())

################################################################################
################################################################################
################################################################################
# 4단계 : 인프라/치료역량 → 사망률 회귀

# log 변환
data <- data %>%
  mutate(log_ami_dead = log(ami_dead + 1))

data_old <- data %>% filter(old_group != "중간")

# 양호지역 / 취약지역 분리
data_good <- data_old %>% filter(old_group == "고령화 양호지역 (하위 25%)")
data_vul  <- data_old %>% filter(old_group == "고령화 취약지역 (상위 25%)")

# 양호지역 회귀
m_good <- lm(log_ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
               ami_amb + ami_heal + ami_trans + region_old,
             data = data_good)
summary(m_good)

# 취약지역 회귀
m_vul <- lm(log_ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
              ami_amb + ami_heal + ami_trans + region_old,
            data = data_vul)
summary(m_vul)

# 치료역량 vs log 사망률 산점도 + 추세선
data_old %>%
  dplyr::select(log_ami_dead, ami_amb, ami_heal, ami_trans, old_group) %>%
  pivot_longer(cols = c(ami_amb, ami_heal, ami_trans),
               names_to = "변수", values_to = "값") %>%
  mutate(변수 = case_when(
    변수 == "ami_amb"   ~ "구급차 이용률",
    변수 == "ami_heal"  ~ "입원치료 제공률",
    변수 == "ami_trans" ~ "전원율"
  )) %>%
  ggplot(aes(x = 값, y = log_ami_dead, color = old_group)) +
  geom_point(alpha = .4, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~변수, scales = "free_x", ncol = 3) +
  scale_color_manual(values = c(
    "고령화 양호지역 (하위 25%)" = "#378ADD",
    "고령화 취약지역 (상위 25%)" = "#993C1D"
  )) +
  labs(title    = "Step 4 | 치료역량 vs 응급실 내 사망률",
       subtitle = "양호지역 vs 취약지역 — 선형 추세선 포함",
       x = NULL, y = "log(응급실 내 사망률 + 1)", color = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold", size = 13),
        plot.subtitle   = element_text(color = "grey50"),
        strip.text      = element_text(face = "bold"))

# 양호지역 (n=65)
# infra_hospital_per_area : p = 0.267  비유의 → 인프라가 사망률에 영향 없음
# infra_amb_per_area      : p = 0.707  비유의 → 인프라가 사망률에 영향 없음
# ami_amb   (구급차 이용률)   : p = 0.989  비유의 → 사망률과 관계 없음
# ami_heal  (입원치료 제공률) : p < 0.001  유의   → 입원치료↑ 사망률↓ (Estimate = -0.099)
# ami_trans (전원율)          : p = 0.007  유의   → 전원율↑  사망률↓ (Estimate = -0.091)
# region_old(고령화율)        : p = 0.848  비유의 → 사망률과 관계 없음
# R² = 0.217

# 취약지역 (n=64)
# infra_hospital_per_area : p = 0.611  비유의 → 인프라가 사망률에 영향 없음
# infra_amb_per_area      : p = 0.980  비유의 → 인프라가 사망률에 영향 없음
# ami_amb   (구급차 이용률)   : p = 0.878  비유의 → 사망률과 관계 없음
# ami_heal  (입원치료 제공률) : p < 0.001  유의   → 입원치료↑ 사망률↓ (Estimate = -0.113)
# ami_trans (전원율)          : p < 0.001  유의   → 전원율↑  사망률↓ (Estimate = -0.117)
# region_old(고령화율)        : p = 0.713  비유의 → 사망률과 관계 없음
# R² = 0.247

# 양호지역/취약지역 모두 인프라 변수는 비유의
# 두 그룹 모두 입원치료 제공률, 전원율이 사망률에 유의한 영향
# 취약지역에서 계수가 더 큼 (-0.113 vs -0.099 / -0.117 vs -0.091)
# → 취약지역에서 치료역량 개선의 효과가 더 강하게 나타남
# → 즉, 취약지역에서 입원치료와 전원율을 개선하면
#    양호지역보다 사망률 감소 효과가 더 크다

################################################################################
################################################################################
################################################################################
# 5단계 : 정책 시뮬레이션
# 목적 : 4단계에서 취약지역의 치료역량 계수가 양호지역보다 크게 나옴
#       취약지역의 치료역량을 양호지역 수준으로 개선하면 사망률이 얼마나 감소하는지 예측

# 양호지역 치료역량 평균 (목표치)
data_old %>%
  group_by(old_group) %>%
  summarise(
    ami_heal_mean  = round(mean(ami_heal), 4),
    ami_trans_mean = round(mean(ami_trans), 4),
    ami_dead_mean  = round(mean(ami_dead), 4)
  )
# 사망률은 동일하지만 계수 차이가 크기 때문에 진행가능
# 취약지역 시나리오 — 양호지역 평균 수준으로 개선
sim_vul <- data.frame(
  시나리오              = c("현재(취약지역)", "ami_heal 개선\n(양호지역 수준)",
                        "ami_trans 개선\n(양호지역 수준)", "동시 개선"),
  infra_hospital_per_area = mean(data_vul$infra_hospital_per_area),
  infra_amb_per_area      = mean(data_vul$infra_amb_per_area),
  ami_amb                 = mean(data_vul$ami_amb),
  ami_heal   = c(mean(data_vul$ami_heal),
                 mean(data_good$ami_heal),   # 양호지역 수준
                 mean(data_vul$ami_heal),
                 mean(data_good$ami_heal)),  # 양호지역 수준
  ami_trans  = c(mean(data_vul$ami_trans),
                 mean(data_vul$ami_trans),
                 mean(data_good$ami_trans),  # 양호지역 수준
                 mean(data_good$ami_trans)), # 양호지역 수준
  region_old = mean(data_vul$region_old)
)

# 사망률 예측 후 역변환
sim_vul$예측사망률 <- round(exp(predict(m_vul, newdata = sim_vul)) - 1, 4)
sim_vul$감소량     <- round(sim_vul$예측사망률[1] - sim_vul$예측사망률, 4)

# 시나리오 순서 고정
sim_vul$시나리오 <- factor(sim_vul$시나리오,
                       levels = c("현재(취약지역)", "ami_heal 개선\n(양호지역 수준)",
                                  "ami_trans 개선\n(양호지역 수준)", "동시 개선"))

# 선그래프
ggplot(sim_vul, aes(x = 시나리오, y = 예측사망률, group = 1)) +
  geom_line(linewidth = 1.2, color = "#993C1D") +
  geom_point(size = 4, color = "#993C1D") +
  geom_text(aes(label = paste0(예측사망률, "%\n(-", 감소량, ")")),
            vjust = -1, size = 3.5, color = "grey30") +
  geom_hline(yintercept = sim_vul$예측사망률[1],
             linetype = "dashed", color = "grey50") +
  ylim(0, max(sim_vul$예측사망률) * 1.2) +
  labs(title    = "Step 5 | 정책 시뮬레이션 — 취약지역 치료역량 개선",
       subtitle = "취약지역 치료역량을 양호지역 수준으로 끌어올렸을 때 예측 사망률",
       x = NULL, y = "예측 사망률 (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey50"),
        axis.text.x   = element_text(size = 10))

# ami_heal 개선 시
# 취약지역 입원치료 제공률을 양호지역 수준(90.1% → 91.9%)으로 개선
# → 사망률 약 40% 감소 (0.83% → 0.50%)
# → 단 1.8%p 개선으로도 효과가 큼

# ami_trans 개선 시
# 취약지역 전원율을 양호지역 수준(5.58% → 3.39%)으로 감소
# → 오히려 사망률 증가 (0.83% → 1.37%)
# → 기존 분석과 동일한 패턴 — 전원율 억제는 역효과
# → 취약지역에서 전원은 사망률을 낮추는 필수 과정

# 동시 개선 시
# ami_heal 개선 효과가 ami_trans 역효과에 상쇄됨
# → ami_heal만 개선하는 것이 더 효과적


################################################################################
################################################################################
################################################################################
# 최종 정책 결론
# 취약지역(고령화 상위 25%) 사망률 감소를 위한 핵심 과제
# 1. 입원치료 제공 역량 강화가 최우선
#    단 1.8%p 개선으로 사망률 40% 감소 예측
# 2. 전원율 억제 정책은 역효과 — 적절한 전원 체계 유지 필요
# 3. 인프라 증설보다 치료역량 강화에 집중 투자 필요