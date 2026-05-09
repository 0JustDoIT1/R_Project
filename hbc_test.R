getwd()
setwd("D:/Project/AI_class/R_Project_data/test/")

data <- read.csv("final_data.csv", fileEncoding = "UTF-8")
str(data)

library(tidyverse)
library(ggplot2)
library(patchwork)
library(effsize)
library(car)
library(factoextra)



data$수도권여부 <- factor(data$수도권여부, levels = c("수도권", "비수도권"))
colSums(is.na(data))
data[data$시도 == "세종", ]
# 세종은 2012년 출범한 신생 행정도시라 의료 인프라가 덜 갖춰져 있음
# 인구도 39만명으로 가장 적음
# 권역응급의료센터가 아예 없음
# => 따라서, 제거

# 세종 제거
data <- data[data$시도 != "세종", ]

# ==============================================================================
# 1단계 : 수도권vs비수도권 인프라 구조
# 귀무가설 (H₀): 수도권과 비수도권 간 응급의료 인프라의 분포에 차이가 없다
# 대립가설 (H₁): 수도권과 비수도권 간 응급의료 인프라의 분포에 차이가 있다 (수도권이 더 높다)
# ==============================================================================

# 인구 기준 / 면적 기준 각각 진행
# 1-1. 인구 기준

# EDA
par(mfrow = c(3, 2))
hist(data$구급차수_인구10만당,      main = "구급차수 (인구 10만명당)",      col = "steelblue", xlab = "대/10만명")
hist(data$응급구조사수_인구10만당,  main = "응급구조사수 (인구 10만명당)",  col = "steelblue", xlab = "명/10만명")
hist(data$응급의료기관수_인구10만당,main = "응급의료기관수 (인구 10만명당)",col = "steelblue", xlab = "개/10만명")
hist(data$병상수_인구10만당,        main = "병상수 (인구 10만명당)",        col = "steelblue", xlab = "병상/10만명")
par(mfrow = c(1, 1))

# 수도권 vs 비수도권 박스플롯
par(mfrow = c(3, 2))
boxplot(구급차수_인구10만당       ~ 수도권여부, data = data, main = "구급차수 (인구 10만명당)",       col = c("#4E79A7", "#F28E2B"), ylab = "대/10만명")
boxplot(응급구조사수_인구10만당   ~ 수도권여부, data = data, main = "응급구조사수 (인구 10만명당)",   col = c("#4E79A7", "#F28E2B"), ylab = "명/10만명")
boxplot(응급의료기관수_인구10만당 ~ 수도권여부, data = data, main = "응급의료기관수 (인구 10만명당)", col = c("#4E79A7", "#F28E2B"), ylab = "개/10만명")
boxplot(병상수_인구10만당         ~ 수도권여부, data = data, main = "병상수 (인구 10만명당)",         col = c("#4E79A7", "#F28E2B"), ylab = "병상/10만명")
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$구급차수_인구10만당) # p-value = 0.05975 : 정규분포
shapiro.test(data$응급구조사수_인구10만당) # p-value = 0.04338 : 정규분포 아님
shapiro.test(data$응급의료기관수_인구10만당) # p-value = 0.1442 : 정규분포 아님
shapiro.test(data$병상수_인구10만당) # p-value = 0.1442 : 정규분포

# Wilcoxon test (정규분포 아닌 변수들이 있으므로 t-test 대신 wilcoxon test)
wilcox.test(구급차수_인구10만당 ~ 수도권여부, data = data)
  # p-value = 0.0125
  # 귀무가설 기각: 수도권과 비수도권의 구급차수(인구 10만명당) 차이가 유의함
wilcox.test(응급구조사수_인구10만당 ~ 수도권여부, data = data)
  # p-value = 0.01964
  # 귀무가설 기각: 수도권과 비수도권의 응급구조사수(인구 10만명당) 차이가 유의함
wilcox.test(응급의료기관수_인구10만당 ~ 수도권여부, data = data)
  # p-value = 0.007143
  # 귀무가설 기각: 수도권과 비수도권의 응급의료기관수(인구 10만명당) 차이가 유의함
wilcox.test(병상수_인구10만당 ~ 수도권여부, data = data)
  # p-value = 0.007143
  # 귀무가설 기각: 수도권과 비수도권의 병상수(인구 10만명당) 차이가 유의함

# Cohen's d — 효과크기
# 그룹 순서 확인
levels(data$수도권여부) # [1] "수도권"   "비수도권"

cohen.d(구급차수_인구10만당 ~ 수도권여부, data = data)
  # d estimate: -1.288394 (large) : 수도권 - 비수도권
  # 수도권 < 비수도권
cohen.d(응급구조사수_인구10만당 ~ 수도권여부, data = data)
  # d estimate: -1.236017 (large) : 수도권 - 비수도권
  # 수도권 < 비수도권
cohen.d(응급의료기관수_인구10만당 ~ 수도권여부, data = data)
  # d estimate: -1.301129 (large) : 수도권 - 비수도권
  # 수도권 < 비수도권
cohen.d(병상수_인구10만당 ~ 수도권여부, data = data)
  # d estimate: -1.439647 (large) : 수도권 - 비수도권
  # 수도권 < 비수도권

# 그룹별 중앙값 확인 (Wilcoxon은 중앙값 기준)
# 실제 차이 값 비교를 위해서 진행
data %>%
  group_by(수도권여부) %>%
  summarise(
    구급차수_중앙값       = round(median(구급차수_인구10만당), 4),
    응급구조사수_중앙값   = round(median(응급구조사수_인구10만당), 4),
    응급의료기관수_중앙값 = round(median(응급의료기관수_인구10만당), 4),
    병상수_중앙값         = round(median(병상수_인구10만당), 4),
  )
# 수도권여부 구급차수_중앙값 응급구조사수_중앙값 응급의료기관수_중앙값 병상수_중앙값 재전원율_중앙값
# <fct>                <dbl>               <dbl>                 <dbl>         <dbl>           <dbl>
# 1 수도권                2.07                1.06                  0.54          11.6             0.8
# 2 비수도권              4.55                4.07                  1.01          16.3             1.2

# 인구 기준 시각화
data %>%
  group_by(수도권여부) %>%
  summarise(
    구급차수       = median(구급차수_인구10만당),
    응급구조사수   = median(응급구조사수_인구10만당),
    응급의료기관수 = median(응급의료기관수_인구10만당),
    병상수         = median(병상수_인구10만당)
  ) %>%
  pivot_longer(-수도권여부, names_to = "변수", values_to = "중앙값") %>%
  ggplot(aes(x = 변수, y = 중앙값, fill = 수도권여부)) +
  geom_col(position = "dodge", width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(중앙값, 2)),
            position = position_dodge(0.5), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "인구 10만명당 응급 인프라 — 수도권 vs 비수도권",
       subtitle = "인구 기준으로는 비수도권이 모든 변수에서 높음",
       x = NULL, y = "중앙값", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"))

### 결론
# 인구 10만명당 기준, 모든 인프라 변수에서 비수도권이 수도권보다 높게 나타남
# 이는 수도권의 인구 밀집으로 인한 희석 효과로 해석됨
# 즉, 절대적 자원량은 수도권이 많더라도 인구 대비 자원 배분은 비수도권이 유리함

# 1-2. 면적 기준
# EDA
par(mfrow = c(3, 2))
hist(data$구급차수_면적1000당,       main = "구급차수 (면적 1000km²당)",       col = "steelblue", xlab = "대/1000km²")
hist(data$응급구조사수_면적1000당,   main = "응급구조사수 (면적 1000km²당)",   col = "steelblue", xlab = "명/1000km²")
hist(data$응급의료기관수_면적1000당, main = "응급의료기관수 (면적 1000km²당)", col = "steelblue", xlab = "개/1000km²")
hist(data$병상수_면적1000당,         main = "병상수 (면적 1000km²당)",         col = "steelblue", xlab = "병상/1000km²")
par(mfrow = c(1, 1))

par(mfrow = c(3, 2))
boxplot(구급차수_면적1000당       ~ 수도권여부, data = data, main = "구급차수 (면적 1000km²당)",       col = c("#4E79A7", "#F28E2B"), ylab = "대/1000km²")
boxplot(응급구조사수_면적1000당   ~ 수도권여부, data = data, main = "응급구조사수 (면적 1000km²당)",   col = c("#4E79A7", "#F28E2B"), ylab = "명/1000km²")
boxplot(응급의료기관수_면적1000당 ~ 수도권여부, data = data, main = "응급의료기관수 (면적 1000km²당)", col = c("#4E79A7", "#F28E2B"), ylab = "개/1000km²")
boxplot(병상수_면적1000당         ~ 수도권여부, data = data, main = "병상수 (면적 1000km²당)",         col = c("#4E79A7", "#F28E2B"), ylab = "병상/1000km²")
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$구급차수_면적1000당)       # p-value = 3.063e-05 : 정규분포 아님
shapiro.test(data$응급구조사수_면적1000당)   # p-value = 7.241e-06 : 정규분포 아님
shapiro.test(data$응급의료기관수_면적1000당) # p-value = 0.0001068 : 정규분포 아님
shapiro.test(data$병상수_면적1000당)         # p-value = 3.06e-05  : 정규분포 아님

# Wilcoxon rank-sum test
wilcox.test(구급차수_면적1000당       ~ 수도권여부, data = data)
  # p-value = 0.04107
  # 귀무가설 기각: 수도권과 비수도권의 구급차수(면적 1000km²당) 차이가 유의함 (수도권 > 비수도권)
wilcox.test(응급구조사수_면적1000당   ~ 수도권여부, data = data)
  # p-value = 0.55
  # 귀무가설 기각 실패: 수도권과 비수도권의 응급구조사수(면적 1000km²당) 차이가 유의하지 않음
wilcox.test(응급의료기관수_면적1000당 ~ 수도권여부, data = data)
  # p-value = 0.04107
  # 귀무가설 기각: 수도권과 비수도권의 응급의료기관수(면적 1000km²당) 차이가 유의함 (수도권 > 비수도권)
wilcox.test(병상수_면적1000당         ~ 수도권여부, data = data)
  # p-value = 0.04107
  # 귀무가설 기각: 수도권과 비수도권의 병상수(면적 1000km²당) 차이가 유의함 (수도권 > 비수도권)

# Cohen's d
cohen.d(구급차수_면적1000당       ~ 수도권여부, data = data)
  # d estimate: 1.685637 (large) : 수도권 - 비수도권
  # 수도권 > 비수도권
cohen.d(응급구조사수_면적1000당   ~ 수도권여부, data = data)
  # d estimate: -0.437924 (small) : 수도권 - 비수도권
  # 유의한 차이 없음
cohen.d(응급의료기관수_면적1000당 ~ 수도권여부, data = data)
  # d estimate: 1.300161 (large) : 수도권 - 비수도권
  # 수도권 > 비수도권
cohen.d(병상수_면적1000당         ~ 수도권여부, data = data)
  # d estimate: 1.512147 (large) : 수도권 - 비수도권
  # 수도권 > 비수도권

# 그룹별 중앙값
data %>%
  group_by(수도권여부) %>%
  summarise(
    구급차수_중앙값       = round(median(구급차수_면적1000당), 4),
    응급구조사수_중앙값   = round(median(응급구조사수_면적1000당), 4),
    응급의료기관수_중앙값 = round(median(응급의료기관수_면적1000당), 4),
    병상수_중앙값         = round(median(병상수_면적1000당), 4)
  )

# 면적 기준 시각화
data %>%
  group_by(수도권여부) %>%
  summarise(
    구급차수       = median(구급차수_면적1000당),
    응급구조사수   = median(응급구조사수_면적1000당),
    응급의료기관수 = median(응급의료기관수_면적1000당),
    병상수         = median(병상수_면적1000당)
  ) %>%
  pivot_longer(-수도권여부, names_to = "변수", values_to = "중앙값") %>%
  ggplot(aes(x = 변수, y = 중앙값, fill = 수도권여부)) +
  geom_col(position = "dodge", width = 0.5, alpha = 0.85) +
  geom_text(aes(label = round(중앙값, 2)),
            position = position_dodge(0.5), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("수도권" = "#378ADD", "비수도권" = "#1D9E75")) +
  labs(title    = "면적 1000km²당 응급 인프라 — 수도권 vs 비수도권",
       subtitle = "면적 기준으로는 수도권이 모든 변수에서 높음",
       x = NULL, y = "중앙값", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey50"))

### 결론
# 면적 1000km²당 기준, 수도권이 비수도권보다 높게 나타남
# 인구 기준과 방향이 반대로 뒤집힘
# 즉, 지리적 접근성 측면에서는 수도권에 인프라가 더 밀집되어 있음

##### 인프라 최종 결론
##### "비수도권은 인프라 수가 부족한 것이 아니라, 접근성이 부족하다"

##### [인구 기준] 비수도권 > 수도권 : 인구 대비 자원은 비수도권이 오히려 많음
##### [면적 기준] 수도권 > 비수도권 : 지리적 밀집도는 수도권이 압도적으로 높음

##### 이는 비수도권의 인프라가 넓은 면적에 흩어져 있어
##### 응급상황 발생 시 실제로 인프라에 도달하기까지의
##### 거리와 시간이 훨씬 길다는 것을 의미

##### why? 인구당 : 자원 배분의 형평성
#####       면적당 : 실제 도달 가능성
##### => 응급상황은 거리와 시간이 우선이므로 면적당이 더 적합한 분석 기준

##### 따라서, 비수도권 응급의료 문제의 핵심은
##### 인프라 증설 자체보다 배치 최적화와 접근성 개선이 우선 과제


# ==============================================================================
# 2단계: 교란변수 사전 검토
# 귀무가설 (H₀): 고령화율, KTAS중증비율은 인프라·사망률과 독립적이다
# 대립가설 (H₁): 고령화율, KTAS중증비율이 인프라·사망률 양쪽과 상관 → 교란 가능
# ==============================================================================
# EDA
par(mfrow = c(1, 2))
hist(data$고령화율,    main = "고령화율",    col = "steelblue", xlab = "%")
hist(data$KTAS중증비율, main = "KTAS중증비율", col = "steelblue", xlab = "%")
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$고령화율)     # p-value = 0.212  : 정규분포 but 표본이 작아 왜곡됨
shapiro.test(data$KTAS중증비율) # p-value = 0.7533 : 정규분포 but 표본이 작아 왜곡됨

# 상관분석에서 Pearson 대신 Spearman 사용 (모양이 정규분포도 아니고, 표본이 적기 때문에)
# Pearson : 두 변수가 선형 관계로 가정 (정규분포에 사용), 이상치에 민감
# Spearman : 순위 기반, 선형 관계 아니어도 가능 (정규분포 아니어도 됨), 이상치에 강건

# Spearman 상관 (고령화율 vs 인프라·사망률)
cor.test(data$고령화율, data$구급차수_면적1000당,       method = "spearman")
cor.test(data$고령화율, data$응급구조사수_면적1000당,   method = "spearman")
cor.test(data$고령화율, data$응급의료기관수_면적1000당, method = "spearman")
cor.test(data$고령화율, data$병상수_면적1000당,         method = "spearman")
cor.test(data$고령화율, data$응급실내사망률,            method = "spearman")

# Spearman 상관 (KTAS중증비율 vs 인프라·사망률)
cor.test(data$KTAS중증비율, data$구급차수_면적1000당,       method = "spearman")
cor.test(data$KTAS중증비율, data$응급구조사수_면적1000당,   method = "spearman")
cor.test(data$KTAS중증비율, data$응급의료기관수_면적1000당, method = "spearman")
cor.test(data$KTAS중증비율, data$병상수_면적1000당,         method = "spearman")
cor.test(data$KTAS중증비율, data$응급실내사망률,            method = "spearman")

# [고령화율] → 교란 HIGH
# 인프라 변수(면적 기준) 전체와 유의한 음의 상관 확인
# 고령화율 ↑ → 면적당 인프라 ↓ (rho: -0.62 ~ -0.67, p < 0.05)
# 고령화율 vs 사망률 : rho = 0.14, p = 0.617 → 유의하지 않음
# => 인프라와는 강하게 연결되나 사망률과는 직접 연결되지 않음
# => 그러나 인프라를 통한 간접 교란 가능성 존재
# => 이후 모든 회귀모델에 고령화율 필수 통제변수로 포함

# [KTAS중증비율] → 교란 LOW
# 인프라 변수 전체와 유의한 상관 없음 (p > 0.05)
# 사망률과의 상관 : rho = -0.49, p = 0.052 → 경계 수준
# => 인프라와 독립적이므로 교란변수로 보기 어려움
# => 보조 통제변수로만 사용, 해석 단순화 가능

# [최종 판단]
# 고령화율 : 필수 통제변수 (교란 HIGH)
# KTAS중증비율 : 보조 통제변수 (교란 LOW)
# => 이후 회귀모델: lm(사망률 ~ 인프라 + 고령화율 + KTAS중증비율)

# ==============================================================================
# 3단계: 인프라 → 시간
# 귀무가설 (H₀): 인프라 변수와 시간 변수 간 관계가 없다
# 대립가설 (H₁): 인프라↑ → 구급대반응시간↓, 발병후내원소요시간↓, 내원후퇴실소요시간↓
# ==============================================================================
# EDA
par(mfrow = c(1, 3))
hist(data$구급대반응시간_중앙값,      main = "구급대 반응시간",      col = "steelblue", xlab = "분")
hist(data$발병후내원소요시간_중앙값,  main = "발병 후 내원 소요시간", col = "steelblue", xlab = "분")
hist(data$내원후퇴실소요시간_중앙값,  main = "내원 후 퇴실 소요시간", col = "steelblue", xlab = "분")
par(mfrow = c(1, 1))

par(mfrow = c(1, 3))
boxplot(구급대반응시간_중앙값     ~ 수도권여부, data = data, main = "구급대 반응시간",       col = c("#4E79A7", "#F28E2B"), ylab = "분")
boxplot(발병후내원소요시간_중앙값 ~ 수도권여부, data = data, main = "발병 후 내원 소요시간", col = c("#4E79A7", "#F28E2B"), ylab = "분")
boxplot(내원후퇴실소요시간_중앙값 ~ 수도권여부, data = data, main = "내원 후 퇴실 소요시간", col = c("#4E79A7", "#F28E2B"), ylab = "분")
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$구급대반응시간_중앙값)     # p-value = 0.0006 : 정규분포 아님
shapiro.test(data$발병후내원소요시간_중앙값) # p-value = 0.4466 : 정규분포
shapiro.test(data$내원후퇴실소요시간_중앙값) # p-value = 0.0013 : 정규분포 아님

# Spearman 상관 (인프라 면적기준 vs 시간변수)
cor.test(data$구급차수_면적1000당,       data$구급대반응시간_중앙값,     method = "spearman")
cor.test(data$구급차수_면적1000당,       data$발병후내원소요시간_중앙값, method = "spearman")
cor.test(data$구급차수_면적1000당,       data$내원후퇴실소요시간_중앙값, method = "spearman")

cor.test(data$응급구조사수_면적1000당,   data$구급대반응시간_중앙값,     method = "spearman")
cor.test(data$응급구조사수_면적1000당,   data$발병후내원소요시간_중앙값, method = "spearman")
cor.test(data$응급구조사수_면적1000당,   data$내원후퇴실소요시간_중앙값, method = "spearman")

cor.test(data$응급의료기관수_면적1000당, data$구급대반응시간_중앙값,     method = "spearman")
cor.test(data$응급의료기관수_면적1000당, data$발병후내원소요시간_중앙값, method = "spearman")
cor.test(data$응급의료기관수_면적1000당, data$내원후퇴실소요시간_중앙값, method = "spearman")

cor.test(data$병상수_면적1000당,         data$구급대반응시간_중앙값,     method = "spearman")
cor.test(data$병상수_면적1000당,         data$발병후내원소요시간_중앙값, method = "spearman")
cor.test(data$병상수_면적1000당,         data$내원후퇴실소요시간_중앙값, method = "spearman")

# 인프라 vs 시간변수 산점도 (유의한 조합만)
par(mfrow = c(3, 2))

plot(data$구급차수_면적1000당, data$구급대반응시간_중앙값,
     main = "구급차수 vs 구급대반응시간",
     xlab = "구급차수 (면적 1000km²당)", ylab = "분",
     pch = 19, col = "#4E79A7")
text(data$구급차수_면적1000당, data$구급대반응시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(구급대반응시간_중앙값 ~ 구급차수_면적1000당, data = data), col = "red", lty = 2)

plot(data$구급차수_면적1000당, data$발병후내원소요시간_중앙값,
     main = "구급차수 vs 발병후내원소요시간",
     xlab = "구급차수 (면적 1000km²당)", ylab = "분",
     pch = 19, col = "#4E79A7")
text(data$구급차수_면적1000당, data$발병후내원소요시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(발병후내원소요시간_중앙값 ~ 구급차수_면적1000당, data = data), col = "red", lty = 2)

plot(data$응급의료기관수_면적1000당, data$구급대반응시간_중앙값,
     main = "응급의료기관수 vs 구급대반응시간",
     xlab = "응급의료기관수 (면적 1000km²당)", ylab = "분",
     pch = 19, col = "#F28E2B")
text(data$응급의료기관수_면적1000당, data$구급대반응시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(구급대반응시간_중앙값 ~ 응급의료기관수_면적1000당, data = data), col = "red", lty = 2)

plot(data$응급의료기관수_면적1000당, data$발병후내원소요시간_중앙값,
     main = "응급의료기관수 vs 발병후내원소요시간",
     xlab = "응급의료기관수 (면적 1000km²당)", ylab = "분",
     pch = 19, col = "#F28E2B")
text(data$응급의료기관수_면적1000당, data$발병후내원소요시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(발병후내원소요시간_중앙값 ~ 응급의료기관수_면적1000당, data = data), col = "red", lty = 2)

plot(data$병상수_면적1000당, data$구급대반응시간_중앙값,
     main = "병상수 vs 구급대반응시간",
     xlab = "병상수 (면적 1000km²당)", ylab = "분",
     pch = 19, col = "#59A14F")
text(data$병상수_면적1000당, data$구급대반응시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(구급대반응시간_중앙값 ~ 병상수_면적1000당, data = data), col = "red", lty = 2)

plot(data$병상수_면적1000당, data$발병후내원소요시간_중앙값,
     main = "병상수 vs 발병후내원소요시간",
     xlab = "병상수 (면적 1000km²당)", ylab = "분",
     pch = 19, col = "#59A14F")
text(data$병상수_면적1000당, data$발병후내원소요시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(발병후내원소요시간_중앙값 ~ 병상수_면적1000당, data = data), col = "red", lty = 2)

par(mfrow = c(1, 1))

# [구급대 반응시간 / 발병후 내원 소요시간]
# 구급차수, 응급의료기관수, 병상수(면적 기준)와 강한 음의 상관 확인
# 인프라 밀집도 ↑ → 구급대 반응시간 ↓, 발병후 내원 소요시간 ↓
# 즉, 면적당 인프라가 촘촘할수록 실제 응급 대응 시간이 단축됨

# [내원후 퇴실 소요시간]
# 모든 인프라 변수와 유의한 상관 없음 (p > 0.05)
# 내원후 퇴실 소요시간은 인프라 접근성보다 병원 내부 처치 역량의 문제
# => 인프라 → 시간 경로에서 내원후퇴실소요시간은 제외

# [응급구조사수]
# 모든 시간변수와 유의한 상관 없음
# => 이후 분석에서 응급구조사수 비중 축소 고려

# [최종 판단]
# 인프라(면적 기준) → 시간 경로는
# 구급대반응시간, 발병후내원소요시간에서만 유의하게 확인됨

# 인프라 변수 간 상관 확인
cor(data[, c("구급차수_면적1000당", "응급구조사수_면적1000당",
             "응급의료기관수_면적1000당", "병상수_면적1000당")],
    method = "spearman")
# VIF 확인 (더미 회귀로 체크)
vif_model <- lm(구급대반응시간_중앙값 ~ 구급차수_면적1000당 +
                  응급의료기관수_면적1000당 +
                  병상수_면적1000당 +
                  고령화율, data = data)
vif(vif_model)
# VIF 결과
# 구급차수_면적1000당       : 43.05  → 심각 (VIF > 10)
# 응급의료기관수_면적1000당 : 48.33  → 심각 (VIF > 10)
# 병상수_면적1000당         : 130.30 → 매우 심각 (VIF > 10)
# 고령화율                  : 1.12   → 정상

# 상관행렬에서도 확인
# 구급차수 vs 응급의료기관수 : rho = 0.976 → 거의 동일한 변수
# 구급차수 vs 병상수         : rho = 0.974 → 거의 동일한 변수
# 응급의료기관수 vs 병상수   : rho = 0.979 → 거의 동일한 변수

# => 구급차수, 응급의료기관수, 병상수 셋을 동시에 넣는 건 불가
# => 세 가지 옵션
# 옵션 A : 가장 상관 높았던 변수 하나만 선택 (구급차수 or 병상수)
# 옵션 B : 세 변수를 PCA로 합쳐서 인프라 종합지수로 만들기
# 옵션 C : 변수별 단순회귀 모델 3개 분리 운영

# PCA 대상 변수 (응급구조사수 제외 - 상관분석에서 유의하지 않았음)
infra_vars <- data[, c("구급차수_면적1000당",
                       "응급의료기관수_면적1000당",
                       "병상수_면적1000당")]

# PCA 실행 (표준화 포함)
pca_result <- prcomp(infra_vars, scale. = TRUE)
summary(pca_result)

# 스크리 플롯 (몇 개 주성분 사용할지 확인)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

# 변수별 기여도 확인
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# PC1 점수를 인프라 종합지수로 저장
data$인프라종합지수 <- pca_result$x[, 1]

# 확인
data %>%
  select(시도, 수도권여부, 인프라종합지수) %>%
  arrange(desc(인프라종합지수))

# PCA 결과 해석
# PC1이 전체 분산의 98.5% 설명 → PC1 하나로 충분
# 세 변수 모두 PC1 방향으로 거의 동일하게 기여 (contrib 33.3%씩)
# => 인프라종합지수 = 세 변수의 균등 가중 종합지수로 해석 가능

# 인프라종합지수 해석 (높을수록 면적당 인프라 밀집도 낮음)
# 주의: PC1이 음의 방향 = 서울(-5.63)이 가장 낮은 값
# 즉, 현재 지수는 높을수록 인프라 밀집도가 낮은 구조
# => 해석 편의를 위해 부호 반전 (-1 곱하기)

data$인프라종합지수 <- -data$인프라종합지수

# 부호 반전 후 확인 (높을수록 인프라 밀집도 높음)
data %>%
  select(시도, 수도권여부, 인프라종합지수) %>%
  arrange(desc(인프라종합지수))

# 모델 1: 인프라종합지수 → 구급대반응시간
model1 <- lm(구급대반응시간_중앙값 ~ 인프라종합지수, data = data)
summary(model1)

# [모델 1: 인프라종합지수 → 구급대반응시간]
# 인프라종합지수 : β = -0.190, p = 0.039 → 유의
# R² = 0.270, Adjusted R² = 0.218, F p = 0.039
# => 인프라종합지수 ↑ → 구급대 반응시간 ↓ 유의한 음의 관계 확인
# => 인프라 접근성이 높을수록 구급대 반응시간이 단축됨
# => 단, R²=0.270으로 설명력은 낮음 (27%만 설명)

# 모델 2: 인프라종합지수 → 발병후내원소요시간
model2 <- lm(발병후내원소요시간_중앙값 ~ 인프라종합지수, data = data)
summary(model2)

# [모델 2: 인프라종합지수 → 발병후내원소요시간]
# 인프라종합지수 : β = -5.796, p = 0.185 → 유의하지 않음
# R² = 0.122, Adjusted R² = 0.059, F p = 0.185
# => 인프라종합지수만으로는 발병후 내원소요시간을 설명하지 못함
# => 구급차수 단독 모델과 동일한 결론

# 잔차 진단
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# 산점도
par(mfrow = c(1, 2))
plot(data$인프라종합지수, data$구급대반응시간_중앙값,
     main = "인프라종합지수 vs 구급대반응시간",
     xlab = "인프라종합지수", ylab = "분",
     pch = 19, col = "#4E79A7")
text(data$인프라종합지수, data$구급대반응시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(구급대반응시간_중앙값 ~ 인프라종합지수, data = data), col = "red", lty = 2)

plot(data$인프라종합지수, data$발병후내원소요시간_중앙값,
     main = "인프라종합지수 vs 발병후내원소요시간",
     xlab = "인프라종합지수", ylab = "분",
     pch = 19, col = "#F28E2B")
text(data$인프라종합지수, data$발병후내원소요시간_중앙값,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(발병후내원소요시간_중앙값 ~ 인프라종합지수, data = data), col = "red", lty = 2)
par(mfrow = c(1, 1))

# [최종 판단]
# 인프라종합지수 → 구급대반응시간 : 유의한 경로 확인 ✅
# 인프라종합지수 → 발병후내원소요시간 : 유의하지 않음 ❌
# => STEP 4에서 구급대반응시간을 주요 매개변수로 사용
# => 발병후내원소요시간은 보조적으로만 활용
# => 단, R²가 낮아 설명되지 않는 분산이 크므로 해석 시 한계 명시 필요

# ==============================================================================
# 4단계: 시간 → 사망률
# 귀무가설 (H₀): 시간 변수와 사망률 간 관계가 없다
# 대립가설 (H₁): 구급대반응시간↑, 발병후내원소요시간↑ → 사망률↑
# ==============================================================================
# EDA
par(mfrow = c(1, 2))
hist(data$응급실내사망률, main = "응급실 내 사망률", col = "steelblue", xlab = "%")
boxplot(응급실내사망률 ~ 수도권여부, data = data,
        main = "응급실 내 사망률", col = c("#4E79A7", "#F28E2B"), ylab = "%")
par(mfrow = c(1, 1))

# 정규성 검정
shapiro.test(data$응급실내사망률) # p-value = 0.971 : 정규분포

# Spearman 상관
cor.test(data$구급대반응시간_중앙값,     data$응급실내사망률, method = "spearman")
  # rho = 0.188, p = 0.486 → 유의하지 않음
cor.test(data$발병후내원소요시간_중앙값, data$응급실내사망률, method = "spearman")
  # rho = 0.111, p = 0.681 → 유의하지 않음


# 회귀모델
model3 <- lm(응급실내사망률 ~ 구급대반응시간_중앙값, data = data)
summary(model3)
# [모델 3: 구급대반응시간 → 사망률]
# β = 0.086, p = 0.583, R² = 0.022 → 유의하지 않음

model4 <- lm(응급실내사망률 ~ 발병후내원소요시간_중앙값, data = data)
summary(model4)
# [모델 4: 발병후내원소요시간 → 사망률]
# β = 0.001, p = 0.675, R² = 0.013 → 유의하지 않음

model5 <- lm(응급실내사망률 ~ 구급대반응시간_중앙값 + 발병후내원소요시간_중앙값, data = data)
summary(model5)
vif(model5)
# [모델 5: 구급대반응시간 + 발병후내원소요시간 → 사망률]
# 두 변수 모두 유의하지 않음, R² = 0.024
# VIF = 1.38 → 다중공선성 없음

# 산점도
par(mfrow = c(1, 2))
plot(data$구급대반응시간_중앙값, data$응급실내사망률,
     main = "구급대반응시간 vs 사망률",
     xlab = "분", ylab = "%",
     pch = 19, col = "#4E79A7")
text(data$구급대반응시간_중앙값, data$응급실내사망률,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(응급실내사망률 ~ 구급대반응시간_중앙값, data = data), col = "red", lty = 2)

plot(data$발병후내원소요시간_중앙값, data$응급실내사망률,
     main = "발병후내원소요시간 vs 사망률",
     xlab = "분", ylab = "%",
     pch = 19, col = "#F28E2B")
text(data$발병후내원소요시간_중앙값, data$응급실내사망률,
     labels = data$시도, cex = 0.7, pos = 3)
abline(lm(응급실내사망률 ~ 발병후내원소요시간_중앙값, data = data), col = "red", lty = 2)
par(mfrow = c(1, 1))

# 잔차 진단
par(mfrow = c(2, 2))
plot(model3)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(model4)
par(mfrow = c(1, 1))

# 통제변수 추가 모델
model6 <- lm(응급실내사망률 ~ 구급대반응시간_중앙값 + 고령화율 + KTAS중증비율, data = data)
summary(model6)
vif(model6)

model7 <- lm(응급실내사망률 ~ 발병후내원소요시간_중앙값 + 고령화율 + KTAS중증비율, data = data)
summary(model7)
vif(model7)

model8 <- lm(응급실내사망률 ~ 구급대반응시간_중앙값 + 발병후내원소요시간_중앙값 + 고령화율 + KTAS중증비율, data = data)
summary(model8)
vif(model8)
