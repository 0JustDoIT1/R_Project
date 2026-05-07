# install.packages("sandwich")
# install.packages("mediation")
# install.packages("pROC")
# install.packages("sf")

library(dplyr)
library(tidyr)
library(car)
library(lmtest)
library(sandwich)
library(mediation)
library(pROC)
library(ggplot2)
library(sf)
library(effsize)

getwd()
setwd("D:/Project/AI_class/R_Project_data")

final_data <- read.csv("final_data.csv", fileEncoding = "CP949")
final_data
str(final_data)
num_data <- final_data[sapply(final_data, is.numeric)]
num_scaled <- scale(num_data)
boxplot(num_scaled)

# 1단계 : 인프라 격차 검증
# 인구 수 기준
# 정규성 검정
shapiro.test(final_data$infra_hospital_per100k)
shapiro.test(final_data$infra_amb_per100k)

# Wilcoxon으로도 확인
wilcox.test(infra_hospital_per100k ~ metro, data = final_data, exact = FALSE)
wilcox.test(infra_amb_per100k ~ metro, data = final_data, exact = FALSE)

# 효과크기

cohen.d(final_data$infra_hospital_per100k ~ final_data$metro)
cohen.d(final_data$infra_amb_per100k ~ final_data$metro)

# 그룹별 평균
final_data %>%
  group_by(metro) %>%
  summarise(
    mean_hospital = mean(infra_hospital_per100k, na.rm = TRUE),
    mean_amb      = mean(infra_amb_per100k, na.rm = TRUE),
    n = n()
  )

# 면적 기준
wilcox.test(infra_hospital_per_area ~ metro, data = final_data, exact = FALSE)
wilcox.test(infra_amb_per_area ~ metro, data = final_data, exact = FALSE)

cohen.d(final_data$infra_hospital_per_area ~ final_data$metro)
cohen.d(final_data$infra_amb_per_area ~ final_data$metro)

final_data %>%
  group_by(metro) %>%
  summarise(
    mean_hospital_area = mean(infra_hospital_per_area, na.rm = TRUE),
    mean_amb_area      = mean(infra_amb_per_area, na.rm = TRUE)
  )

# "인프라 격차는 어떤 기준으로 보느냐에 따라 다르다.
# 인구당으로는 비수도권이 우수하고,
# 면적당으로는 수도권이 우수하다.
# 즉 비수도권의 문제는 인프라 수량이 아니라
# 지리적 분산으로 인한 접근성 부족이다."
# 즉 => 비수도권에서 말하는 인프라 확충은 지리적 요건을 고려한 주장일 확률이 높음


# 분포 시각화 — 면적당으로 변경
final_data %>%
  dplyr::select(metro, infra_hospital_per_area, infra_amb_per_area) %>%
  pivot_longer(-metro, names_to = "variable", values_to = "value") %>%
  mutate(variable = dplyr::recode(variable,
                                  "infra_hospital_per_area" = "응급기관 수 (면적당)",
                                  "infra_amb_per_area"      = "구급차 수 (면적당)")) %>%
  ggplot(aes(x = metro, y = value, fill = metro)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "수도권 vs 비수도권 인프라 비교 (면적당)",
       x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# 지도 데이터 불러오기
korea_map_raw <- st_read("sig.shp", options = "ENCODING=CP949")

# 시도 코드로 수도권 구분
korea_map <- korea_map_raw %>%
  rename(district = SIG_KOR_NM) %>%
  mutate(
    sido_code = substr(SIG_CD, 1, 2),
    metro = ifelse(sido_code %in% c("11", "28", "41"),
                   "수도권", "비수도권")
  ) %>%
  mutate(district = dplyr::recode(district, "세종특별자치시" = "세종시"))

# 지도 데이터 생성
map_data <- merge(korea_map, final_data, by = "district")

# 응급기관 수 지도 (면적당)
ggplot() +
  geom_sf(data = map_data,
          aes(fill = infra_hospital_per_area),
          color = "white", size = 0.1) +
  geom_sf(data = map_data %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#E84855", size = 0.1) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A2C5B",
                      name = "응급기관 수\n(개/km²)") +
  labs(title = "시군구별 응급기관 수 (면적당)",
       caption = "빨간 테두리 : 수도권") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"),
        legend.position = "right")

# 구급차 수 지도 (면적당)
ggplot() +
  geom_sf(data = map_data,
          aes(fill = infra_amb_per_area),
          color = "white", size = 0.1) +
  geom_sf(data = map_data %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#E84855", size = 0.1) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A2C5B",
                      name = "구급차 수\n(대/km²)") +
  labs(title = "시군구별 구급차 수 (면적당)",
       caption = "빨간 테두리 : 수도권") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"),
        legend.position = "right")


# 2단계 : 인프라(면적당) → 역량지표
# 상관분석
cor.test(final_data$infra_hospital_per_area, final_data$ami_amb,
         method = "spearman", exact = FALSE)
cor.test(final_data$infra_hospital_per_area, final_data$ami_heal,
         method = "spearman", exact = FALSE)
cor.test(final_data$infra_hospital_per_area, final_data$ami_trans,
         method = "spearman", exact = FALSE)

cor.test(final_data$infra_amb_per_area, final_data$ami_amb,
         method = "spearman", exact = FALSE)
cor.test(final_data$infra_amb_per_area, final_data$ami_heal,
         method = "spearman", exact = FALSE)
cor.test(final_data$infra_amb_per_area, final_data$ami_trans,
         method = "spearman", exact = FALSE)

# 회귀분석
model_heal_h  <- lm(ami_heal  ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = final_data)
model_amb_h   <- lm(ami_amb   ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = final_data)
model_trans_h <- lm(ami_trans ~ infra_hospital_per_area + infra_amb_per_area + region_old, data = final_data)

summary(model_heal_h)
summary(model_amb_h)
summary(model_trans_h)


library(ggpubr)

# 응급기관 밀도 → 역량 변수들
p1 <- ggplot(final_data, aes(x = infra_hospital_per_area, y = ami_heal)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E84855") +
  labs(title = "응급기관 밀도 → 입원치료 제공률",
       x = "응급기관 수 (면적당)", y = "입원치료 제공률") +
  theme_minimal()

p2 <- ggplot(final_data, aes(x = infra_hospital_per_area, y = ami_trans)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E84855") +
  labs(title = "응급기관 밀도 → 전원율",
       x = "응급기관 수 (면적당)", y = "전원율") +
  theme_minimal()

p3 <- ggplot(final_data, aes(x = infra_amb_per_area, y = ami_heal)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1A2C5B") +
  labs(title = "구급차 밀도 → 입원치료 제공률",
       x = "구급차 수 (면적당)", y = "입원치료 제공률") +
  theme_minimal()

p4 <- ggplot(final_data, aes(x = infra_amb_per_area, y = ami_trans)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1A2C5B") +
  labs(title = "구급차 밀도 → 전원율",
       x = "구급차 수 (면적당)", y = "전원율") +
  theme_minimal()

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE)

library(corrplot)

cor_vars <- c("infra_hospital_per_area", "infra_amb_per_area",
              "ami_amb", "ami_heal", "ami_trans")

cor_matrix <- cor(final_data[, cor_vars], method = "spearman", 
                  use = "complete.obs")

# 변수명 한글로
rownames(cor_matrix) <- colnames(cor_matrix) <- 
  c("응급기관(면적당)", "구급차(면적당)", "구급차이용률", "입원치료제공률", "전원율")

corrplot(cor_matrix, method = "color", addCoef.col = "black",
         type = "upper", tl.cex = 0.8, tl.col = "black",
         title = "인프라-역량 변수 간 상관관계", mar = c(0,0,2,0))

# 전원율 이상치 확인
final_data %>%
  filter(ami_trans > 15) %>%
  dplyr::select(district, metro, ami_trans, 
                infra_hospital_per_area, ami_heal, ami_dead)

# 전원율 이상치 분석
# 전원율 15% 이상 지역들을 보면 공통점이 있어요.
# 삼척시  33.1% / 입원치료 58.8% / 응급기관 밀도 0.0008
# 속초시  23.5% / 입원치료 54.9% / 응급기관 밀도 0.0189
# 태안군  26.6% / 입원치료 66.0% / 응급기관 밀도 0.0019
# 서산시  30.1% / 입원치료 64.6% / 응급기관 밀도 0.0027
# 전원율 높은 지역 = 입원치료 제공률 낮은 지역이고, 대부분 응급기관 밀도가 매우 낮은 비수도권 중소도시·농촌이에요. 이게 아까 히트맵의 -0.73을 설명해줍니다.

# 응급기관 밀도 이상치 확인
final_data %>%
  filter(infra_hospital_per_area > 0.2) %>%
  dplyr::select(district, metro, infra_hospital_per_area,
                ami_heal, ami_trans, ami_dead)

# 응급기관 밀도 이상치 분석
# 밀도 높은 지역은 전부 도심 구 단위예요.
# 중구(대구)  0.425 / 입원치료 94.6% / 전원율 1.4%
#   동대문구    0.281 / 입원치료 95.5% / 전원율 0.4%
#   수영구      0.294 / 입원치료 87.8% / 전원율 7.3%
#   응급기관이 밀집한 도심일수록 입원치료 제공률이 높고 전원율이 낮아요. 방향이 명확합니다.

# 이 결과가 말하는 것
# 응급기관 밀도 ↑ → 입원치료 제공률 ↑, 전원율 ↓
# 관계는 존재하지만 효과크기가 작음 (r = 0.15, -0.17)
# 이상치(농촌 중소도시)가 패턴을 흐트러뜨리고 있음


# 3단계 : 역량 → 사망률 / 인프라 → 사망률 비교
# 인프라 → 사망률
model_infra_dead <- lm(ami_dead ~ infra_hospital_per_area + 
                         infra_amb_per_area + region_old,
                       data = final_data)
summary(model_infra_dead)

# 역량 → 사망률
model_capacity_dead <- lm(ami_dead ~ ami_amb + ami_heal + 
                            ami_trans + region_old,
                          data = final_data)
summary(model_capacity_dead)

# 모형 비교
AIC(model_infra_dead, model_capacity_dead)

# 역량 변수 개별 상관
cor.test(final_data$ami_heal, final_data$ami_dead,
         method = "spearman", exact = FALSE)
cor.test(final_data$ami_trans, final_data$ami_dead,
         method = "spearman", exact = FALSE)
cor.test(final_data$ami_amb, final_data$ami_dead,
         method = "spearman", exact = FALSE)

# 시각화 1 : 역량 변수 → 사망률 산점도
p1 <- ggplot(final_data, aes(x = ami_heal, y = ami_dead)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E84855") +
  labs(title = "입원치료 제공률 → 사망률",
       x = "입원치료 제공률", y = "병원 내 사망률") +
  theme_minimal()

p2 <- ggplot(final_data, aes(x = ami_trans, y = ami_dead)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E84855") +
  labs(title = "전원율 → 사망률",
       x = "전원율", y = "병원 내 사망률") +
  theme_minimal()

p3 <- ggplot(final_data, aes(x = ami_amb, y = ami_dead)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1A2C5B") +
  labs(title = "구급차 이용률 → 사망률",
       x = "구급차 이용률", y = "병원 내 사망률") +
  theme_minimal()

p4 <- ggplot(final_data, aes(x = region_old, y = ami_dead)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1A2C5B") +
  labs(title = "고령화율 → 사망률",
       x = "고령화율", y = "병원 내 사망률") +
  theme_minimal()

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE)

# 시각화 2 : 사망률 분포 지도
ggplot() +
  geom_sf(data = map_data,
          aes(fill = ami_dead),
          color = "white", size = 0.1) +
  geom_sf(data = map_data %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#E84855", size = 0.1) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A2C5B",
                      name = "병원 내\n사망률") +
  labs(title = "시군구별 심근경색 병원 내 사망률",
       caption = "빨간 테두리 : 수도권") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"))

# 시각화 3 : 입원치료 제공률 지도
ggplot() +
  geom_sf(data = map_data,
          aes(fill = ami_heal),
          color = "white", size = 0.1) +
  geom_sf(data = map_data %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#E84855", size = 0.1) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A2C5B",
                      name = "입원치료\n제공률") +
  labs(title = "시군구별 입원치료 제공률",
       caption = "빨간 테두리 : 수도권") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"))

# 사망률 이상치 지역 확인
final_data %>%
  filter(ami_dead > 20) %>%
  dplyr::select(district, metro, ami_dead, ami_heal, 
                ami_trans, infra_hospital_per_area) %>%
  arrange(desc(ami_dead))

# 각 그래프 해석
# 입원치료 제공률 → 사망률
# 음의 방향이 보이긴 하는데 산포가 매우 커요. 특히 입원치료 제공률이 낮은 구간(60~80%)에서 사망률이 매우 높게 튀는 지역들이 있어요. 이게 회귀에서 유의하게 나온 이유입니다.
# 전원율 → 사망률
# 전원율 0~5% 구간에 데이터가 몰려 있고 그 구간 안에서 사망률 편차가 매우 커요. 전원율 높은 지역(15% 이상)은 오히려 사망률이 낮은 패턴인데, 이게 아까 말씀드린 "적절한 전원이 사망률을 낮춘다" 는 해석을 뒷받침합니다.
# 구급차 이용률 → 사망률
# 거의 평탄한 선이에요. 관계 없음이 시각적으로도 확인됩니다.
# 고령화율 → 사망률
# 완전히 평탄해요. 고령화가 사망률에 영향을 주지 않는다는 게 시각적으로도 명확합니다.

# 핵심 발견
# 시도별 결과와 완전히 일치해요.
# 인프라(면적당) → 사망률 : 유의하지 않음 ❌
# 역량(입원치료 제공률, 전원율) → 사망률 : 유의함 ✅
# 역량 모형이 인프라 모형보다 AIC 14 낮음
# 그런데 흥미로운 점이 있어요. 상관분석에서는 두 역량 변수 모두 유의하지 않은데 회귀에서는 유의하게 나왔어요.
# 이건 두 변수를 동시에 통제했을 때 각각의 고유한 효과가 드러나는 거예요.
# 입원치료 제공률과 전원율이 서로 -0.73 상관이라 개별로는 약하지만 함께 넣으면 효과가 명확해지는 구조입니다.
# 
# 핵심 발견
# 두 가지 상반된 패턴이 보여요.
# 패턴 1 : 울릉군, 보은군, 옥천군
# 입원치료 제공률이 높은데(97~100%) 사망률도 높아요. 치료를 받았지만 사망했다는 건 치료의 질적 수준 문제일 가능성이 높아요. 특히 울릉군은 섬 지역이라 중증 치료 역량이 절대적으로 부족한 구조적 문제가 있어요.
# 패턴 2 : 고성군
# 입원치료 제공률도 낮고(74.1%) 전원율도 높고(18.5%) 응급기관도 없어요. 전형적인 의료 취약지 패턴입니다.

# 4단계 : 매개효과 분석
# 인프라 → 입원치료 제공률 → 사망률

library(mediation)

# Step 1 : X → M (인프라 → 입원치료 제공률)
model_m <- lm(ami_heal ~ infra_hospital_per_area + 
                infra_amb_per_area + region_old,
              data = final_data)

# Step 2 : X + M → Y (인프라 + 입원치료 제공률 → 사망률)
model_y <- lm(ami_dead ~ infra_hospital_per_area + 
                infra_amb_per_area + ami_heal + region_old,
              data = final_data)

summary(model_m)
summary(model_y)

# Bootstrap 매개효과 (응급기관 밀도 기준)
med_result_hospital <- mediate(model_m, model_y,
                               treat = "infra_hospital_per_area",
                               mediator = "ami_heal",
                               boot = TRUE, sims = 1000)
summary(med_result_hospital)

# Bootstrap 매개효과 (구급차 밀도 기준)
med_result_amb <- mediate(model_m, model_y,
                          treat = "infra_amb_per_area",
                          mediator = "ami_heal",
                          boot = TRUE, sims = 1000)
summary(med_result_amb)

# "입원치료 제공률은 인프라와 사망률 사이의 매개변수 역할을 하지 않는 것으로 나타났다.
# 이는 인프라가 치료역량에 미치는 영향이 제한적이기 때문이며,
# 치료역량은 인프라와 독립적으로 사망률에 직접 영향을 미치는 요인임을 시사한다."

# 5단계 : 고령화 영향 검증
# 상관분석
cor.test(final_data$region_old, final_data$ami_dead,
         method = "spearman", exact = FALSE)

# 단순회귀
model_old_simple <- lm(ami_dead ~ region_old, data = final_data)
summary(model_old_simple)

# 전체 통합 모형 (인프라 + 역량 + 고령화)
model_full <- lm(ami_dead ~ infra_hospital_per_area + infra_amb_per_area +
                   ami_amb + ami_heal + ami_trans + region_old,
                 data = final_data)
summary(model_full)

# 모형 비교
AIC(model_infra_dead, model_capacity_dead, model_full)

# 고령화 시각화
ggplot(final_data, aes(x = region_old, y = ami_dead)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E84855") +
  labs(title = "고령화율과 심근경색 사망률",
       x = "고령화율 (%)", y = "병원 내 사망률") +
  theme_minimal()

# "고령화율은 심근경색 병원 내 사망률과 유의한 관계가 없었으며(rho=-0.06, p=0.31),
# 인프라 변수를 추가한 통합 모형도 역량 변수만 포함한 모형보다 설명력이 낮았다.
# 이는 사망률을 결정하는 핵심 요인이 인프라 수량이나 인구 구조가 아닌 실질적인 치료역량임을 재확인한다."

# 6단계 : 정책 시뮬레이션
# 사망률 상위 25% 고위험 지역 추출
high_risk <- final_data %>%
  filter(ami_dead > quantile(ami_dead, 0.75, na.rm = TRUE))

cat("고위험 지역 수:", nrow(high_risk), "\n")
cat("고위험 지역 사망률 범위:", 
    round(min(high_risk$ami_dead), 1), "~", 
    round(max(high_risk$ami_dead), 1), "\n")

# 개선 목표값 (전국 상위 25% 수준)
target_heal  <- quantile(final_data$ami_heal, 0.75, na.rm = TRUE)
target_trans <- quantile(final_data$ami_trans, 0.25, na.rm = TRUE)
# 전원율은 낮을수록 좋으니 하위 25%를 목표로

cat("\n목표 입원치료 제공률:", round(target_heal, 1), "\n")
cat("목표 전원율:", round(target_trans, 1), "\n")

# 시나리오 A : 입원치료 제공률만 개선
simA <- high_risk %>% mutate(ami_heal = target_heal)

# 시나리오 B : 전원율만 개선
simB <- high_risk %>% mutate(ami_trans = target_trans)

# 시나리오 C : 입원치료 제공률 + 전원율 동시 개선
simC <- high_risk %>% mutate(ami_heal  = target_heal,
                             ami_trans = target_trans)

# 현재 예측값 vs 시나리오별 예측값
result <- data.frame(
  region    = high_risk$district,
  metro     = high_risk$metro,
  현재_사망률  = round(predict(model_capacity_dead, high_risk), 2),
  시나리오A   = round(predict(model_capacity_dead, simA), 2),
  시나리오B   = round(predict(model_capacity_dead, simB), 2),
  시나리오C   = round(predict(model_capacity_dead, simC), 2)
) %>%
  mutate(
    A_감소폭 = round(현재_사망률 - 시나리오A, 2),
    B_감소폭 = round(현재_사망률 - 시나리오B, 2),
    C_감소폭 = round(현재_사망률 - 시나리오C, 2)
  ) %>%
  arrange(desc(현재_사망률))

print(result)

# 시나리오별 평균 감소폭
cat("\n시나리오A 평균 감소폭:", round(mean(result$A_감소폭), 2), "\n")
cat("시나리오B 평균 감소폭:", round(mean(result$B_감소폭), 2), "\n")
cat("시나리오C 평균 감소폭:", round(mean(result$C_감소폭), 2), "\n")

# 시각화
result_long <- result %>%
  dplyr::select(region, 현재_사망률, 시나리오A, 시나리오B, 시나리오C) %>%
  pivot_longer(-region, names_to = "시나리오", values_to = "사망률") %>%
  mutate(시나리오 = factor(시나리오, 
                       levels = c("현재_사망률", "시나리오A", 
                                  "시나리오B", "시나리오C")))

ggplot(result_long, aes(x = reorder(region, -사망률), 
                        y = 사망률, color = 시나리오, group = 시나리오)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  coord_flip() +
  scale_color_manual(values = c("현재_사망률" = "#1A2C5B",
                                "시나리오A"   = "#4A7CC7",
                                "시나리오B"   = "#F0803C",
                                "시나리오C"   = "#E84855")) +
  labs(title = "고위험 지역 시나리오별 사망률 변화",
       x = "", y = "병원 내 사망률", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# "입원치료 제공률을 전국 상위 25% 수준으로 개선할 경우 고위험 지역의 사망률이 평균 1.61 감소할 것으로 추정된다.
# 반면 전원율 단독 개선은 오히려 사망률을 높이는 역효과가 나타났는데,
# 이는 적절한 전원이 치료의 일부임을 시사한다.
# 따라서 정책 방향은 전원율 억제가 아닌 현지 입원치료 역량 강화에 집중해야 한다."

# 1순위 : 취약지수 + 지도 시각화
final_data <- final_data %>%
  mutate(
    # 각 변수 0~1 정규화
    score_heal  = 1 - (ami_heal - min(ami_heal, na.rm = TRUE)) /
      (max(ami_heal, na.rm = TRUE) - min(ami_heal, na.rm = TRUE)),
    score_trans = (ami_trans - min(ami_trans, na.rm = TRUE)) /
      (max(ami_trans, na.rm = TRUE) - min(ami_trans, na.rm = TRUE)),
    score_infra = 1 - (infra_hospital_per_area - min(infra_hospital_per_area, na.rm = TRUE)) /
      (max(infra_hospital_per_area, na.rm = TRUE) - min(infra_hospital_per_area, na.rm = TRUE)),
    
    # 취약지수 (높을수록 취약)
    vulnerability = (score_heal + score_trans + score_infra) / 3
  )

# 취약지수 상위 20개 지역
final_data %>%
  arrange(desc(vulnerability)) %>%
  dplyr::select(district, metro, vulnerability, ami_dead, 
                ami_heal, ami_trans, infra_hospital_per_area) %>%
  head(20)

# 취약지수와 사망률 상관
cor.test(final_data$vulnerability, final_data$ami_dead,
         method = "spearman", exact = FALSE)

# 취약지수 → 사망률 산점도
ggplot(final_data, aes(x = vulnerability, y = ami_dead)) +
  geom_point(aes(color = metro), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E84855") +
  labs(title = "응급의료 취약지수와 심근경색 사망률",
       x = "취약지수 (높을수록 취약)", y = "병원 내 사망률") +
  theme_minimal()

# 취약지수 지도
map_data2 <- merge(korea_map, 
                   final_data %>% dplyr::select(district, vulnerability, ami_dead),
                   by = "district")

ggplot() +
  geom_sf(data = map_data2,
          aes(fill = vulnerability),
          color = "white", size = 0.1) +
  geom_sf(data = map_data2 %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#1A2C5B", size = 0.3) +
  scale_fill_gradient(low = "#AED6F1", high = "#E84855",
                      name = "취약지수") +
  labs(title = "시군구별 응급의료 취약지수",
       caption = "파란 테두리 : 수도권 / 붉은색일수록 취약") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"))

# 대부분 비수도권 중소도시·농촌이고 입원치료 제공률이 낮고 전원율이 높은 지역이에요.
# 취약지수와 사망률 상관 : rho = 0.023, p = 0.718
# 상관이 없어요. 이게 의미하는 바가 중요합니다.
# 취약한데도 사망률이 낮은 지역이 많아요. 이건 두 가지로 해석할 수 있어요.
# 첫째, 전원율이 높은 게 오히려 사망률을 낮추는 구조 때문이에요.
# 취약지수에 전원율을 포함시켰는데, 전원율이 높은 지역은 중증 환자를 잘 보내서 사망률이 낮을 수 있거든요.
# 둘째, 지도에서 보이는 패턴이 중요해요.
# 수도권은 파란 테두리 안에 연한 색(낮은 취약)이고, 비수도권은 전반적으로 붉은 색이 강해요.
# 근데 동해안 쪽(속초, 삼척)이 특히 붉게 나오는 게 눈에 띄어요.

final_data <- final_data %>%
  mutate(
    vulnerability2 = (score_heal + score_infra) / 2
  )

cor.test(final_data$vulnerability2, final_data$ami_dead,
         method = "spearman", exact = FALSE)

# 중앙값 기준으로 4개 유형 분류
final_data <- final_data %>%
  mutate(
    heal_group  = ifelse(ami_heal >= median(ami_heal, na.rm = TRUE), 
                         "치료역량 높음", "치료역량 낮음"),
    trans_group = ifelse(ami_trans <= median(ami_trans, na.rm = TRUE), 
                         "전원율 낮음", "전원율 높음"),
    region_type = paste(heal_group, "+", trans_group)
  )

# 유형별 사망률
final_data %>%
  group_by(region_type) %>%
  summarise(
    n = n(),
    mean_dead = round(mean(ami_dead, na.rm = TRUE), 2),
    mean_heal = round(mean(ami_heal, na.rm = TRUE), 2),
    mean_trans = round(mean(ami_trans, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_dead))

# 유형별 사망률 시각화
ggplot(final_data, aes(x = ami_heal, y = ami_trans, color = ami_dead)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_gradient(low = "#AED6F1", high = "#E84855",
                       name = "사망률") +
  geom_vline(xintercept = median(final_data$ami_heal, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = median(final_data$ami_trans, na.rm = TRUE),
             linetype = "dashed", color = "gray50") +
  annotate("text", x = 70, y = 28, label = "치료역량 낮음\n전원율 높음", 
           color = "#E84855", fontface = "bold", size = 3.5) +
  annotate("text", x = 97, y = 28, label = "치료역량 높음\n전원율 높음", 
           color = "gray50", size = 3.5) +
  annotate("text", x = 70, y = 1, label = "치료역량 낮음\n전원율 낮음", 
           color = "gray50", size = 3.5) +
  annotate("text", x = 97, y = 1, label = "치료역량 높음\n전원율 낮음", 
           color = "#1A2C5B", size = 3.5) +
  labs(title = "치료역량 × 전원율 유형별 지역 분포",
       x = "입원치료 제공률", y = "전원율") +
  theme_minimal()

# 정책 메시지
# 최우선 : 치료역량 낮음 + 전원율 높음 (91개 지역)
# → 입원치료 제공률 개선이 핵심
# 
# 차선책 : 치료역량 낮음 + 전원율 낮음 (30개 지역)
# → 전원 시스템과 현지 역량 동시 개선 필요
# 2순위 : 군집 분석

library(cluster)
library(factoextra)

# 군집화에 사용할 변수 선택 및 정규화
cluster_vars <- final_data %>%
  dplyr::select(ami_heal, ami_trans, ami_amb,
                ami_dead, infra_hospital_per_area) %>%
  scale()

# 최적 군집 수 확인 (elbow method)
fviz_nbclust(cluster_vars, kmeans, method = "wss") +
  labs(title = "최적 군집 수 결정 (Elbow Method)")

# silhouette method
fviz_nbclust(cluster_vars, kmeans, method = "silhouette") +
  labs(title = "최적 군집 수 결정 (Silhouette Method)")

# k=2, k=4 둘 다 시도
set.seed(42)
km2 <- kmeans(cluster_vars, centers = 2, nstart = 25)
km4 <- kmeans(cluster_vars, centers = 4, nstart = 25)

final_data$cluster2 <- as.factor(km2$cluster)
final_data$cluster4 <- as.factor(km4$cluster)

# k=2 군집별 특성
cat("=== k=2 군집별 평균 ===\n")
final_data %>%
  group_by(cluster2) %>%
  summarise(
    n = n(),
    사망률 = round(mean(ami_dead, na.rm = TRUE), 2),
    입원치료 = round(mean(ami_heal, na.rm = TRUE), 2),
    전원율 = round(mean(ami_trans, na.rm = TRUE), 2),
    구급차이용률 = round(mean(ami_amb, na.rm = TRUE), 2),
    응급기관밀도 = round(mean(infra_hospital_per_area, na.rm = TRUE), 4)
  )

# k=4 군집별 특성
cat("\n=== k=4 군집별 평균 ===\n")
final_data %>%
  group_by(cluster4) %>%
  summarise(
    n = n(),
    사망률 = round(mean(ami_dead, na.rm = TRUE), 2),
    입원치료 = round(mean(ami_heal, na.rm = TRUE), 2),
    전원율 = round(mean(ami_trans, na.rm = TRUE), 2),
    구급차이용률 = round(mean(ami_amb, na.rm = TRUE), 2),
    응급기관밀도 = round(mean(infra_hospital_per_area, na.rm = TRUE), 4)
  ) %>%
  arrange(desc(사망률))

# k=4 시각화
fviz_cluster(km4, data = cluster_vars,
             palette = c("#1A2C5B", "#4A7CC7", "#F0803C", "#E84855"),
             geom = "point", ellipse.type = "convex",
             ggtheme = theme_minimal()) +
  labs(title = "시군구 군집 분석 결과 (k=4)")

# k=4 지도 시각화
map_data3 <- merge(korea_map,
                   final_data %>% dplyr::select(district, cluster4),
                   by = "district")

ggplot() +
  geom_sf(data = map_data3,
          aes(fill = cluster4),
          color = "white", size = 0.1) +
  geom_sf(data = map_data3 %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "black", size = 0.3) +
  scale_fill_manual(values = c("1" = "#1A2C5B", "2" = "#4A7CC7",
                               "3" = "#F0803C", "4" = "#E84855"),
                    name = "군집") +
  labs(title = "시군구별 군집 분포 (k=4)",
       caption = "검정 테두리 : 수도권") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 10, color = "gray50"))

# 군집별 의미
# 군집 3 (고위험 취약지, 24개)
# 입원치료 제공률 74%로 가장 낮고 전원율 18.8%로 가장 높아요. 응급기관 밀도도 거의 없어요. 삼척, 속초, 고성, 문경 같은 지역들이에요. 최우선 정책 대상입니다.
# 군집 1 (도심 고밀도, 19개)
# 응급기관 밀도가 0.214로 압도적으로 높고 사망률이 가장 낮아요. 서울 주요 구, 대도시 중심 구들이에요. 인프라와 역량이 모두 갖춰진 이상적 유형입니다.
# 군집 2 (일반 비수도권)
# 구급차 이용률이 70.7%로 가장 높아요. 인프라는 어느 정도 있지만 치료역량이 군집 1보다 낮은 개선 필요 지역이에요.

# 수도권 → 군집 1 집중 (도심 고밀도, 사망률 낮음)
# 동해안·내륙 산간 → 군집 3 집중 (고위험 취약지)
# 수도권/비수도권 경계보다
# 지리적 고립도가 더 중요한 요인

# 3순위 : 분위수 회귀
library(quantreg)

model_q25 <- rq(ami_dead ~ ami_heal + ami_trans +
                  infra_hospital_per_area + region_old,
                tau = 0.25, data = final_data)

model_q50 <- rq(ami_dead ~ ami_heal + ami_trans +
                  infra_hospital_per_area + region_old,
                tau = 0.50, data = final_data)

model_q75 <- rq(ami_dead ~ ami_heal + ami_trans +
                  infra_hospital_per_area + region_old,
                tau = 0.75, data = final_data)

summary(model_q25)
summary(model_q50)
summary(model_q75)

# "분위수 회귀 분석 결과, 사망률이 높은 지역(상위 75%)에서 입원치료 제공률과 전원율의 영향이 가장 크게 나타났다.
# 이는 치료역량 개선의 효과가 고위험 지역에서 더 두드러짐을 의미하며,
# 정책 자원을 고위험 지역에 집중 투입할 근거를 제공한다."

# 4순위 : 상호작용 효과 분석
# 모형 1 : 상호작용 없음 (기존)
model_no_int <- lm(ami_dead ~ ami_heal + ami_trans + 
                     infra_hospital_per_area + region_old,
                   data = final_data)

# 모형 2 : 입원치료 × 전원율 상호작용
model_int1 <- lm(ami_dead ~ ami_heal * ami_trans + 
                   infra_hospital_per_area + region_old,
                 data = final_data)

# 모형 3 : 입원치료 × 인프라 상호작용
model_int2 <- lm(ami_dead ~ ami_heal * infra_hospital_per_area + 
                   ami_trans + region_old,
                 data = final_data)

# 모형 4 : 전원율 × 인프라 상호작용
model_int3 <- lm(ami_dead ~ ami_heal + 
                   ami_trans * infra_hospital_per_area + region_old,
                 data = final_data)

summary(model_int1)
summary(model_int2)
summary(model_int3)

# 모형 비교
AIC(model_no_int, model_int1, model_int2, model_int3)

# 상호작용 시각화 (유의한 모형 기준)
# 입원치료 제공률을 높음/낮음으로 나눠서 전원율과 사망률 관계 확인
final_data <- final_data %>%
  mutate(heal_level = ifelse(ami_heal >= median(ami_heal, na.rm = TRUE),
                             "입원치료 높음", "입원치료 낮음"))

ggplot(final_data, aes(x = ami_trans, y = ami_dead, 
                       color = heal_level)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("입원치료 높음" = "#1A2C5B",
                                "입원치료 낮음" = "#E84855")) +
  labs(title = "입원치료 제공률 수준별 전원율 → 사망률 관계",
       x = "전원율", y = "병원 내 사망률", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")



# 사망률 기준 3분위 지역 구분
final_data <- final_data %>%
  mutate(dead_group = case_when(
    ami_dead <= quantile(ami_dead, 0.33, na.rm = TRUE) ~ "저위험",
    ami_dead <= quantile(ami_dead, 0.66, na.rm = TRUE) ~ "중위험",
    TRUE ~ "고위험"
  ),
  dead_group = factor(dead_group, levels = c("저위험", "중위험", "고위험")))

# 그룹별 현황 파악
final_data %>%
  group_by(dead_group) %>%
  summarise(
    n = n(),
    사망률 = round(mean(ami_dead, na.rm = TRUE), 2),
    입원치료 = round(mean(ami_heal, na.rm = TRUE), 2),
    전원율 = round(mean(ami_trans, na.rm = TRUE), 2),
    구급차이용률 = round(mean(ami_amb, na.rm = TRUE), 2),
    응급기관밀도 = round(mean(infra_hospital_per_area, na.rm = TRUE), 4),
    수도권비율 = round(mean(metro == "수도권", na.rm = TRUE) * 100, 1)
  )

# Kruskal-Wallis 검정
kruskal.test(ami_heal  ~ dead_group, data = final_data)
kruskal.test(ami_trans ~ dead_group, data = final_data)
kruskal.test(ami_amb   ~ dead_group, data = final_data)
kruskal.test(infra_hospital_per_area ~ dead_group, data = final_data)
kruskal.test(region_old ~ dead_group, data = final_data)

# 사후 검정
library(FSA)
dunnTest(ami_heal  ~ dead_group, data = final_data, method = "bonferroni")
dunnTest(ami_trans ~ dead_group, data = final_data, method = "bonferroni")
dunnTest(infra_hospital_per_area ~ dead_group, data = final_data, method = "bonferroni")
