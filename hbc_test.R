# install.packages("sandwich")
# install.packages("mediation")
# install.packages("pROC")
# install.packages("sf")

library(dplyr)
library(car)
library(lmtest)
library(sandwich)
library(mediation)
library(pROC)
library(ggplot2)
library(sf)

getwd()
final_data <- read.csv("final_data.csv", fileEncoding = "CP949")
final_data
str(final_data)
num_data <- final_data[sapply(final_data, is.numeric)]
num_scaled <- scale(num_data)
boxplot(num_scaled)

# 1단계 : 인프라 격차 검증
# 정규성 검정
shapiro.test(final_data$infra_hospital)
shapiro.test(final_data$infra_amb)

# Wilcoxon으로도 확인
wilcox.test(infra_hospital ~ metro, data = final_data, exact = FALSE)
wilcox.test(infra_amb ~ metro, data = final_data, exact = FALSE)

# 효과크기
library(effsize)
cohen.d(final_data$infra_hospital ~ final_data$metro)
cohen.d(final_data$infra_amb ~ final_data$metro)

# 그룹별 평균
final_data %>%
  group_by(metro) %>%
  summarise(
    mean_hospital = mean(infra_hospital, na.rm = TRUE),
    mean_amb      = mean(infra_amb, na.rm = TRUE),
    n = n()
  )

# 분포 시각화 — 두 변수 한번에
final_data %>%
  dplyr::select(metro, infra_hospital, infra_amb) %>%
  pivot_longer(-metro, names_to = "variable", values_to = "value") %>%
  mutate(variable = dplyr::recode(variable,
                           "infra_hospital" = "응급기관 수",
                           "infra_amb"      = "구급차 수")) %>%
  ggplot(aes(x = metro, y = value, fill = metro)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "수도권 vs 비수도권 인프라 비교",
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

# 응급기관 수 지도
map1 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = infra_hospital),
          color = "white", size = 0.1) +
  geom_sf(data = map_data %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#E84855", size = 0.1) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A2C5B",
                      name = "응급기관 수") +
  labs(title = "시군구별 응급기관 수") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# 구급차 수 지도
map2 <- ggplot() +
  geom_sf(data = map_data,
          aes(fill = infra_amb),
          color = "white", size = 0.1) +
  geom_sf(data = map_data %>%
            filter(substr(SIG_CD, 1, 2) %in% c("11", "28", "41")),
          fill = NA, color = "#E84855", size = 0.1) +
  scale_fill_gradient(low = "#AED6F1", high = "#1A2C5B",
                      name = "구급차 수") +
  labs(title = "시군구별 구급차 수") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

# 두 지도 나란히
library(gridExtra)
grid.arrange(map1, map2, ncol = 2,
             bottom = "빨간 테두리 : 수도권")

# 2단계 : 인프라 -> 역량지표
# 인프라 → 구급차 이용률
cor.test(final_data$infra_hospital, final_data$ami_amb,
         method = "spearman", exact = FALSE)

model_amb <- lm(ami_amb ~ infra_hospital + region_old, data = final_data)
summary(model_amb)

# 인프라 → 입원치료 제공률
cor.test(final_data$infra_hospital, final_data$ami_heal,
         method = "spearman", exact = FALSE)

model_heal <- lm(ami_heal ~ infra_hospital + region_old, data = final_data)
summary(model_heal)

# 인프라 → 전원율
cor.test(final_data$infra_hospital, final_data$ami_trans,
         method = "spearman", exact = FALSE)

model_trans <- lm(ami_trans ~ infra_hospital + region_old, data = final_data)
summary(model_trans)

# 3단계 : 인프라 -> 사망률 , 역량지표 -> 사망률
# 인프라 → 사망률
cor.test(final_data$infra_hospital, final_data$ami_dead,
         method = "spearman", exact = FALSE)

model_infra_dead <- lm(ami_dead ~ infra_hospital + region_old, 
                       data = final_data)
summary(model_infra_dead)

# 역량 → 사망률 (각각)
cor.test(final_data$ami_amb, final_data$ami_dead,
         method = "spearman", exact = FALSE)
cor.test(final_data$ami_heal, final_data$ami_dead,
         method = "spearman", exact = FALSE)
cor.test(final_data$ami_trans, final_data$ami_dead,
         method = "spearman", exact = FALSE)

model_capacity_dead <- lm(ami_dead ~ ami_amb + ami_heal + 
                            ami_trans + region_old,
                          data = final_data)
summary(model_capacity_dead)

# 모형 비교
AIC(model_infra_dead, model_capacity_dead)
