install.packages("readr") # CSV 등 대용량 텍스트 파일을 빠르고 정확하게 읽어오는 패키지
install.packages("dplyr") # 데이터 프레임 조작(필터링, 요약, 변량 계산 등)의 핵심 도구
install.packages("ggplot2") # R의 표준 그래프 제작 패키지 (레이어 방식으로 정교한 시각화 가능)
install.packages("effsize") # 효과 크기(Cohen's d 등)를 계산하여 차이의 실질적 의미를 측정
install.packages("mediation") # 매개분석(인과 관계의 중간 경로 분석)을 수행하는 전문 패키지
install.packages("car") # 회귀 진단(다중공선성 VIF 확인 등) 및 통계 분석용 필수 도구
install.packages("factoextra") # 군집 분석(Clustering) 및 PCA 결과를 시각적으로 예쁘게 표현해주는 도구
install.packages("plotly") # 인터랙티브(마우스 오버, 확대 등) 그래프를 만드는 동적 시각화 도구
install.packages("showtext") # 그래프 내 한글 폰트 깨짐 방지 및 다양한 폰트 적용 지원

library(readr)
library(dplyr)
library(ggplot2)
library(effsize)
library(mediation)
library(car)
library(factoextra)
library(plotly)
library(showtext)

font_add_google("Nanum Gothic", "nanum") # 구글 폰트에서 나눔고딕 가져오기
showtext_auto()

getwd()
data <- read_csv("final_data.csv", locale = locale(encoding = "CP949"))
str(data)

colnames(data)

summary(data)


# 가설 1
# 수도권 vs 비수도권 인프라 격차

# 응급기관 수
wilcox.test(infra_hospital_per100k ~ metro, data = data)

# 구급차 수
wilcox.test(infra_amb_per100k ~ metro, data = data)

# 면적 대비 응급기관
wilcox.test(infra_hospital_per_area ~ metro, data = data)

# 면적 대비 구급차
wilcox.test(infra_amb_per_area ~ metro, data = data)

# 효과크기
cohen.d(data$infra_hospital_per100k, data$metro)
cohen.d(data$infra_amb_per100k, data$metro)

# 시각화
ggplot(data, aes(x = metro, y = infra_hospital_per100k, fill = metro)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2)

ggplot(data, aes(x = metro, y = infra_amb_per100k, fill = metro)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2)

ggplot(data, aes(x = metro, y = infra_hospital_per_area, fill = metro)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2)

ggplot(data, aes(x = metro, y = infra_amb_per_area, fill = metro)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2)


# 가설 2
# 인프라 → 치료역량

# 입원치료 제공률
model_heal <- lm(
  ami_heal ~
    infra_hospital_per100k +
    infra_amb_per100k,
  data = data
)

summary(model_heal)

# 전원율
model_trans <- lm(
  ami_trans ~
    infra_hospital_per100k +
    infra_amb_per100k,
  data = data
)

summary(model_trans)

# 구급차 이용률
model_amb <- lm(
  ami_amb ~
    infra_hospital_per100k +
    infra_amb_per100k,
  data = data
)

summary(model_amb)

ggplot(data, aes(infra_hospital_per100k, ami_heal)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE) +
  labs(title="응급기관 수와 입원치료 제공률")

ggplot(data, aes(infra_hospital_per100k, ami_trans)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE) +
  labs(title="응급기관 수와 전원율")

ggplot(data, aes(infra_amb_per100k, ami_amb)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE) +
  labs(title="구급차 수와 구급차 이용률")

# 상관분석

# 1. 병원 밀도와 치료율의 관계
cor.test(data$infra_hospital_per100k, data$ami_heal, method = "spearman", exact = FALSE)

# 2. 구급차 밀도와 치료율의 관계
cor.test(data$infra_amb_per100k, data$ami_heal, method = "spearman", exact = FALSE)

# 3. 병원 밀도와 전원율의 관계 (단위 통일: per100k로 수정 추천)
cor.test(data$infra_hospital_per100k, data$ami_trans, method = "spearman", exact = FALSE)

# 4. 구급차 밀도와 구급차 이용률의 관계 (단위 통일: per100k로 수정 추천)
cor.test(data$infra_amb_per100k, data$ami_amb, method = "spearman", exact = FALSE)

# 가설 3
# 치료역량 → 사망률률

model_3 <- lm(
  ami_dead ~
    ami_amb +
    ami_heal +
    ami_trans,
  data = data
)

summary(model_3)

par(mfrow = c(2,2))
plot(model_3)

# 매개효과
# 응급기관수 → 입원치료 제공률 → 사망률

model_m <- lm( # 
  ami_heal ~
    infra_hospital_per100k +
    infra_amb_per100k,
  data = data
)
# 병원과 구급차 수가 , 그 지역의 '입원치료 제공률(치료 역량)' 영향을 미치는지

summary(model_m)

model_y <- lm(
  ami_dead ~
    infra_hospital_per100k +
    infra_amb_per100k +
    ami_heal,
  data = data
)
# 병원과 구급차 수, 입원치료 제공, 그 지역의 '사망률' 영향을 미치는지
summary(model_y)

med_result <- mediate(
  model.m = model_m,
  model.y = model_y,
  treat = "infra_hospital_per100k",
  mediator = "ami_heal",
  sims = 1000
)
# 응급기관 수가 '입원치료 제공률'이라는 중간 과정을 거쳐서, 
# model_y에 들어있는 최종 결과(사망률)에 어떤 영향을 미치는지
summary(med_result)

# 1. 분석용 데이터 준비 (주요 지표 추출 및 표준화)
# 인프라(hospital), 과정(heal), 결과(dead)를 모두 포함하여 지역의 '유형'을 정의합니다.
cluster_vars <- data %>% 
  dplyr::select(infra_hospital_per100k, ami_heal, ami_dead) %>% 
  na.omit() 

cluster_scale <- scale(cluster_vars)

# 2. 최적의 군집 수 결정 (Elbow Method)
# 그래프가 꺾이는 지점을 통해 몇 개의 그룹이 적절한지 판단합니다.
fviz_nbclust(cluster_scale, kmeans, method = "wss") +
  labs(title = "최적 군집 수 확인")

# 최적의 군집 수($k$)를 결정하기 위해 사용하는 엘보우 방법
set.seed(123)
km_res <- kmeans(cluster_scale, centers = 4, nstart = 25)

# 4. 시각화 (factoextra)
fviz_cluster(km_res, data = cluster_scale,
             geom = "point", # 지역명이 너무 많으면 point로, 적으면 text로 변경
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "응급의료 인프라 및 성과 기반 지역 군집화")

# 의료 우수형 (인프라 상, 성과 상) - 파란
# 인프라 부족형 (인프라 하, 성과 하) - 노란
# 효율 저하형 (인프라 상, 성과 하) - 회색
# 효율 우수형 (인프라 하, 성과 상) - 빨강

# 5. 군집별 특 (어떤 그룹이 문제인지 확인)
data_with_cluster <- data %>% 
  mutate(cluster = km_res$cluster)

cluster_summary <- data_with_cluster %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    avg_infra = mean(infra_hospital_per100k),
    avg_heal = mean(ami_heal),
    avg_dead = mean(ami_dead)
  )

print(cluster_summary)

#3D 그래프
df_3d <- as.data.frame(cluster_scale)
df_3d$cluster <- as.factor(km_res$cluster)

fig <- plot_ly(df_3d,
               x = ~infra_hospital_per100k,
               y = ~ami_heal,
               z = ~ami_dead,
               color = ~cluster,
               colors = "Set1", # 기존 그래프와 유사한 색상 팔레트
               type = 'scatter3d',
               mode = 'markers',
               marker = list(size = 3, opacity = 0.2,line = list(width = 0.1, color = 'rgb(0, 0, 0)'))) %>%
  layout(title = "응급의료 인프라 및 성과 3D 군집 분석",
         scene = list(xaxis = list(title = '인프라(병수)'),
                      yaxis = list(title = '치료 제공률'),
                      zaxis = list(title = '사망률')))

#그래프 출력
fig
#
# 가설 5
# 고령화 통제

cor.test(
  data$region_old,
  data$ami_dead,
  method = "spearman",
  exact = FALSE
)
# 지역의 노령화 정도(노령화 지수)와 심근경색 사망률 사이에 
# 유의미한 상관관계가 있는지 검정하는 것입니다.
model_final <- lm(
  ami_dead ~
    infra_hospital_per100k +
    infra_amb_per100k +
    ami_amb +
    ami_heal +
    ami_trans +
    region_old +
    metro,
  data = data
)

summary(model_final)

# 다중공선성
vif(model_final)

# 가설 6
# 정책 시뮬레이션

data$policy_hospital <- data$infra_hospital_per100k * 1.10
data$policy_amb <- data$infra_amb_per100k * 1.10

baseline <- predict(model_final)

policy <- predict(
  model_final,
  newdata = transform(
    data,
    infra_hospital_per100k = policy_hospital,
    infra_amb_per100k = policy_amb
  )
)

data$death_reduction <- baseline - policy

summary(data$death_reduction)

mean_reduction <- mean(data$death_reduction)

cat("평균 예측 사망률 감소:", mean_reduction)

# 정책효과 시각화

# 감소량을 기준으로 순위 매기기 및 3등분 그룹화
data_split <- data %>%
  mutate(rank = min_rank(desc(death_reduction)), # 감소량 높은 순으로 순위
         group = ntile(desc(death_reduction), 3)) %>% # 3개 그룹으로 분할
  mutate(group_label = case_when(
    group == 1 ~ "1그룹: 감소 효과 상위 지역",
    group == 2 ~ "2그룹: 감소 효과 중위 지역",
    group == 3 ~ "3그룹: 감소 효과 하위 지역"
  ))

# 3개의 그래프를 개별적으로 생성하거나 한 번에 보기 (facet_wrap 활용)
ggplot(data_split, aes(x = reorder(district, -death_reduction), y = death_reduction, fill = group_label)) +
  geom_bar(stat = "identity") +
  facet_wrap(~group_label, scales = "free_x", ncol = 1) + # 그룹별로 그래프 분할
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12) # 그룹 제목 강조
  ) +
  labs(
    title = "인프라 10% 확충 시 지역별 예상 사망률 감소량 (효과순 3등분)",
    x = "지역 (감소 효과 높은 순)",    y = "예상 감소량"
  )

