### 데이터 불러오기 ###
df <- read.csv("final_data.csv",fileEncoding = "CP949")
str(df)
summary(df)

### 범주형 변수 지정 ###
df$metro <- factor(df$metro,levels = c("비수도권", "수도권"))
str(df) # 확인 

### 확률 기반 분석(로지스틱 회귀)을 위해 비율(%) 데이터를 확률(0~1)값으로 변환###
df$ami_amb   <- df$ami_amb / 100
df$ami_heal  <- df$ami_heal / 100
df$ami_trans <- df$ami_trans / 100
df$ami_dead  <- df$ami_dead / 100
str(df) # 확인

### 패키지 ###
# install.packages("effsize")
# install.packages("tidyr")
# install.packages("corrplot")
# install.packages("mediation")
# install.packages("car")


library(effsize)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(mediation)
library(car)

#############################################################################

##### 1단계 : 인프라 격차 검증(인구기준 /  면적 기준 나누어서 각각 분석) #####

### 인구 기준 인프라 - 응급기관 수 ###
# H0 : 수도권과 비수도권의 인구(밀도) 대비 응급기관 수 차이 없음
# H1 :수도권의 인구(밀도) 대비 응급기관 수가 더 많음

# 변수 탐색 - EDA
boxplot(df$infra_hospital_per100k)
hist(df$infra_hospital_per100k)

# 정규성 확인
shapiro.test(df$infra_hospital_per100k)
# p-value < 2.2e-16 => 정규성 불만족 : Wilcoxon 진행

# wilcoxon test
wilcox.test(infra_hospital_per100k ~ metro, data = df)
# p-value : 2.375e-09

# 비수도권 vs 수도권 어느쪽이 높은지 확인 - 평균비교
aggregate(infra_hospital_per100k ~ metro, data = df, mean)
# 1 비수도권              1.6534486
# 2   수도권              0.7185648

# 효과크기 확인
cohen.d(infra_hospital_per100k ~ metro, data = df)
# d estimate: 0.7098288 (medium)


### 인구 기준 인프라 - 구급차 수 ###
# H0 : 수도권과 비수도권의 인구대비 구급차 수 차이 없음
# H1 :수도권의 인구대비 구급차 수가 더 많음

# 변수 탐색 - EDA
boxplot(df$infra_amb_per100k)
hist(df$infra_amb_per100k)

# 정규성 확인
shapiro.test(df$infra_amb_per100k)
# p-value : 9.019e-15 => 정규성 불만족 : Wilcoxon 진행

# wilcoxon
wilcox.test(infra_amb_per100k ~ metro, data = df)
# p-value : 1.332e-15


# 비수도권 vs 수도권 어느쪽이 높은지 확인 - 평균비교
aggregate(infra_amb_per100k ~ metro, data = df, mean)
# 1 비수도권          7.966609
# 2   수도권          2.621225

# 효과크기 확인
cohen.d(infra_amb_per100k ~ metro, data = df)
# d estimate: 0.9778843 (large)
# 인프라 중 구급차 자원 격차가 더 큼

#인구 기준 인프라 시각화
infra_plot <- data.frame(metro = c("비수도권", "수도권"),
                         hospital = c(1.6534486, 0.7185648),
                         ambulance = c(7.966609, 2.621225))

infra_long <- infra_plot %>%
  pivot_longer(cols = c(hospital, ambulance),
               names_to = "type",
               values_to = "value")

infra_long$type <- factor(infra_long$type,
                          levels = c("hospital", "ambulance"),
                          labels = c("응급기관 수", "구급차 수"))

ggplot(infra_long, aes(x = type, y = value, fill = metro)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = round(value, 2)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 5,
    fontface = "bold") +
  scale_fill_manual(values = c("비수도권" = "#4E79A7", "수도권" = "#E15759")) +
  
  labs(
    title = "수도권-비수도권 응급의료 인프라 비교",
    subtitle = "인구 10만명당 기준",
    x = "",
    y = "인프라 수",
    fill = ""
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(plot.title = element_text(face = "bold",  size = 20),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(face = "bold", size = 12),
        legend.position = "top")

### 면적 기준 인프라 - 응급기관 수 ###
# EDA
boxplot(df$infra_hospital_per_area)
hist(df$infra_hospital_per_area)

# 정규성
shapiro.test(df$infra_hospital_per_area)

# Wilcoxon
wilcox.test(infra_hospital_per_area ~ metro, data = df)

# 평균 비교
aggregate(infra_hospital_per_area ~ metro, data = df, mean)

# 효과크기
cohen.d(infra_hospital_per_area ~ metro, data = df)

### 면적 기준 인프라 - 구급차 수 ###

# EDA
boxplot(df$infra_amb_per_area)
hist(df$infra_amb_per_area)

# 정규성
shapiro.test(df$infra_amb_per_area)

# Wilcoxon
wilcox.test(infra_amb_per_area ~ metro, data = df)

# 평균 비교
aggregate(infra_amb_per_area ~ metro, data = df, mean)

# 효과크기
cohen.d(infra_amb_per_area ~ metro, data = df)

# 면적 기준 인프라 시각화
area_plot <- data.frame(metro = c("비수도권", "수도권"),
                        hospital = c(
                          mean(df$infra_hospital_per_area[df$metro == "비수도권"]),
                          mean(df$infra_hospital_per_area[df$metro == "수도권"])
                        ),
                        
                        ambulance = c(
                          mean(df$infra_amb_per_area[df$metro == "비수도권"]),
                          mean(df$infra_amb_per_area[df$metro == "수도권"])
                        )
)

# long 형태 변환
area_long <- area_plot %>%
  pivot_longer(
    cols = c(hospital, ambulance),
    names_to = "type",
    values_to = "value"
  )

# 변수 이름 변경
area_long$type <- factor(
  area_long$type,
  levels = c("hospital", "ambulance"),
  labels = c("응급기관 수", "구급차 수")
)

# 시각화
ggplot(area_long,
       aes(x = type, y = value, fill = metro)) +
  
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6,
    alpha = 0.9
  ) +
  
  geom_text(
    aes(label = round(value, 3)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = c(
      "비수도권" = "#4E79A7",
      "수도권" = "#E15759"
    )
  ) +
  
  labs(title = "수도권-비수도권 면적 기준 응급의료 인프라 비교",
       subtitle = "지역 면적(km²) 기준",
       x = "",
       y = "면적당 인프라 수",
       fill = ""
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(face = "bold", size = 12),
        axis.title.y = element_text(size = 14),
        legend.position = "top"
  )
#############################################################################

##### 2단계 : 인프라와 치료 역량 상관분석(인구/면적, 응급기관/구급차 기반 나누어 분석) #####

####인구 기준 ####
### 응급기관 기반 ###
# 치료역량 변수 지정
infra_treat_hospital <- df[, c("infra_hospital_per100k", "ami_amb", "ami_heal", "ami_trans")]
# 상관분석
cor(infra_treat_hospital)

### 구급차 기반 ###
# 치료역량 변수 지정
infra_treat_amb <- df[, c("infra_amb_per100k", "ami_amb", "ami_heal", "ami_trans")]
# 상관분석
cor(infra_treat_amb)

### 단순 회귀 ###
# 1. 기관 수 -> 구급차 이용률
model_amb <- lm(ami_amb ~ infra_hospital_per100k, data = df)
summary(model_amb)
# 2. 기관 수 -> 치료 제공률
model_heal <- lm(ami_heal ~ infra_hospital_per100k, data = df)
summary(model_heal)
# 3. 기관 수 -> 전원률
model_trans <- lm(ami_trans ~ infra_hospital_per100k, data = df)
summary(model_trans)


# 4. 구급차 수 -> 구급차 이용률
model_amb2 <- lm(ami_amb ~ infra_amb_per100k, data = df)
summary(model_amb2)
# 5. 구급차 수 -> 치료 제공률
model_heal2 <- lm(ami_amb ~ infra_amb_per100k, data = df)
summary(model_amb2)
# 6. 구급차 수 -> 전원률
model_trans2 <- lm(ami_trans ~ infra_amb_per100k, data = df)
summary(model_trans2)
# Estimate : 0.0331592, R-squared:  0.0456  -> 약간의 가능성

# 시각화
ggplot(df, aes(x = infra_amb_per100k, y = ami_trans)) +
  geom_point(color = "#4E79A7", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#E15759", linewidth = 1.2) +
  labs(
    title = "구급차 인프라와 전원률의 관계",
    subtitle = "인구 10만명당 구급차 수 증가에 따른 전원률 변화",
    x = "구급차 수 (인구 10만명당)",
    y = "전원률"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 11))

#### 면적 기준 ####
### 응급기관 기반 ###
# 변수지정, 상관분석
infra_treat_hospital_area <- df[, c("infra_hospital_per_area", "ami_amb", "ami_heal", "ami_trans")]
cor(infra_treat_hospital_area)

### 구급차 기반 ###
# 변수지정, 상관분석
infra_treat_amb_area <- df[, c("infra_amb_per_area", "ami_amb", "ami_heal", "ami_trans")]
cor(infra_treat_amb_area)

### 단순 회귀###
# 1. 기관 수 -> 구급차 이용률
model_amb_area <- lm(ami_amb ~ infra_hospital_per_area, data = df)
summary(model_amb_area)
# 2. 기관 수 -> 치료 제공률
model_heal_area <- lm(ami_heal ~ infra_hospital_per_area, data = df)
summary(model_heal_area)
# 3. 기관 수 -> 전원률
model_trans_area <- lm(ami_trans ~ infra_hospital_per_area, data = df)
summary(model_trans_area)


# 4. 구급차 수 -> 구급차 이용률
model_amb2_area <- lm(ami_amb ~ infra_amb_per_area, data = df)
summary(model_amb2_area)
# 5. 구급차 수 -> 치료 제공률
model_heal2_area <- lm(ami_amb ~ infra_amb_per_area, data = df)
summary(model_amb2_area)
# 6. 구급차 수 -> 전원률
model_trans2_area <- lm(ami_trans ~ infra_amb_per_area, data = df)
summary(model_trans2_area)

# 시각화
ggplot(df, aes(x = infra_amb_per_area, y = ami_trans)) +
  geom_point(color = "#2E8B57", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#0B3B2E", linewidth = 1.2) +
  labs(
    title = "면적 기준 구급차 인프라와 전원률 관계",
    subtitle = "면적당 구급차 수 증가에 따른 전원률 변화",
    x = "구급차 수 (면적 기준)",
    y = "전원률"
  ) +
  theme_minimal(base_size = 14) +
  
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 11))

######################################################################

##### 3단계 : 치료역량 -> 사망률 ######

# 상관분석
death_df <- df[, c("ami_dead", "ami_amb", "ami_heal", "ami_trans")]
cor(death_df)
#              ami_dead     ami_amb   ami_heal   ami_trans
#ami_dead   1.00000000  0.01211914 -0.1173510  0.01052963
#ami_amb    0.01211914  1.00000000  0.1293856 -0.13112786
#ami_heal  -0.11735103  0.12938560  1.0000000 -0.90734034
#ami_trans  0.01052963 -0.13112786 -0.9073403  1.00000000

# 시각화
corrplot(cor(death_df), method = "color",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.cex = 1,
         tl.col = "black")

# 단순회귀 - 각 변수
summary(lm(ami_dead ~ ami_heal, data = df))
# Estimate :  -0.06624 , p-value : 0.06394

# 시각화
ggplot(df, aes(x = ami_heal, y = ami_dead)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "입원치료 제공률과 사망률 관계",
    subtitle = "치료제공률 증가 시 사망률 감소 경향",
    x = "입원치료 제공률",
    y = "심근경색 사망률"
  ) +
  theme_minimal(base_size = 14)

summary(lm(ami_dead ~ ami_amb, data = df))

summary(lm(ami_dead ~ ami_trans, data = df))
# 시각화(대립가설과 다른 결과 시각화)
ggplot(df, aes(x = ami_trans, y = ami_dead)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "전원율과 사망률 관계",
       subtitle = "전원체계 활성화와 관련 가능성",
       x = "전원율",
       y = "심근경색 사망률") +
  theme_minimal(base_size = 14)

# 다중회귀
model_treat <- lm(ami_dead ~ ami_amb + ami_heal + ami_trans, data = df)
summary(model_treat)
# 다중공선성 확인
vif(model_treat)
# 결과 VIF -> ami_amb : 1.01, ami_heal : 5.45, ami_trans : 5.44
# 치료제공률과 전원률 사이에 높은 상관이 있음 (의료체계 특성상 자연스러운 결과)
# 치료 역량이 높은 지역일수록 자체 치료 비율이 높고 전원필요성이 감소
# 치료 어려운 지역일수록 전원율이 높아질 수 밖에 없는 상황 
######################################################################

##### 4단계 : 매개효과- 인프라 + 치료역량 -> 사망률 #####

# 병원 수가 많을 수록 입원치료 제공률이 높아지는가
med_model <- lm(ami_heal ~ infra_hospital_per100k, data = df)
# 병원 수와 치료 제공률을 동시에 넣었을 때 사망률에 어떻게 영향을 주는가
out_model <- lm(ami_dead ~ infra_hospital_per100k + ami_heal, data = df)
# 병원 수 증가 효과 중 얼마나 치료제공률을 통해 전달되는가
med_result <- mediate(med_model, out_model, treat = "infra_hospital_per100k", mediator = "ami_heal", boot = TRUE, sims = 1000)
summary(med_result)

######################################################################

##### 5단계 : 고령화 영향 #####

# EDA
boxplot(df$region_old)
hist(df$region_old)
boxplot(df$ami_dead)
hist(df$ami_dead)


# 상관분석
cor(df[, c("region_old", "ami_dead")],method = "spearman")
cor.test(df$region_old, df$ami_dead, method = "spearman")

plot(df$region_old, df$ami_dead, pch = 19)
abline(lm(ami_dead ~ region_old, data = df), col = "skyblue", lwd = 2)

# 통합모형
model_final <- lm(ami_dead ~ ami_heal + ami_trans + ami_amb + region_old, data = df)
summary(model_final)

######################################################################

##### 6단계 :  정책 시뮬레이션 #####
## 평균보다 낮은 지역-> 평균 수준 개선 시나리오

#시나리오 A - 치료제공률 개선
mean(df$ami_heal)
low_heal <- df[df$ami_heal < mean(df$ami_heal), ]
scenario_A <- low_heal

scenario_A$ami_heal <- mean(df$ami_heal)
base_pred_A <- predict(
  model_final,
  newdata = low_heal,
  type = "response"
)
improve_pred_A <- predict(
  model_final,
  newdata = scenario_A,
  type = "response"
)
mean(base_pred_A)

mean(improve_pred_A)

(mean(base_pred_A) - mean(improve_pred_A)) / mean(base_pred_A) * 100


# 시나리오 B - 전원체계 개선
low_trans <- df[df$ami_trans < mean(df$ami_trans), ]
scenario_B <- low_trans

scenario_B$ami_trans <- mean(df$ami_trans)
base_pred_B <- predict(
  model_final,
  newdata = low_trans,
  type = "response"
)

improve_pred_B <- predict(
  model_final,
  newdata = scenario_B,
  type = "response"
)
mean(base_pred_B)

mean(improve_pred_B)

(mean(base_pred_B) - mean(improve_pred_B)) / mean(base_pred_B) * 100


scenario_result <- data.frame(
  scenario = c(
    "치료제공률 개선 전",
    "치료제공률 개선 후",
    "전원체계 개선 전",
    "전원체계 개선 후"
  ),
  
  death_rate = c(
    mean(base_pred_A),
    mean(improve_pred_A),
    mean(base_pred_B),
    mean(improve_pred_B)
  )
)

scenario_result

# 시각화
scenario_result <- data.frame(
  scenario = c(
    "전원체계 개선 전",
    "전원체계 개선 후",
    "치료제공률 개선 전",
    "치료제공률 개선 후"
  ),
  
  death_rate = c(
    mean(base_pred_B),
    mean(improve_pred_B),
    mean(base_pred_A),
    mean(improve_pred_A)
  )
)

ggplot(scenario_result,
       aes(x = scenario, y = death_rate, fill = scenario)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(death_rate, 3)), vjust = -0.5, size = 5) +
  
  # 감소율 표시
  annotate("text", x = 1.5, y = 0.094, label = "↓10.1%",
           size = 6,
           fontface = "bold",
           color = "#1565C0"
  ) +
  
  annotate("text",
           x = 3.5,
           y = 0.099,
           label = "↓19.0%",
           size = 6,
           fontface = "bold",
           color = "#00897B"
  ) +
  
  scale_fill_manual(
    values = c(
      "#90CAF9",  # 전원 개선 전
      "#1565C0",  # 전원 개선 후
      "#80CBC4",  # 치료 개선 전
      "#00897B"   # 치료 개선 후
    )
  ) +
  
  labs(
    title = "정책 시나리오별 예상 심근경색 사망률 변화",
    subtitle = "취약지역을 전국 평균 수준으로 개선한 가상 시나리오",
    x = "",
    y = "예상 사망률"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(legend.position = "none",
        plot.title = element_text(
          face = "bold",
          size = 20
        ),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 14))



