##############################################

# 시작하기 전에 R 메모리 정리하기
rm(list = ls())
gc(reset = T)

# 필요 패키지 불러오기
library(dplyr)
library(lubridate)
library(psych)
library(corrplot)
library(ggplot2)
library(nortest)
library(Hmisc)

# 데이터 불러오기
train <- read.csv("train.csv", header = TRUE)
store <- read.csv("store.csv", header = TRUE)

# 외부 데이터 불러오기
store_states <- read.csv("store_states.csv")
rossmann_opening_hours <- read.csv("rossmann_opening_hours.csv")
rossmann_store_geo <- read.csv("rossmann_store_geo.csv")

# 데이터 placeid그룹화 및 로스만시간, 위치자료 병합
rossmann_opening_hours_group <- rossmann_opening_hours %>% 
  group_by(placeid) %>% 
  summarise(OpenTime = mean(OpenTime),
            CloseTime = mean(CloseTime),
            OpenDuration = mean(OpenDuration))
rossmann_out_data <- merge(rossmann_opening_hours_group, rossmann_store_geo, by = "placeid", all.x = T)

# 주별 그룹화
rossmann_out_data <- rossmann_out_data %>% 
  group_by(State) %>% 
  summarise(OpenTime = round(mean(OpenTime), 1),
            CloseTime = mean(CloseTime),
            OpenDuration = mean(CloseTime) - mean(OpenTime),
            lat = mean(lat), lon = mean(lon),
            Elevation = mean(elevation),
            Mean_Hotel_Dist = mean(nearest_hotel),
            Mean_RailSta_Dist = mean(nearest_railstation))

# gtrend 데이터 불러오기, 기존 데이터와 병합
gtrend <- read.csv("GTrends_rossmann.csv")
atrain <- merge(train, store, by = "Store", all.x = T)
atrain <- merge(atrain, gtrend, by = "Date", all.x = T)

# Open 하지 않았거나, Sales가 전혀 없는 Store의 데이터 제거
atrain <- atrain %>% filter(Open != 0)
atrain <- atrain %>% filter(Sales != 0)

# Date 년, 월, 일로 분리
atrain <- atrain %>% mutate(Year = year(Date),
                            Month = month(Date),
                            Day = day(Date))


# 시계열 데이터임에도 불구하고, 시계열 분석이 불가능하다고 판단했다.
# 그래서 시간 데이터를 최소화하여, 다중공산성을 줄이고자 하였다.
# day단위가 year, month보다 분류수가 많아 제일 영향력이 크다고 생각했다.
# day 단위 데이터와 관련된 column 제거
atrain$Day <- NULL
atrain$Open <- NULL
atrain$DayOfWeek <-  NULL
atrain$SchoolHoliday <- NULL
atrain$StateHoliday <- NULL

# promotion 관련 column들을 하나의 column으로 묶어주기
# "현재 날짜에 프로모션을 진행하는가"라는 정보를 가진 하나의 column 생성
levels(atrain$PromoInterval) <- c(3, 2, 1, 0) # 3이면 프로모션 진행 x, 2이면 feb 2 / 5 / 8 / 11,
# 1이면 jan 1 / 4 / 7 / 10, 0이면 mar 3 / 6 / 9 / 12
atrain$PromoInterval <- as.numeric(atrain$PromoInterval) # 크기 비교를 위해서 factor에서 numeric으로 변경
atrain$PromoInterval <- ifelse(atrain$PromoInterval == 1, 3, # numeric으로 바꾸면 순서가 뒤죽박죽되서
                               ifelse(atrain$PromoInterval == 2, 2, # 원래대로 변경하기위해 ifelse문 사용
                                      ifelse(atrain$PromoInterval == 3, 1, 0)))
atrain <- atrain %>% mutate(Promo2Ing = ifelse(PromoInterval == 3, 0, # 이부분부터는 그전에 했던 내용
                                               ifelse(Promo2SinceYear > Year, 0,
                                                      ifelse(Promo2SinceYear == Year, 
                                                             ifelse(Promo2SinceWeek > week(Date), 0, 
                                                                    ifelse(Month %% 3 == PromoInterval, 1, 0)), 0))))

atrain$Promo2SinceYear <- NULL # 불필요 column 제거
atrain$Promo2SinceWeek <- NULL # 불필요 column 제거
atrain$PromoInterval <- NULL # 불필요 column 제거
atrain$Date <- NULL # 불필요 column 제거


# CompetitionOpen 관련 column 2개를 하나로 합치기
# "경쟁사가 현재 년,월에 존재하는가"라는 정보를 가진 하나의 column 생성
atrain <- atrain %>% mutate(CompExist = ifelse(is.na(CompetitionOpenSinceYear), 0, 
                                               ifelse(Year < CompetitionOpenSinceYear, 0,
                                                      ifelse(Year == CompetitionOpenSinceYear, 
                                                             ifelse(Month < CompetitionOpenSinceMonth, 0, 1), 1))))

atrain$CompetitionOpenSinceYear <- NULL # 불필요 column 제거
atrain$CompetitionOpenSinceMonth <- NULL # 불필요 column 제거



# 월,년도 기준으로 Store 정렬
atrain <- atrain %>%
  select(Store, Year, Month, Sales, Customers, CompExist, Promo2Ing, Trend) %>% 
  group_by(Store, Year, Month) %>% 
  summarise(Sales = round(median(Sales), 0),
            Customers = round(median(Customers), 0),
            CompExist = mean(CompExist),
            Promo2Ing = mean(Promo2Ing),
            Trend = mean(Trend))

# 데이터 값 정리
atrain <- merge(atrain, store, by = "Store", all.x = T)
atrain$CompetitionOpenSinceYear <- NULL
atrain$CompetitionOpenSinceMonth <- NULL
atrain$Promo2SinceYear <- NULL
atrain$Promo2SinceWeek <- NULL
atrain$PromoInterval <- NULL
atrain$Promo2Ing <- ifelse(atrain$Promo2Ing == 0, 0, 1) # 0.xxx 이라도 해당 달에 프로모션을 진행한다고 판단해서 1로 변경
atrain$Trend <- round(atrain$Trend, 0) # trend 정수형 데이터로 반올림

# 더미변수 생성
atrain <- atrain %>% mutate(StoreTypeb = ifelse(StoreType == 'b', 1, 0),
                            StoreTypec = ifelse(StoreType == 'c', 1, 0),
                            StoreTyped = ifelse(StoreType == 'd', 1, 0))
atrain <- atrain %>% mutate(Assortmentb = ifelse(Assortment == 'b', 1, 0),
                            Assortmentc = ifelse(Assortment == 'c', 1, 0))

atrain$StoreType <- NULL # 불필요 column 제거
atrain$Assortment <- NULL # 불필요 column 제거


# CompetitionDistance의 NA를 0으로 치환하기
atrain$CompetitionDistance <- ifelse(is.na(atrain$CompetitionDistance), 0, atrain$CompetitionDistance)


# State데이터와 train데이터 병합
atrain <- merge(atrain, store_states, by = "Store", all.x = T)
rosstrain <- merge(atrain, rossmann_out_data, by = "State", all.x = T)
rosstrain <- rosstrain %>% filter(State != "HB,NI")


# train과 test 데이터로 분리
sample_idx <- sample(1 : nrow(rosstrain), size = round(0.7 * nrow(rosstrain)))
rosstrain_train <- rosstrain[sample_idx, ]
rosstrain_test <- rosstrain[-sample_idx, ]


# 정규성 검사
# str(rosstrain_train)으로 확인 후, factor열 제외 시키기
ross.nor1 <- list()
for(i in 2:16){
  ross.nor <- ad.test(rosstrain_train[ , i])
  ross.nor1 <- list(ross.nor, ross.nor1)
}


# Boxplot으로 이상치 값 확인하기
par(mfrow = c(3, 5))
for(i in 2:16){
  boxplot(rosstrain_train[ , i],
          main = colnames(rosstrain_train[i]))
}
par(mfrow = c(1, 1))

# 이분법으로 된 데이터를 제외하고, 이상치가 보이는 데이터의 이상치 범위 확인
outliers1 <- boxplot.stats(rosstrain_train$Sales)$out
outliers2 <- boxplot.stats(rosstrain_train$Customers)$out
outliers3 <- boxplot.stats(rosstrain_train$CompetitionDistance)$out
outliers4 <- boxplot.stats(rosstrain_train$Trend)$out

# 각각의 분산값을 구하여 어떤 데이터 형태를 갖는지 유추해보기

var(rosstrain_train$Sales)
var(rosstrain_train$Customers)
var(rosstrain_train$CompetitionDistance)
var(rosstrain_train$Trend)
# 결과적으로 분산값이 굉장히 큰 값을 보이므로, 이상치를 제거할 필요성이 사라졌음


# train 데이터의 각 변수간의 상관관계 데이터로 확인하기
ggplot(rosstrain_train,
       aes(x = log(Customers), y = log(Sales))) + 
  geom_point(alpha = 0.2) + geom_smooth() # Sales와 Customoers간의 그래프

yg <- aggregate(Sales~Year, atrain, mean)
plot(yg$Year, yg$Sales, type = "o") # Sales와 Year간의 그래프

mg <- aggregate(Sales~Month, atrain, mean)
plot(mg$Month, mg$Sales, type = "o") # Sales와 Month간의 그래프


cg <- aggregate(Sales~CompetitionDistance, rosstrain_train, mean)
plot(cg$CompetitionDistance, cg$Sales, type = "o") # Sales와 CompetitionDistance간의 그래프

tg <- aggregate(Sales~Trend, rosstrain_train, mean)
plot(tg$Trend, tg$Sales, type = "o") # Sales와 Trend간의 그래프




# corr.test로 각 변수들끼리의 연관성 확인하기 (numeric variables만 돌림)
# str(rosstrain_train)으로 numeric variables 구별하기
corr.test(rosstrain_train[ , c(2:6, 9:10)], use = "pairwise", method = "pearson")

# corr.test결과 시각화해서 확인하기
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor_5 <- rcorr(as.matrix(rosstrain_train[, c(2 : 6, 9 : 10)]))
corrplot(cor_5$r,
         method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black",
         tl.col = "darkblue", tl.srt = 45,
         p.mat = cor_5$P, sig.level = 0.01,  
         diag = FALSE 
)

# MSE 사용자 함수 및 계산
rmse(rossmann_obs, rossmannhat_lm)

rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}

# 선형회귀식 모델링
# 임의 모델 종류별로 MSE / R-Square 계산하기
rossmodel1 <- lm(Sales ~ State + Year + Month + Customers + Promo2Ing + 
                   Trend + CompetitionDistance + Promo2 + StoreTypeb + 
                   StoreTyped + Assortmentb + Assortmentc, data = rosstrain_train) # 1035.938 / 0.8202
rossmodel2 <- lm(Sales ~ State + Year + Month + Customers + Trend, 
                 data = rosstrain_train) # 1360.3 / 0.6901
rossmodel3 <- lm(Sales ~ Customers + StoreTypeb + StoreTypec + StoreTyped, 
                 data = rosstrain_train) # 1152.717 / 0.7783
rossmodel4 <- lm(Sales ~ Customers + Trend, 
                 data = rosstrain_train) # 1388.732 / 0.6754
rossmodel5 <- lm(Sales ~ Customers, 
                 data = rosstrain_train) # 1393.042 / 0.6721
rossmodel6 <- lm(Sales ~ Trend,
                 data = rosstrain_train) # 2445.111 / 0.0060
rossmodel7 <- lm(Sales ~ .- CompExist - StoreTypec, 
                 data = rosstrain_train) # 1029.554 / 0.8217 - best!!!!!
ttt <- rosstrain_train %>% filter(State != "HE")
tttt <- rosstrain_test %>% filter(State != "HE")
rossmodel8 <- lm(Sales ~ .- CompExist - StoreTypec,
                 data = ttt) # 1032.648 / 0.8245 
rossmodel9 <- lm(Sales ~ .- Store - CompExist - StoreTypec,
                 data = rosstrain_train) # 1030.174 / 0.8216
rossmodel10 <- lm(Sales ~ .- Promo2Ing - CompExist - StoreTypec,
                  data = rosstrain_train) # 1029.956 / 0.8216
rossmodel11 <- lm(Sales ~ .- Store - Promo2Ing - CompExist - StoreTypec,
                  data = rosstrain_train) # 1030.581 / 0.8214
rossmodel12 <- lm(Sales ~ .- Promo2 - Store - Promo2Ing - CompExist - StoreTypec,
                  data = rosstrain_train) # 1031.915 / 0.8213



# Variables Selection 단계 및 모델 선택하기

rossmann.forward1 <- step(lm(Sales ~ Customers, data = rosstrain_train), direction = "forward")
summary(rossmann.forward1)
str(rossmann.forward1)
sort(abs(rossmann.forward1$coefficients), decreasing = T)



rossmann.backward1 <- step(lm(Sales ~ ., data = rosstrain_train), direction = "backward")
summary(rossmann.backward1)
str(rossmann.backward1)
sort(abs(rossmann.backward1$coefficients), decreasing = T)

rossmann.both1 <- step(lm(Sales ~ ., data = rosstrain_train), direction = "both")
summary(rossmann.backward1)
str(rossmann.backward1)
sort(abs(rossmann.backward1$coefficients), decreasing = T)
# 최종적으로 "backward"로 선정해서 사용

# 다중공선성(Multicollinearity) 구하기

car::vif(rossmodel7)

# 독립변수들의 영향력 크기의 비교

lm.beta1 <- lm.beta::lm.beta(rossmodel7)

coeffi1 <- sort(abs(lm.beta1$standardized.coefficients), decreasing = T)

barplot(coeffi1[-21],
        space = 0.8,
        main = "Sales ~ variables' correlation coefficients ",
        xlab = "independent variables")

# test의 Sales 값 예측하기
rossmannhat_lm <- predict(rossmodel7, newdata = rosstrain_test, interval = "predict")
nrow(rossmannhat_lm) # 9835
# 예측치의 정답률 확인
rossmannhat_lm <- cbind(rossmannhat_lm, rosstrain_test$Sales)
rossmannhat_lm <- as.data.frame(rossmanhat_lm)
rossmannhat_lm <- mutate(rossmannhat_lm, TF = ifelse((rossmannhat_lm$V4 > rossmannhat_lm$lwr & rossmannhat_lm$V4 < rossmannhat_lm$upr), 1, 0))
nrow(rossmannhat_lm)
table(rossmannhat_lm$TF)
(9400/9835)*100
