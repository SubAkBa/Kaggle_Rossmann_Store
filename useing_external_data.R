rm(list = ls())
gc(reset = T)
# 경로설정
library(dplyr)
library(lubridate)
library(psych)
library(corrplot)
library(ggplot2)

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

# 일별 데이터 관련 column 제거
atrain <- atrain %>% filter(Open != 0)
atrain <- atrain %>% filter(Sales != 0)
atrain$Open <- NULL
atrain$DayOfWeek <-  NULL
atrain$SchoolHoliday <- NULL
atrain$StateHoliday <- NULL

# Date 년, 월, 일로 분리
atrain <- atrain %>% mutate(Year = year(Date),
                            Month = month(Date),
                            Day = day(Date))

# 현재 날짜에 프로모션을 진행하는가
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
atrain$Promo2SinceYear <- NULL
atrain$Promo2SinceWeek <- NULL
atrain$PromoInterval <- NULL
# 경쟁사가 현재 월, 년도에 존재하는가
atrain <- atrain %>% mutate(CompExist = ifelse(is.na(CompetitionOpenSinceYear), 0, 
                                               ifelse(Year < CompetitionOpenSinceYear, 0,
                                                      ifelse(Year == CompetitionOpenSinceYear, 
                                                             ifelse(Month < CompetitionOpenSinceMonth, 0, 1), 1))))
# 불필요 데이터 제거
atrain$CompetitionOpenSinceYear <- NULL
atrain$CompetitionOpenSinceMonth <- NULL
atrain$Date <- NULL
atrain$Day <- NULL

# 월, 년도 별 가게 집계
atrain <- atrain %>%
  select(Store, Year, Month, Sales, Customers, CompExist, Promo2Ing, Trend) %>% 
  group_by(Store, Year, Month) %>% 
  summarise(Sales = round(median(Sales), 0),
            Customers = round(median(Customers), 0),
            CompExist = mean(CompExist),
            Promo2Ing = mean(Promo2Ing),
            Trend = mean(Trend))

# 데이터 병합(그룹화 하면서 다 같이 가져올경우 이상한 값으로 바뀔수 있기 때문에)
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
atrain$StoreType <- NULL
atrain$Assortment <- NULL
atrain$CompetitionDistance <- ifelse(is.na(atrain$CompetitionDistance), 0, atrain$CompetitionDistance)

# State데이터와 train데이터 병합
atrain <- merge(atrain, store_states, by = "Store", all.x = T)
rosstrain <- merge(atrain, rossmann_out_data, by = "State", all.x = T)
rosstrain <- rosstrain %>% filter(State != "HB,NI")

# 소수점 값들을 가지는 열들 제거 (회귀모델에 의미가 없음 -> NA로 뜸)
ross_geo_time <- rosstrain %>% sample_n(1000)
ross_geo_ti
ross_geo_time %>% ggplot(aes(x = State, y = lat,  colour = as.factor(Year))) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "forestgreen"))
ross_geo_time %>% ggplot(aes(x = Store, y = lat,  colour = as.factor(Year))) +
  geom_point() +
  geom_jitter() +
  scale_color_manual(values = c("red", "blue", "forestgreen"))
ross_geo_time %>% ggplot(aes(x = State, y = lon,  colour = as.factor(Year))) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "forestgreen"))
ross_geo_time %>% ggplot(aes(x = Store, y = lon,  colour = as.factor(Year))) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "forestgreen"))

rosstrain[ , c(17 : 24)] <- NULL


# train과 test 데이터로 분리
sample_idx <- sample(1 : nrow(rosstrain), size = round(0.7 * nrow(rosstrain)))
rosstrain_train <- rosstrain[sample_idx, ]
rosstrain_test <- rosstrain[-sample_idx, ]

# MSE 사용자 함수 및 계산
rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}

# 밑에 모델들을 만들고 여기에 predict 첫번째 파라미터의 모델만 바꿔서 넣어주면 되요 !
rossmann_obs <- rosstrain_test$Sales
rossmannhat_lm <- predict(rossmodel12, newdata = rosstrain_test)
rmse(rossmann_obs, rossmannhat_lm)
colnames(rosstrain)

# MSE / R-Square
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
                 data = rosstrain_train) # 1029.554 / 0.8217
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
summary(rossmodel7)




# 각 변수별 그래프 그려보기

head(rosstrain_train)


# Boxplot으로 이상치 값 확인하기
str(train)

boxplot(train$DayOfWeek)
boxplot(rosstrain$Sales)
boxplot(train$Customers)
boxplot(train$CompetitionDistance)

outliers1 <- boxplot.stats(train$DayOfWeek)$out
outliers2 <- boxplot.stats(train$Sales)$out
outliers3 <- boxplot.stats(train$Customers)$out
outliers4 <- boxplot.stats(rosstrain$CompetitionDistance)$out

# 각각의 분산값을 구하여 어떤 데이터 형태를 갖는지 유추해보기

var(rosstrain$Sales)
var(rosstrain$Customers)
var(rosstrain$CompetitionDistance)

# train 데이터에 number index를 id colunm으로 생성 및 삽입하기
nrow(train)


train.id <- matrix( 1:844338 , nrow = nrow(train), ncol = 1) 

train <- cbind(train, train.id)
train <- mutate(train, Id = train.id)
train$train.id <- NULL
train$Id <- as.numeric(train$Id)

View(train)
# corr.test로 각 변수들끼리의 연관성 확인하기 (numeric variables만 돌림)
colnames(rosstrain)
str(rosstrain)
corr.test(rosstrain[ , c(3 : 6, 9 : 10, )], use = "pairwise", method = "pearson")
table(is.na(rosstrain$Mean_RailSta_Dist))
colnames(rosstrain)


# Variables Selection 단계 및 모델 선택하기

rossmann.forward1 <- step(rossmann.model, direction = "forward")
summary(rossmann.forward1)
str(rossmann.forward1)
sort(abs(rossmann.forward1$coefficients), decreasing = T)

rossmann.backward1 <- step(rossmann.model, direction = "backward")
summary(rossmann.backward1)
str(rossmann.backward1)
sort(abs(rossmann.backward1$coefficients), decreasing = T)

# 다중공선성(Multicollinearity) 구하기

car::vif(rossmann.model)

# 독립변수들의 영향력 크기의 비교

lm.beta1 <- lm.beta::lm.beta(rossmann.model)

coeffi1 <- sort(abs(lm.beta1$standardized.coefficients), decreasing = T)

barplot(coeffi1[-21],
        space = 0.8,
        main = "Sales ~ variables' correlation coefficients ",
        xlab = "independent variables")

# test의 Sales 값 예측하기

predict(rossmann.model, newdata = test)
test.sales <- matrix(unlist(predict(rossmann.model, newdata = test)), nrow = nrow(test), ncol = 1)

View(test.sales)
nrow(test.sales)
nrow(test)


################################
rmse <- function(yi, yhat_i){
  sqrt(mean((yi-yhat_i)^2))
}

y_obs <- testset$solo_WinRatio
yhat_lm <- predict(model, newdata = test)

rmse(y_obs, yhat_lm)
