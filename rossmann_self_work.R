install.packages("lubridate")
install.packages("dummies")
library(nortest)
library(car)
library(anytime)

rm(list = ls())
gc(reset = T)

dir(pattern = "csv")

ss <- read.csv(file = "sample_submission.csv",
               header = TRUE)
str(ss)
head(ss)

store <- read.csv(file = "store.csv",
                  header = TRUE)
str(store)
head(store)

test <- read.csv(file = "test.csv",
                 header = TRUE)
str(test)
head(test)
test <- test %>% arrange(Id)

train <- read.csv(file = "train.csv",
                  header = TRUE)


library(dplyr) # 시작----
library(lubridate)
library(car)
library(dummies)
dtrain <- merge(store, train, by = "Store", all.y = TRUE)
dtrain <- dtrain %>% filter(Open == 1)
dtrain <- dtrain %>% filter(Sales != 0)
head(dtrain)
str(dtrain)
summary(dtrain)
colnames(dtrain)
vif(lm(Sales ~ Customers + StoreTypeb + StoreTypec + StoreTyped + Assortmentb + Assortmentc +
         StateHolidaya + StateHolidayb + StateHolidayc + PromoIntervalJ + PromoIntervalF + PromoIntervalM +
         CompOpenYear1961 + CompOpenYear1990 + CompOpenYear1994 + CompOpenYear1995 + CompOpenYear1998 +
         CompOpenYear1999 + CompOpenYear2000 + CompOpenYear2001 + CompOpenYear2002, data = dtrain))

vif(lm(Sales ~ ., data = dtrain))

dtrain <- dtrain %>% mutate(StoreTypeb = ifelse(StoreType == 'b', 1, 0),
                            StoreTypec = ifelse(StoreType == 'c', 1, 0),
                            StoreTyped = ifelse(StoreType == 'd', 1, 0))
summary(lm(Sales ~ Customers + StoreTypeb + StoreTypec + StoreTyped, data = dtrain))
vif(lm(Sales ~ Customers + StoreTypeb + StoreTypec + StoreTyped, data = dtrain))
summary(lm(Sales ~ StoreTypeb + StoreTypec + StoreTyped, data = dtrain))
vif(lm(Sales ~ StoreTypeb + StoreTypec + StoreTyped, data = dtrain)) 
summary(lm(Customers ~ StoreTypeb + StoreTypec + StoreTyped, data = dtrain))
vif(lm(Customers ~ StoreTypeb + StoreTypec + StoreTyped, data = dtrain)) 
dtrain$StoreType <- NULL
dtrain <- dtrain %>% mutate(Assortmentb = ifelse(Assortment == 'b', 1, 0),
                            Assortmentc = ifelse(Assortment == 'c', 1, 0))
summary(lm(Sales ~ Customers + Assortmentb + Assortmentc, data = dtrain))
summary(lm(Sales ~ Assortmentb + Assortmentc, data = dtrain))
summary(lm(Customers ~ Assortmentb + Assortmentc, data = dtrain))
dtrain$Assortment <- NULL
dtrain <- dtrain %>% mutate(StateHolidaya = ifelse(StateHoliday == 'a', 1, 0),
                            StateHolidayb = ifelse(StateHoliday == 'b', 1, 0),
                            StateHolidayc = ifelse(StateHoliday == 'c', 1, 0))
summary(lm(Sales ~ Customers + StateHolidaya + StateHolidayb + StateHolidayc, data = dtrain))
summary(lm(Sales ~ StateHolidaya + StateHolidayb + StateHolidayc, data = dtrain))
summary(lm(Customers ~ StateHolidaya + StateHolidayb + StateHolidayc, data = dtrain))
dtrain$StateHoliday <- NULL
levels(dtrain$PromoInterval) <- c(0, 2, 1, 3)
dtrain <- dtrain %>% mutate(PromoIntervalJ = ifelse(PromoInterval == '1', 1, 0),
                            PromoIntervalF = ifelse(PromoInterval == '2', 1, 0),
                            PromoIntervalM = ifelse(PromoInterval == '3', 1, 0))
summary(lm(Sales ~ Customers + PromoIntervalJ + PromoIntervalF + PromoIntervalM, data = dtrain))
summary(lm(Sales ~ PromoIntervalJ + PromoIntervalF + PromoIntervalM, data = dtrain))
summary(lm(Customers ~ PromoIntervalJ + PromoIntervalF + PromoIntervalM, data = dtrain))
# dtrain$PromoInterval <- NULL
# table(dtrain$CompetitionOpenSinceYear)
# dtrain <- dtrain %>% mutate(CompOpenYear1961 = ifelse(CompetitionOpenSinceYear == 1961, 1, 0),
#                             CompOpenYear1990 = ifelse(CompetitionOpenSinceYear == 1990, 1, 0),
#                             CompOpenYear1994 = ifelse(CompetitionOpenSinceYear == 1994, 1, 0),
#                             CompOpenYear1995 = ifelse(CompetitionOpenSinceYear == 1995, 1, 0),
#                             CompOpenYear1998 = ifelse(CompetitionOpenSinceYear == 1998, 1, 0),
#                             CompOpenYear1999 = ifelse(CompetitionOpenSinceYear == 1999, 1, 0),
#                             CompOpenYear2000 = ifelse(CompetitionOpenSinceYear == 2000, 1, 0),
#                             CompOpenYear2001 = ifelse(CompetitionOpenSinceYear == 2001, 1, 0),
#                             CompOpenYear2002 = ifelse(CompetitionOpenSinceYear == 2002, 1, 0),
#                             CompOpenYear2003 = ifelse(CompetitionOpenSinceYear == 2003, 1, 0),
#                             CompOpenYear2004 = ifelse(CompetitionOpenSinceYear == 2004, 1, 0),
#                             CompOpenYear2005 = ifelse(CompetitionOpenSinceYear == 2005, 1, 0),
#                             CompOpenYear2006 = ifelse(CompetitionOpenSinceYear == 2006, 1, 0),
#                             CompOpenYear2007 = ifelse(CompetitionOpenSinceYear == 2007, 1, 0),
#                             CompOpenYear2008 = ifelse(CompetitionOpenSinceYear == 2008, 1, 0),
#                             CompOpenYear2009 = ifelse(CompetitionOpenSinceYear == 2009, 1, 0),
#                             CompOpenYear2010 = ifelse(CompetitionOpenSinceYear == 2010, 1, 0),
#                             CompOpenYear2011 = ifelse(CompetitionOpenSinceYear == 2011, 1, 0),
#                             CompOpenYear2012 = ifelse(CompetitionOpenSinceYear == 2012, 1, 0),
#                             CompOpenYear2013 = ifelse(CompetitionOpenSinceYear == 2013, 1, 0),
#                             CompOpenYear2014 = ifelse(CompetitionOpenSinceYear == 2014, 1, 0),
#                             CompOpenYear2015 = ifelse(CompetitionOpenSinceYear == 2015, 1, 0))
# summary(lm(Sales ~ Customers + PromoIntervalJ + PromoIntervalF + PromoIntervalM, data = dtrain))
# summary(lm(Sales ~ PromoIntervalJ + PromoIntervalF + PromoIntervalM, data = dtrain))
# summary(lm(Customers ~ PromoIntervalJ + PromoIntervalF + PromoIntervalM, data = dtrain))
# dtrain$CompetitionOpenSinceYear <- NULL
# table(dtrain$CompetitionOpenSinceMonth)
# dtrain <- dtrain %>% mutate(CompOpenMonth2 = ifelse(CompetitionOpenSinceMonth == 2, 1, 0),
#                             CompOpenMonth3 = ifelse(CompetitionOpenSinceMonth == 3, 1, 0),
#                             CompOpenMonth4 = ifelse(CompetitionOpenSinceMonth == 4, 1, 0),
#                             CompOpenMonth5 = ifelse(CompetitionOpenSinceMonth == 5, 1, 0),
#                             CompOpenMonth6 = ifelse(CompetitionOpenSinceMonth == 6, 1, 0),
#                             CompOpenMonth7 = ifelse(CompetitionOpenSinceMonth == 7, 1, 0),
#                             CompOpenMonth8 = ifelse(CompetitionOpenSinceMonth == 8, 1, 0),
#                             CompOpenMonth9 = ifelse(CompetitionOpenSinceMonth == 9, 1, 0),
#                             CompOpenMonth10 = ifelse(CompetitionOpenSinceMonth == 10, 1, 0),
#                             CompOpenMonth11 = ifelse(CompetitionOpenSinceMonth == 11, 1, 0),
#                             CompOpenMonth12 = ifelse(CompetitionOpenSinceMonth == 12, 1, 0))
# dtrain$CompetitionOpenSinceMonth <- NULL  
# summary(dtrain$CompetitionDistance)
# table(dtrain$CompetitionDistance)
# boxplot(dtrain$CompetitionDistance)
# which.max(table(dtrain$CompetitionDistance))
# dtrain <- dtrain %>% mutate(CompDistNA = ifelse(is.na(CompetitionDistance), 0, 
#                                                 cut(CompetitionDistance,
#                                                     breaks = seq(from = 0, to = 75900, by = 100),
#                                                     right = TRUE)))
# table(dtrain$CompDistNA)
# for(i in 1 : 759){
#   str <- paste0('CompDist', i)
#   dtrain <- dtrain %>% mutate(str[1] = ifelse(CompDistNA == i, 1, 0))
# }
# 
# sort(unique(dtrain$CompDistNA))
# dtrain <- dtrain %>% mutate(CompDist100 = ifelse(CompDistNA == 1,1,0),CompDist200 = ifelse(CompDistNA == 2,1,0),
#                             CompDist300 = ifelse(CompDistNA == 3,1,0),CompDist400 = ifelse(CompDistNA == 4,1,0),
#                             CompDist500 = ifelse(CompDistNA == 5,1,0),CompDist600 = ifelse(CompDistNA == 6,1,0),
#                             CompDist700 = ifelse(CompDistNA == 7,1,0),CompDist800 = ifelse(CompDistNA == 8,1,0),
#                             CompDist900 = ifelse(CompDistNA == 9,1,0),CompDist1000 = ifelse(CompDistNA == 10,1,0),
#                             CompDist1100 = ifelse(CompDistNA == 11,1,0),CompDist1200 = ifelse(CompDistNA == 12,1,0),
#                             CompDist1300 = ifelse(CompDistNA == 13,1,0),CompDist1400 = ifelse(CompDistNA == 14,1,0),
#                             CompDist1500 = ifelse(CompDistNA == 15,1,0),CompDist1600 = ifelse(CompDistNA == 16,1,0),
#                             CompDist1700 = ifelse(CompDistNA == 17,1,0),CompDist1800 = ifelse(CompDistNA == 18,1,0),
#                             CompDist1900 = ifelse(CompDistNA == 19,1,0),CompDist2000 = ifelse(CompDistNA == 20,1,0),
#                             CompDist2100 = ifelse(CompDistNA == 21,1,0),CompDist2200 = ifelse(CompDistNA == 22,1,0),
#                             CompDist2300 = ifelse(CompDistNA == 23,1,0),CompDist2400 = ifelse(CompDistNA == 24,1,0),
#                             CompDist2500 = ifelse(CompDistNA == 25,1,0),CompDist2600 = ifelse(CompDistNA == 26,1,0),
#                             CompDist2700 = ifelse(CompDistNA == 27,1,0),CompDist2800 = ifelse(CompDistNA == 28,1,0),
#                             CompDist2900 = ifelse(CompDistNA == 29,1,0),CompDist3000 = ifelse(CompDistNA == 30,1,0),
#                             CompDist3100 = ifelse(CompDistNA == 31,1,0),CompDist3200 = ifelse(CompDistNA == 32,1,0),
#                             CompDist3300 = ifelse(CompDistNA == 33,1,0),CompDist3400 = ifelse(CompDistNA == 34,1,0),
#                             CompDist3500 = ifelse(CompDistNA == 35,1,0),CompDist3600 = ifelse(CompDistNA == 36,1,0),
#                             CompDist3700 = ifelse(CompDistNA == 37,1,0),CompDist3800 = ifelse(CompDistNA == 38,1,0),
#                             CompDist3900 = ifelse(CompDistNA == 39,1,0),CompDist4000 = ifelse(CompDistNA == 40,1,0),
#                             CompDist4100 = ifelse(CompDistNA == 41,1,0),CompDist4200 = ifelse(CompDistNA == 42,1,0),
#                             CompDist4300 = ifelse(CompDistNA == 43,1,0),CompDist4400 = ifelse(CompDistNA == 44,1,0),
#                             CompDist4500 = ifelse(CompDistNA == 45,1,0),CompDist4600 = ifelse(CompDistNA == 46,1,0),
#                             CompDist4700 = ifelse(CompDistNA == 47,1,0),CompDist4800 = ifelse(CompDistNA == 48,1,0),
#                             CompDist4900 = ifelse(CompDistNA == 49,1,0),CompDist5000 = ifelse(CompDistNA == 50,1,0),
#                             CompDist5100 = ifelse(CompDistNA == 51,1,0),CompDist5200 = ifelse(CompDistNA == 52,1,0),
#                             CompDist5300 = ifelse(CompDistNA == 53,1,0),CompDist5400 = ifelse(CompDistNA == 54,1,0),
#                             CompDist5500 = ifelse(CompDistNA == 55,1,0),CompDist5600 = ifelse(CompDistNA == 56,1,0),
#                             CompDist5700 = ifelse(CompDistNA == 57,1,0),CompDist5800 = ifelse(CompDistNA == 58,1,0),
#                             CompDist5900 = ifelse(CompDistNA == 59,1,0),CompDist6000 = ifelse(CompDistNA == 60,1,0),
#                             CompDist6200 = ifelse(CompDistNA == 62,1,0),CompDist6300 = ifelse(CompDistNA == 63,1,0),
#                             CompDist6400 = ifelse(CompDistNA == 64,1,0),CompDist6500 = ifelse(CompDistNA == 65,1,0),
#                             CompDist6600 = ifelse(CompDistNA == 66,1,0),CompDist6700 = ifelse(CompDistNA == 67,1,0),
#                             CompDist6800 = ifelse(CompDistNA == 68,1,0),CompDist6900 = ifelse(CompDistNA == 69,1,0),
#                             CompDist7000 = ifelse(CompDistNA == 70,1,0),CompDist7200 = ifelse(CompDistNA == 72,1,0),
#                             CompDist7300 = ifelse(CompDistNA == 73,1,0),CompDist7400 = ifelse(CompDistNA == 74,1,0),
#                             CompDist7500 = ifelse(CompDistNA == 75,1,0),CompDist7600 = ifelse(CompDistNA == 76,1,0),
#                             CompDist7700 = ifelse(CompDistNA == 77,1,0),CompDist7800 = ifelse(CompDistNA == 78,1,0),
#                             CompDist7900 = ifelse(CompDistNA == 79,1,0),CompDist8000 = ifelse(CompDistNA == 80,1,0),
#                             CompDist8100 = ifelse(CompDistNA == 81,1,0),CompDist8200 = ifelse(CompDistNA == 82,1,0),
#                             CompDist8300 = ifelse(CompDistNA == 83,1,0),CompDist8400 = ifelse(CompDistNA == 84,1,0),
#                             CompDist8500 = ifelse(CompDistNA == 85,1,0),CompDist8600 = ifelse(CompDistNA == 86,1,0),
#                             CompDist8700 = ifelse(CompDistNA == 87,1,0),CompDist8800 = ifelse(CompDistNA == 88,1,0),
#                             CompDist8900 = ifelse(CompDistNA == 89,1,0),CompDist9000 = ifelse(CompDistNA == 90,1,0),
#                             CompDist9100 = ifelse(CompDistNA == 91,1,0),CompDist9200 = ifelse(CompDistNA == 92,1,0),
#                             CompDist9300 = ifelse(CompDistNA == 93,1,0),CompDist9400 = ifelse(CompDistNA == 94,1,0),
#                             CompDist9500 = ifelse(CompDistNA == 95,1,0),CompDist9600 = ifelse(CompDistNA == 96,1,0),
#                             CompDist9700 = ifelse(CompDistNA == 97,1,0),CompDist9800 = ifelse(CompDistNA == 98,1,0),
#                             CompDist9900 = ifelse(CompDistNA == 99,1,0),CompDist10000 = ifelse(CompDistNA==100,1,0),
#                             CompDist10100 = ifelse(CompDistNA==101,1,0),CompDist10200 = ifelse(CompDistNA==102,1,0),
#                             CompDist10400 = ifelse(CompDistNA==104,1,0),CompDist10300 = ifelse(CompDistNA==103,1,0),
#                             CompDist10600 = ifelse(CompDistNA==106,1,0),CompDist10500 = ifelse(CompDistNA==105,1,0),
#                             CompDist10800 = ifelse(CompDistNA==108,1,0),CompDist10900 = ifelse(CompDistNA==109,1,0),
#                             CompDist11000 = ifelse(CompDistNA==110,1,0),CompDist11200 = ifelse(CompDistNA==112,1,0),
#                             CompDist11300 = ifelse(CompDistNA==113,1,0),CompDist11400 = ifelse(CompDistNA==114,1,0),
#                             CompDist11500 = ifelse(CompDistNA==115,1,0),CompDist11600 = ifelse(CompDistNA==116,1,0),
#                             CompDist11700 = ifelse(CompDistNA==117,1,0),CompDist11900 = ifelse(CompDistNA==119,1,0),
#                             CompDist12100 = ifelse(CompDistNA==121,1,0),CompDist12500 = ifelse(CompDistNA==125,1,0),
#                             CompDist12600 = ifelse(CompDistNA==126,1,0),CompDist12700 = ifelse(CompDistNA==127,1,0),
#                             CompDist12800 = ifelse(CompDistNA==128,1,0),CompDist12900 = ifelse(CompDistNA==129,1,0),
#                             CompDist13000 = ifelse(CompDistNA==130,1,0),CompDist13100 = ifelse(CompDistNA==131,1,0),
#                             CompDist13200 = ifelse(CompDistNA==132,1,0),CompDist13600 = ifelse(CompDistNA==136,1,0),
#                             CompDist13700 = ifelse(CompDistNA==137,1,0),CompDist13800 = ifelse(CompDistNA==138,1,0),
#                             CompDist13900 = ifelse(CompDistNA==139,1,0),CompDist14000 = ifelse(CompDistNA==140,1,0),
#                             CompDist14100 = ifelse(CompDistNA==141,1,0),CompDist14200 = ifelse(CompDistNA==142,1,0),
#                             CompDist14300 = ifelse(CompDistNA==143,1,0),CompDist14600 = ifelse(CompDistNA==146,1,0),
#                             CompDist14700 = ifelse(CompDistNA==147,1,0),CompDist14900 = ifelse(CompDistNA==149,1,0),
#                             CompDist15000 = ifelse(CompDistNA==150,1,0),CompDist15100 = ifelse(CompDistNA==151,1,0),
#                             CompDist15200 = ifelse(CompDistNA==152,1,0),CompDist15300 = ifelse(CompDistNA==153,1,0),
#                             CompDist15400 = ifelse(CompDistNA==154,1,0),CompDist15500 = ifelse(CompDistNA==155,1,0),
#                             CompDist15700 = ifelse(CompDistNA==157,1,0),CompDist15800 = ifelse(CompDistNA==158,1,0),
#                             CompDist16200 = ifelse(CompDistNA==162,1,0),CompDist16300 = ifelse(CompDistNA==163,1,0),
#                             CompDist16400 = ifelse(CompDistNA==164,1,0),CompDist16500 = ifelse(CompDistNA==165,1,0),
#                             CompDist16600 = ifelse(CompDistNA==166,1,0),CompDist16700 = ifelse(CompDistNA==167,1,0),
#                             CompDist16800 = ifelse(CompDistNA==168,1,0),CompDist17000 = ifelse(CompDistNA==170,1,0),
#                             CompDist17100 = ifelse(CompDistNA==171,1,0),CompDist17200 = ifelse(CompDistNA==172,1,0),
#                             CompDist17300 = ifelse(CompDistNA==173,1,0),CompDist17400 = ifelse(CompDistNA==174,1,0),
#                             CompDist17500 = ifelse(CompDistNA==175,1,0),CompDist17600 = ifelse(CompDistNA==176,1,0),
#                             CompDist17700 = ifelse(CompDistNA==177,1,0),CompDist18000 = ifelse(CompDistNA==180,1,0),
#                             CompDist18100 = ifelse(CompDistNA==181,1,0),CompDist18200 = ifelse(CompDistNA==182,1,0),
#                             CompDist18400 = ifelse(CompDistNA==184,1,0),CompDist18600 = ifelse(CompDistNA==186,1,0),
#                             CompDist18700 = ifelse(CompDistNA==187,1,0),CompDist18800 = ifelse(CompDistNA==188,1,0),
#                             CompDist19400 = ifelse(CompDistNA==194,1,0),CompDist19700 = ifelse(CompDistNA==197,1,0),
#                             CompDist19800 = ifelse(CompDistNA==198,1,0),CompDist19900 = ifelse(CompDistNA==199,1,0),
#                             CompDist20100 = ifelse(CompDistNA==201,1,0),CompDist20300 = ifelse(CompDistNA==203,1,0),
#                             CompDist20400 = ifelse(CompDistNA==204,1,0),CompDist20700 = ifelse(CompDistNA==207,1,0),
#                             CompDist21000 = ifelse(CompDistNA==210,1,0),CompDist21400 = ifelse(CompDistNA==214,1,0),
#                             CompDist21800 = ifelse(CompDistNA==218,1,0),CompDist21900 = ifelse(CompDistNA==219,1,0),
#                             CompDist22000 = ifelse(CompDistNA==220,1,0),CompDist22400 = ifelse(CompDistNA==224,1,0),
#                             CompDist22500 = ifelse(CompDistNA==225,1,0),CompDist22600 = ifelse(CompDistNA==226,1,0),
#                             CompDist23200 = ifelse(CompDistNA==232,1,0),CompDist23700 = ifelse(CompDistNA==237,1,0),
#                             CompDist24000 = ifelse(CompDistNA==240,1,0),CompDist24600 = ifelse(CompDistNA==246,1,0),
#                             CompDist24800 = ifelse(CompDistNA==248,1,0),CompDist25400 = ifelse(CompDistNA==254,1,0),
#                             CompDist25500 = ifelse(CompDistNA==255,1,0),CompDist26200 = ifelse(CompDistNA==262,1,0),
#                             CompDist26500 = ifelse(CompDistNA==265,1,0),CompDist27000 = ifelse(CompDistNA==270,1,0),
#                             CompDist27200 = ifelse(CompDistNA==272,1,0),CompDist27600 = ifelse(CompDistNA==276,1,0),
#                             CompDist27700 = ifelse(CompDistNA==277,1,0),CompDist29100 = ifelse(CompDistNA==291,1,0),
#                             CompDist29200 = ifelse(CompDistNA==292,1,0),CompDist30000 = ifelse(CompDistNA==300,1,0),
#                             CompDist30100 = ifelse(CompDistNA==301,1,0),CompDist30400 = ifelse(CompDistNA==304,1,0),
#                             CompDist31900 = ifelse(CompDistNA==319,1,0),CompDist32300 = ifelse(CompDistNA==323,1,0),
#                             CompDist32400 = ifelse(CompDistNA==324,1,0),CompDist33100 = ifelse(CompDistNA==331,1,0),
#                             CompDist34100 = ifelse(CompDistNA==341,1,0),CompDist35300 = ifelse(CompDistNA==353,1,0),
#                             CompDist36500 = ifelse(CompDistNA==365,1,0),CompDist38700 = ifelse(CompDistNA==387,1,0),
#                             CompDist38800 = ifelse(CompDistNA==388,1,0),CompDist40600 = ifelse(CompDistNA==406,1,0),
#                             CompDist40900 = ifelse(CompDistNA==409,1,0),CompDist44400 = ifelse(CompDistNA==444,1,0),
#                             CompDist45800 = ifelse(CompDistNA==458,1,0),CompDist46600 = ifelse(CompDistNA==466,1,0),
#                             CompDist48400 = ifelse(CompDistNA==484,1,0),CompDist58300 = ifelse(CompDistNA==583,1,0),
#                             CompDist75900 = ifelse(CompDistNA==759,1,0))
# dtrain$CompetitionDistance <- NULL
# dtrain$CompDistNA <- NULL
# table(dtrain$Promo2SinceYear)
# dtrain <- dtrain %>% mutate(Promo2SinceYear2010 = ifelse(Promo2SinceYear == 2010, 1, 0),
#                             Promo2SinceYear2011 = ifelse(Promo2SinceYear == 2011, 1, 0),
#                             Promo2SinceYear2012 = ifelse(Promo2SinceYear == 2012, 1, 0),
#                             Promo2SinceYear2013 = ifelse(Promo2SinceYear == 2013, 1, 0),
#                             Promo2SinceYear2014 = ifelse(Promo2SinceYear == 2014, 1, 0),
#                             Promo2SinceYear2015 = ifelse(Promo2SinceYear == 2015, 1, 0))
# dtrain$Promo2SinceYear <- NULL
# table(dtrain$Promo2SinceWeek)
# dtrain <- dtrain %>% mutate(Promo2SinceWeek5 = ifelse(Promo2SinceWeek == 5, 1, 0),
#                             Promo2SinceWeek6 = ifelse(Promo2SinceWeek == 6, 1, 0),
#                             Promo2SinceWeek9 = ifelse(Promo2SinceWeek == 9, 1, 0),
#                             Promo2SinceWeek10 = ifelse(Promo2SinceWeek == 10, 1, 0),
#                             Promo2SinceWeek13 = ifelse(Promo2SinceWeek == 13, 1, 0),
#                             Promo2SinceWeek14 = ifelse(Promo2SinceWeek == 14, 1, 0),
#                             Promo2SinceWeek18 = ifelse(Promo2SinceWeek == 18, 1, 0),
#                             Promo2SinceWeek22 = ifelse(Promo2SinceWeek == 22, 1, 0),
#                             Promo2SinceWeek23 = ifelse(Promo2SinceWeek == 23, 1, 0),
#                             Promo2SinceWeek26 = ifelse(Promo2SinceWeek == 26, 1, 0),
#                             Promo2SinceWeek27 = ifelse(Promo2SinceWeek == 27, 1, 0),
#                             Promo2SinceWeek28 = ifelse(Promo2SinceWeek == 28, 1, 0),
#                             Promo2SinceWeek31 = ifelse(Promo2SinceWeek == 31, 1, 0),
#                             Promo2SinceWeek35 = ifelse(Promo2SinceWeek == 35, 1, 0),
#                             Promo2SinceWeek36 = ifelse(Promo2SinceWeek == 36, 1, 0),
#                             Promo2SinceWeek37 = ifelse(Promo2SinceWeek == 37, 1, 0),
#                             Promo2SinceWeek39 = ifelse(Promo2SinceWeek == 39, 1, 0),
#                             Promo2SinceWeek40 = ifelse(Promo2SinceWeek == 40, 1, 0),
#                             Promo2SinceWeek44 = ifelse(Promo2SinceWeek == 44, 1, 0),
#                             Promo2SinceWeek45 = ifelse(Promo2SinceWeek == 45, 1, 0),
#                             Promo2SinceWeek48 = ifelse(Promo2SinceWeek == 48, 1, 0),
#                             Promo2SinceWeek49 = ifelse(Promo2SinceWeek == 49, 1, 0),
#                             Promo2SinceWeek50 = ifelse(Promo2SinceWeek == 50, 1, 0))
# dtrain$Promo2SinceWeek <-  NULL
# head(dtrain)
# table(dtrain$DayOfWeek)
# dtrain <- dtrain %>% mutate(DayOfWeek2 = ifelse(DayOfWeek == 2, 1, 0),
#                             DayOfWeek3 = ifelse(DayOfWeek == 3, 1, 0),
#                             DayOfWeek4 = ifelse(DayOfWeek == 4, 1, 0),
#                             DayOfWeek5 = ifelse(DayOfWeek == 5, 1, 0),
#                             DayOfWeek6 = ifelse(DayOfWeek == 6, 1, 0),
#                             DayOfWeek7 = ifelse(DayOfWeek == 7, 1, 0))
# dtrain$DayOfWeek <- NULL
head(dtrain)
dtrain <- dtrain %>% mutate(Year = year(Date),
                            Month = month(Date),
                            Day = day(Date))
dtrain$Date <- NULL
dtrain$CompetitionDistance <- ifelse(is.na(dtrain$CompetitionDistance), 0, 
                                     dtrain$CompetitionDistance)
dtrain$CompetitionOpenSinceMonth <- ifelse(is.na(dtrain$CompetitionOpenSinceMonth), 0, 
                                           dtrain$CompetitionOpenSinceMonth)
dtrain$CompetitionOpenSinceYear <- ifelse(is.na(dtrain$CompetitionOpenSinceYear), 0, 
                                          dtrain$CompetitionOpenSinceYear)
dtrain$Promo2SinceWeek <- ifelse(is.na(dtrain$Promo2SinceWeek), 0,
                                 dtrain$Promo2SinceWeek)
dtrain$Promo2SinceYear <- ifelse(is.na(dtrain$Promo2SinceYear), 0,
                                 dtrain$Promo2SinceYear)
table(is.na(dtrain$PromoIntervalM))
colnames(dtrain)
first_lm <- lm(Customers ~ ., data = dtrain)
summary(first_lm)

# plot(dtrain$Sales ~ dtrain$Y)
# head(dtrain)
# table(dtrain$Y)
# dtrain <- dtrain %>% mutate(Y2014 = ifelse(Y == 2014, 1, 0),
#                             Y2015 = ifelse(Y == 2015, 1, 0))
# dtrain$Y <- NULL
# table(dtrain$M)
# dtrain <- dtrain %>% mutate(M2 = ifelse(M == 2, 1, 0),
#                             M3 = ifelse(M == 3, 1, 0),
#                             M4 = ifelse(M == 4, 1, 0),
#                             M5 = ifelse(M == 5, 1, 0),
#                             M6 = ifelse(M == 6, 1, 0),
#                             M7 = ifelse(M == 7, 1, 0),
#                             M8 = ifelse(M == 8, 1, 0),
#                             M9 = ifelse(M == 9, 1, 0),
#                             M10 = ifelse(M == 10, 1, 0),
#                             M11 = ifelse(M == 11, 1, 0),
#                             M12 = ifelse(M == 12, 1, 0))
# dtrain$M <- NULL
# table(dtrain$D)
# dtrain <- dtrain %>% mutate(D2 = ifelse(D == 2, 1, 0),
#                             D3 = ifelse(D == 3, 1, 0),
#                             D4 = ifelse(D == 4, 1, 0),
#                             D5 = ifelse(D == 5, 1, 0),
#                             D6 = ifelse(D == 6, 1, 0),
#                             D7 = ifelse(D == 7, 1, 0),
#                             D8 = ifelse(D == 8, 1, 0),
#                             D9 = ifelse(D == 9, 1, 0),
#                             D10 = ifelse(D == 10, 1, 0),
#                             D11 = ifelse(D == 11, 1, 0),
#                             D12 = ifelse(D == 12, 1, 0),
#                             D13 = ifelse(D == 13, 1, 0),
#                             D14 = ifelse(D == 14, 1, 0),
#                             D15 = ifelse(D == 15, 1, 0),
#                             D16 = ifelse(D == 16, 1, 0),
#                             D17 = ifelse(D == 17, 1, 0),
#                             D18 = ifelse(D == 18, 1, 0),
#                             D19 = ifelse(D == 19, 1, 0),
#                             D20 = ifelse(D == 20, 1, 0),
#                             D21 = ifelse(D == 21, 1, 0),
#                             D22 = ifelse(D == 22, 1, 0),
#                             D23 = ifelse(D == 23, 1, 0),
#                             D24 = ifelse(D == 24, 1, 0),
#                             D25 = ifelse(D == 25, 1, 0),
#                             D26 = ifelse(D == 26, 1, 0),
#                             D27 = ifelse(D == 27, 1, 0),
#                             D28 = ifelse(D == 28, 1, 0),
#                             D29 = ifelse(D == 29, 1, 0),
#                             D30 = ifelse(D == 30, 1, 0),
#                             D31 = ifelse(D == 31, 1, 0))
# dtrain$D <- NULL
head(dtrain)
colnames(dtrain)
ncol(dtrain)
ggplot(dtrain, aes(x = month(Date), y = Sales, col = year(Date))) + geom_point()
library(ggplot2)


timeseries <- ttrain %>% 
  select(Date, Sales, Customers) %>% 
  group_by(Date) %>% 
  summarise(Day_Sales = sum(Sales),
            Day_Customers = sum(Customers))
dates <- anydate(timeseries$Date)
plot(dates, timeseries$Day_Sales, cex = 1)
plot(dates, timeseries$Day_Customers, cex = 1)


ts_model <- aov(Day_Sales ~ Date, data = timeseries)
TukeyHSD(ts_model)

ts(1 : 10, frequency = 4, start = c(1959, 2))
ts(timeseries$Day_Sales, frequency = 31, start = c(1, 11))

head(timeseries$Day_Sales)

tdates <- ts(timeseries$Date)
plot(tdates)
str(tdates)
ap <- AirPassengers
str(ap)
plot(ap)

# Rossmann operates over 3,000 drug stores in 7 European countries. 

# Currently, Rossmann store managers are tasked with predicting 
# their daily sales for up to six weeks in advance. 

# Store sales are influenced by many factors, 
# including promotions, competition, school and state holidays, seasonality, and locality. 

# With thousands of individual managers predicting sales based on their unique circumstances, 
# the accuracy of results can be quite varied.

# In their first Kaggle competition, 
# Rossmann is challenging you to predict 6 weeks of daily sales for 1,115 stores located across Germany. 

# Reliable sales forecasts enable store managers to create effective staff schedules 
# that increase productivity and motivation. 

# By helping Rossmann create a robust prediction model, 
# you will help store managers stay focused on what’s most important to them: their customers and their teams! 



# Id - an Id that represents a (Store, Date) duple within the test set
# Store - a unique Id for each store
# Sales - the turnover for any given day (this is what you are predicting)
# Customers - the number of customers on a given day
# Open - an indicator for whether the store was open: 0 = closed, 1 = open

# StateHoliday - indicates a state holiday. 
#                Normally all stores, with few exceptions, are closed on state holidays. 
#                Note that all schools are closed on public holidays and weekends. 
#                a = public holiday, b = Easter holiday, c = Christmas, 0 = None

# SchoolHoliday - indicates if the (Store, Date) was affected by the closure of public schools
# StoreType - differentiates between 4 different store models: a, b, c, d
# Assortment - describes an assortment level: a = basic, b = extra, c = extended
# CompetitionDistance - distance in meters to the nearest competitor store

# CompetitionOpenSince[Month/Year] - gives the approximate year 
#                                    and month of the time the nearest competitor was opened

# Promo - indicates whether a store is running a promo on that day
# Promo2 - Promo2 is a continuing and consecutive promotion 
#          for some stores: 0 = store is not participating, 
#                           1 = store is participating

# Promo2Since[Year/Week] - describes the year and calendar week 
#                          when the store started participating in Promo2

# PromoInterval - describes the consecutive intervals Promo2 is started, 
#                 naming the months the promotion is started anew. 
#                 E.g. "Feb,May,Aug,Nov" means each round starts 
#                 in February, May, August, November of any given year for that store
