library(tidyverse)
library(rio)
library(skimr)
library(tidyr)
library(readxl)
library(caret)
library(car)
library(glmnet)
library(tseries)
library(sandwich)
library(broom)
library(prettyR)
library(prettyunits)
library(dplyr)
library(glmnet)
library(quantreg)
library(lmtest)
library(knitr)
library(MASS) 

# Загружаем данные
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
destfile <- "forestfires.xls"
download.file(url, destfile)
df <- read.csv(destfile)
view(df)

# 1 задание ------------------------------------ 
# отфильтруем нулевые значения переменной area
df$area[df$area==0] <- NA
df2 <- df[complete.cases(df),]

# Рассмотрим значения area
qplot(data = df2, area, fill ="brick", binwidth = 1)
# Судя по графику, стоит попробовать прологарифмировать значения переменной area

qplot(data = df2, log(area), fill ="brick", binwidth = 1)

dmy <- dummyVars(" ~ .", data = df2)
tr <- data.frame(predict(dmy, newdata = df2))

# Избавимся от выбросов
tr <- mutate(tr, temp2 = temp^2, RH2 = RH^2, weather = temp*RH)
#-----------------------------------------------

# 2 задание ------------------------------------ 
# Попробуем разобраться, какие переменные нам нужны будут в модели

cor(df2$ISI, df2$area)
cor(df2$RH, df2$area)
cor(df2$wind, df2$area)
cor(df2$rain, df2$area)
cor(df2$temp, df2$area)
cor(df2$X, df2$area)
cor(df2$Y, df2$area)
cor(df2$FFMC, df2$area)
cor(df2$DMC, df$area)
cor(df2$DC, df2$area)
# Есть предположение, что количество пожаров в выходные дни больше, чем в будние
cor(tr)
# Заметим, что индексы зависят от температуры, поэтому за регрессор берем температуру. Также высокая корреляция между температурой и месяцем, поэтому месяц мы не будем использовать.
qplot(data = tr, temp, fill ="brick", binwidth = 1)
shapiro.test(tr$area)
shapiro.test(log(tr$temp))
shapiro.test(log(tr$temp))
shapiro.test(tr$temp)
shapiro.test(tr$RH)
ggplot(data = tr, tr$temp, fill = brick)
qplot(data = tr, temp, log(area))
qplot(data = tr, temp)

#-----------------------------------------------

# 3, 4 задание ------------------------------------ 
model <- lm(data = tr, log(area) ~ temp + temp2 + RH + RH2 + weather)
summary(model)
model2 <- lm(data = tr, log(area) ~ temp + RH)

vif(model)
vif(model2)

X <- model.matrix(data = tr, log(area) ~ temp + temp2 + RH + RH2 + weather)
XX <- t(X) %*% X
eigen <- eigen(XX)
eigen$values
CI1 <- sqrt(max(eigen$values) / min(eigen$values))
CI1

X <- model.matrix(data = tr, log(area) ~ temp + RH)
XX <- t(X) %*% X
eigen <- eigen(XX)
eigen$values
CI2 <- sqrt(max(eigen$values) / min(eigen$values))
CI2
# В первой модели все перемнные имеют показатель vif > 10, значит есть мультиколлинеарность. Во второй модели значения < 10 , поэтому мульиколлинеарности нет. Показатели CN в обоих случаях очень высокие, что свидетельсвтует о мультиколлинеарности.

summary(model2)
# Все переменные незначимы

# 5 задание ------------------------------------
jarque.bera.test(residuals(model2))
# Проведя гипотезу о нормальности, приходим к выводу, что не факт, что на 5% значимости она не отвергается

#Доверительный и предиктивный интервалы
predict(model2, newdata = tr, interval = "confidence")
predict(model2, newdata = tr, interval = "prediction")
------------------------------------------------

# 6 задание ------------------------------------
qplot(data = tr, temp, log(area))
qplot(data = tr, RH, log(area))
vcov(model2)
------------------------------------------------

# 7, 8 задание ------------------------------------
gqtest(model2, order.by = ~temp, data = tr, fraction = 0.2)
gqtest(model2, order.by = ~RH, data = tr, fraction = 0.2)
## H0 не отвергается в силу большого p-value = 0.9074, следовательно гетероскедастичность отсутствует в наших данных
------------------------------------------------
  
# 9 задание ------------------------------------
# робастные ошибки в форме Уайта
vcovHC(model2, type = "HC0")
# робастные ошибки ковариационной матрицы, устойчивые к гетероскедастичности
coeftest(model2, vcov. = vcovHC(model2))
# сравним в оценками МНК
coeftest(model2)
# видно, что для переменных влажности и температуры, где было подозрение на гетероскедастичность, критическое значение (p-value) снизилось
vcovHC(model2, type = "HC3")
coeftest(model2, vcov. = vcovHC(model2, type="HC3"))
------------------------------------------------
  
# 9 и 10 задания пропустил