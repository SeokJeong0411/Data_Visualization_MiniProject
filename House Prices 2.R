library(shiny)
library(ggplot2)
library(gapminder)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(ggrepel)
library(data.table)
library(psych)
library(GGally)
test <- fread('test.csv', header=T)
train <- fread('train.csv', header=T)
sample_submission <- fread('sample_submission.csv', header=T)

whole <- bind_rows(test, train)
whole

# train 파일에 있는 수치형 column들만 추출한 것
numeric_train_all <- train[ , c(2,4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,
                                51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
numeric_train_all

numeric_train_all_model <- lm(SalePrice~., data=numeric_train_all)
summary(numeric_train_all_model)



# p-value가 큰 변수를 제외한 것
numeric_train <- train[ , c(5,18,27,35,39,44,45,48,50,51,52,53,55,57,60,62,63,67,68,71,72,81)]
numeric_train

numeric_train_model <- lm(SalePrice~., data=numeric_train)
summary(numeric_train_model)

# p-value 작은 변수만을 그래프로 그려봄봄
plot(SalePrice~., data=numeric_train, col='tomato')
