library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')
library(corrplot)

getwd()
train <-read.csv('./house-prices-advanced-regression-techniques/train.csv', stringsAsFactors = F)
test  <-read.csv('./house-prices-advanced-regression-techniques/test.csv', stringsAsFactors = F)



#test 데이터프레임에 SalePrice 컬럼 만들어주기
test$SalePrice<-rep(NA,1459)

#나눠져 있는 train ,test파일을 하나로 합침
house<-bind_rows(train,test)

dim(test)
dim(train)
dim(house)

#합친 데이터의 integer값들의 summary 확인
summary(house[,sapply(house[,1:81], typeof) == "integer"])
house.describe().T

# hose 가격 부포
p1 = ggplot(house, aes(x = SalePrice)) +
    geom_histogram(aes(x = SalePrice, stat(density)),
                   bins = 100,
                   fill = "orange",
                   alpha = 0.7) +
    geom_density(color = "blue") +
    scale_x_continuous(breaks= seq(0, 800000, by=100000),
                       labels = scales::comma) +
    labs(x = "판매 가격", y = "비율", title = "집값 거래가 분포도")
p1

# 주거용 지상공간의 넓이와 가격
p2 = ggplot(house,aes(x = GrLivArea, y = SalePrice)) +
    geom_point(color = "orange", alpha = 0.75, size = 2) +
    geom_smooth(mapping=aes(x=GrLivArea, y=SalePrice), data=house) +
    scale_y_continuous(breaks= seq(0, 800000, by=200000), 
                       labels = scales::comma) +
    labs(x = "주거용 지상공간의 넓이", y = "판매 가격", 
         title = "Sale Price by Above Ground Living Area")
p2


##숫자값 있는 열만 따로뽑기 
house_p = house[c(2,4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,
                  51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78)]
cor1 = cor(house_p)

#결측치 제거
corr1 = cor(na.omit(house_p))

corrplot(corr1 , method = "square")


##p값 관계 큰열 만 따로 뽑기
house_r = house[c(2,4,5,18,27,35,39,44,45,48,50,51,52,
                  53,55,57,60,62,63,67,68,71,72)]
cor2 = cor(house_r)

corr2 = cor(na.omit(house_r))

corrplot(corr2, method ="square", type ="lower")
corrplot(corr2, order = "hclust", addrect = 2, col = c("white", "black") , bg="gold2")




#################################################################################

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
#test <- fread('test.csv', header=T)
#train <- fread('train.csv', header=T)
sample_submission <- read.csv('./house-prices-advanced-regression-techniques/sample_submission.csv', header=T)

whole <- bind_rows(test, train)
whole

cor(test)

# train_model <- lm(SalePrice ~ SaleCondition, data=train)
# train_model
# summary(train_model)

numeric_train1 <- train[ , c(5,18,27,35,81)]
numeric_train1

numeric_train2 <- train[ , c(39,44,45,48,50,51,81)]
numeric_train2

numeric_train3 <- train[ , c(52,53,55,57,60,62,63,67,68,81)]
numeric_train3

numeric_train4 <- train[ , c(71,72,81)]
numeric_train4


numeric_train_model1 <- lm(SalePrice~., data=numeric_train1)
summary(numeric_train_model1)

numeric_train_model2 <- lm(SalePrice~., data=numeric_train2)
summary(numeric_train_model2)

numeric_train_model3 <- lm(SalePrice~., data=numeric_train3)
summary(numeric_train_model3)

numeric_train_model4 <- lm(SalePrice~., data=numeric_train4)
summary(numeric_train_model4)




windows(width = 10.0, height=6.5)
par(mfrow=c(2,3))

plot(SalePrice~., data=numeric_train1, col='tomato')
plot(SalePrice~., data=numeric_train2, col='tomato')
plot(SalePrice~., data=numeric_train3, col='tomato')
plot(SalePrice~., data=numeric_train4, col='tomato')





















