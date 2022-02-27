train <- read.csv("C:/yangosi/my-first-github-base/teamproject/project1/train.csv")
test <- read.csv("C:/yangosi/my-first-github-base/teamproject/project1/test.csv")
gender <- read.csv("C:/yangosi/my-first-github-base/teamproject/project1/gender_submission.csv")

str(train) #Survived column involved - 12개
str(test) #Survived column X - 11개
str(gender)

library(dplyr)
library(ggplot2)
library(tidyverse)
install.packages("plotly")
library(plotly)

install.packages("treemap")
library(treemap)



#============================== 전처리 ======================================

# 결측지 확인 

sum(is.na(train))

## 결측치가 포함된 열 찾기 - Age에서 177개
sum(is.na(train$Age))

## 결측치 제거
train <- na.omit(train)
train

## 결측치 0 확인
sum(is.na(train))

#---------------------------------------------------------------------------

# 데이터 유형 변경

## factor로 변경 -> 범주형으로 변경 (Pclass)
train$Pclass <- as.factor(train$Pclass)

##Character로 변경 : Name, Ticket, cabin
train$Name <- as.character(train$Name)
train$Ticket <-  as.character(train$Ticket)
train$Cabin <- as.character(train$Cabin)

str(train)


# 데이터 요약 보기

summary(train)


#============================== 전처리 ======================================

#연령대 확인, 최대값 확인
train$Age
max(train$Age)

dv_train <- train %>% 
  mutate(Ages = case_when(
    Age < 10 ~ "10세 미만",
    Age < 20 ~ "10세 ~ 20세",
    Age < 30 ~ "20세 ~ 30세",
    Age < 40 ~ "30세 ~ 40세",
    Age < 50 ~ "40세 ~ 50세",
    Age < 60 ~ "50세 ~ 60세",
    T ~ "60세 이상"
   
    
    
  ))
dv_train

dv_train$Ages <- factor(dv_train$Ages,
                        levels = c("10세 미만", "10세 ~ 20세","20세 ~ 30세",
                                   "30세 ~ 40세", "40세 ~ 50세","50세 ~ 60세", 
                                   "60세 이상"))
train_df <- dv_train %>%
  group_by(Ages) %>%
  summarise(count_ages = n())

train_df <- dv_train %>%
  group_by(Pclass) %>%
  summarise(count_Pclass = n())



#============================== 시각화 ======================================

# 탑승자 연령대 

ggplot(train_df, aes(x = Ages, y = count_ages, fill = Ages)) +
  geom_col() + 
    geom_text(aes(label = (count_ages)), vjust=3, hjust=0.5, 
            color = "black", size = 4) +
  labs(x="연령대", 
       y="연령대별 합계", 
       title="타이타닉호 탑승자 연령대") +
  theme_void()+
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        legend.position="right") 
  

# 좌석 등급별 생존 여부

treemap(train_df,
        index = "Pclass",
        vSize = "count_Pclass",
        type = "index",
        palette = "Set2",
        title = "좌석 등급별 생존자",
        fontsize.title = 25)

#성별에 따른 생존 여부

ggplot(dv_train, aes(x = Survived , fill = Sex))+
  geom_bar() + 
  theme_bw() +
  labs(x = "사망자와 생존자",
       title = " 성별에 따른 사망자 (왼) 와 생존자 수 (오) " ) +
  theme(plot.title = element_text(hjust=0.5))



# 동승자에 따른 생존 여부 

dv_train %>% 
  ggplot(aes( x = Survived, fill = factor(SibSp))) +
  geom_bar() +
  theme_bw() +
  labs(x = "사망자와 생존자",
       title = "같이 탑승한 배우자 또는 형제에 따른 생존여부" ) +
  theme(plot.title = element_text(hjust=0.5))



# 다중선형회귀

ttn <- dv_train[c("Survived", "SibSp", "Parch", "Fare")]#내가 필요한 것만 추출
summary(ttn)

cor(ttn)

plot(ttn)
lm(Survived~., data = ttn)
model <-lm(Survived~., data = dv_train) #~이후로는 독립변수 넣어주기
#종속변수 외에 다른 변수들을 독립변수로 넣어주려면 ~다음에 .이라고 적기
summary(model)
