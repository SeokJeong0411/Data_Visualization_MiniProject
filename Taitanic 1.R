###과제 1번 타이타닉 분석

library(readr)         
library(descr)         
library(VIM)             
library(ggplot2)         
library(RColorBrewer)    
library(scales)          
library(dplyr)          
library(purrr)          
library(tidyr)          
library(randomForest)

#---------------------------------------------------
# multiplot 사용 함수 정의

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)
    
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
        
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}
#---------------------------------------------------
# getwd()
# setwd("C:/joonion/my-first-github-base/TEAM")

# CSV 불러오기

train <- readr::read_csv('train.csv')

test  <- readr::read_csv('test.csv')

full <- dplyr::bind_rows(train, test) # csv 결합

#---------------------------------------------------
# 변수 속성 변환
full <- full %>%
    dplyr::mutate(Survived = factor(Survived),
                  Pclass   = factor(Pclass, ordered = T),
                  Name     = factor(Name),
                  Sex      = factor(Sex),
                  Ticket   = factor(Ticket),
                  Cabin    = factor(Cabin),
                  Embarked = factor(Embarked))

#---------------------------------------------------
# data 확인

head(full, 10)

#---------------------------------------------------
# 승객 나이 확인 그래프

age.p1 <- full %>% 
    ggplot(aes(Age)) + 
    # 히스토그램 그리기, 설정
    geom_histogram(breaks = seq(0, 80, by = 1), # 간격 설정 
                   col    = "red",              # 막대 경계선 색깔 
                   fill   = "green",            # 막대 내부 색깔 
                   alpha  = .5) +               # 막대 투명도 = 50% 
    # Plot title
    ggtitle("All Titanic passengers age hitogram") +
    theme(plot.title = element_text(face = "bold",    # 글씨체 
                                    hjust = 0.5,      # Horizon(가로비율) = 0.5
                                    size = 15, color = "darkblue"))

age.p2 <- full %>% 
    # test data set의 Survived == NA 인 값들 제외 
    filter(!is.na(Survived)) %>% 
    ggplot(aes(Age, fill = Survived)) + 
    geom_density(alpha = .5) +
    ggtitle("Titanic passengers age density plot") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5,
                                    size = 15, color = "darkblue"))

# multiplot layout 형식 지정
multi.layout = matrix(c(1, 1, 2, 2), 2, 2, byrow = T)

# 위에서 생성한 2개의 그래프 한 화면에 출력 
multiplot(age.p1, age.p2, layout = multi.layout)


#---------------------------------------------------
# P Class별 탑승객 확인 그래프

full %>% 
    # dplyr::group_by(), summarize() 를 이용해서 Pclass 빈도수 구하기
    group_by(Pclass) %>% 
    summarize(N = n()) %>% 
    # Aesthetic setting 
    ggplot(aes(Pclass, N)) +
    geom_col() +
    # Pclass 빈도수 plot에 출력 
    geom_text(aes(label = N),        # Plot의 y에 해당하는 N(빈도수)를 매핑
              size = 5,              # 글씨 크기 
              vjust = 1.2,           # vertical(가로) 위치 설정 
              color = "#FFFFFF") +   # 글씨 색깔 : 흰색
    # Plot title 
    ggtitle("Number of each Pclass's passengers") + 
    # Title setting 
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
    # x, y axis name change  
    labs(x = "Pclass", y = "Count")


#---------------------------------------------------
# 승객의 Fare 분포 확인 그래프

# Histogram 
Fare.p1 <- full %>%
    ggplot(aes(Fare)) + 
    geom_histogram(col    = "yellow",
                   fill   = "blue", 
                   alpha  = .5) +
    ggtitle("Histogram of passengers Fare") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# Boxplot 
Fare.p2 <- full %>%
    filter(!is.na(Survived)) %>% 
    ggplot(aes(Survived, Fare)) + 
    # 관측치를 회색점으로 찍되, 중복되는 부분은 퍼지게 그려줍니다.
    geom_jitter(col = "gray") + 
    # 상자그림 : 투명도 50% 
    geom_boxplot(alpha = .5) + 
    ggtitle("Boxplot of passengers Fare") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# multiplot layout 형식 지정
multi.layout = matrix(c(1, 1, 2, 2), 2, 2)

# 위에서 생성한 2개의 그래프 한 화면에 출력 
multiplot(Fare.p1, Fare.p2, layout = multi.layout)


#---------------------------------------------------
# 승객의 성별 분포 확인 그래프

sex.p1 <- full %>% 
    dplyr::group_by(Sex) %>% 
    summarize(N = n()) %>% 
    ggplot(aes(Sex, N)) +
    geom_col() +
    geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
    ggtitle("Bar plot of Sex") +
    labs(x = "Sex", y = "Count")

sex.p2 <- full[1:891, ] %>% 
    ggplot(aes(Sex, fill = Survived)) +
    geom_bar(position = "fill") + 
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = percent) +
    ggtitle("Survival Rate by Sex") + 
    labs(x = "Sex", y = "Rate")

multi.layout = matrix(rep(c(1, 2), times = 2), 2, 2, byrow = T)

multiplot(sex.p1, sex.p2, layout = multi.layout)


#---------------------------------------------------
# 데이터 전처리(결측치, 변수를 결합해서 파생변수 생성)


# Age를 범주화한 Age Group 생성
full <- full %>%
    # 결측치(NA)를 먼저 채우는데 결측치를 제외한 값들의 평균으로 채움.
    mutate(Age = ifelse(is.na(Age), mean(full$Age, na.rm = TRUE), Age),
           # Age 값에 따라 범주형 파생 변수 Age.Group 를 생성
           Age.Group = case_when(Age < 13             ~ "Age.0012",
                                 Age >= 13 & Age < 18 ~ "Age.1317",
                                 Age >= 18 & Age < 60 ~ "Age.1859",
                                 Age >= 60            ~ "Age.60inf"),
           # Chr 속성을 Factor로 변환
           Age.Group = factor(Age.Group))


# FamilySize 생성
full <- full %>% 
    # SibSp, Parch와 1(본인)을 더해서 FamilySize라는 파생변수를 먼저 생성  
    mutate(FamilySize = .$SibSp + .$Parch + 1,
           # FamilySize 의 값에 따라서 범주형 파생 변수 FamilySized 를 생성 
           FamilySized = dplyr::case_when(FamilySize == 1 ~ "Single",
                                          FamilySize >= 2 & FamilySize < 5 ~ "Small",
                                          FamilySize >= 5 ~ "Big"),
           # Chr 속성인 FamilySized를 factor로 변환하고
           # 집단 규모 크기에 따라 levels를 새로 지정
           FamilySized = factor(FamilySized, levels = c("Single", "Small", "Big")))


# 성별과 관련된 이름을 범주화
# 우선 Name 열벡터만 추출해서 title 벡터에 저장 
title <- full$Name

# 정규표현식과 gsub()을 이용해서 성별과 관련성이 높은 이름만 추출해서 title 벡터로 저장 
title <- gsub("^.*, (.*?)\\..*$", "\\1", title)

# 위에서 저장한 title 벡터를 다시 full 에 저장하되, title 파생변수로 저장 
full$title <- title

# 고유한 title 확인
unique(full$title)

# 18개의 범주를 5개 범주로 단순화 시키는 작업 
full <- full %>%

    mutate(title = ifelse(title %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", title),
           title = ifelse(title == "Mme", "Mrs", title),
           title = ifelse(title %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don",
                                       "Sir", "the Countess", "Jonkheer"), "Officer", title),
           title = factor(title))

# 파생변수 생성 후 각 범주별 빈도수, 비율 확인 
descr::CrossTable(full$title)


# 티켓값 치환

# 티켓값에 중복이 있음
# 우선 ticket.unique가 모두 0이라고 저장함
ticket.unique <- rep(0, nrow(full))

# Ticket Feature에서 고유한 것들만 추출해서 tickets 벡터에 저장 
tickets <- unique(full$Ticket)

# 반복문을 중첩 활용해서 티켓이 같은 승객들만 추출 후, 각 티켓들의 길이(문자 갯수)를 추출해서 저장한다.
for (i in 1:length(tickets)) {
    current.ticket <- tickets[i]
    party.indexes <- which(full$Ticket == current.ticket)
    # For loop 중첩 
    for (k in 1:length(party.indexes)) {
        ticket.unique[party.indexes[k]] <- length(party.indexes)
    }
}

# 위에서 계산한 ticket.unique 을 파생변수로 저장 
full$ticket.unique <- ticket.unique

# ticket.unique에 따라 세가지 범주로 나눠서 ticket.size 변수 생성 
full <- full %>% 
    mutate(ticket.size = case_when(ticket.unique == 1 ~ 'Single',
                                   ticket.unique < 5 & ticket.unique >= 2 ~ "Small",
                                   ticket.unique >= 5 ~ "Big"),
           ticket.size = factor(ticket.size,
                                levels = c("Single", "Small", "Big")))


# Embarked 치환 - S가 가장 많았음
full$Embarked <- replace(full$Embarked,               # 치환할 Data$feature 지정
                         which(is.na(full$Embarked)), # 결측치들만 찾기
                         'S')                         # 치환할 값 지정 


# Fare 치환 - 결측치 1개만 있음
full$Fare <- replace(full$Fare, which(is.na(full$Fare)), 0)


#---------------------------------------------------
# 데이터 상관관계 확인


# 전처리 끝난 데이터 다시 분할
train <- full[1:891, ]

test <- full[892:1309, ]


# Pclass 상관관계 그래프
train %>% 
    ggplot(aes(Pclass, fill = Survived)) + 
    geom_bar(position = "fill") +
    # plot 테마 설정 : 조금 더 선명한 색깔로 변환해준다.
    scale_fill_brewer(palette = "Set1") +
    # Y axis setting 
    scale_y_continuous(labels = percent) +
    # x, y 축 이름과 plot의 main title, sub title 설정 
    labs(x = "Pclass", y = "Rate",
         title = "Bar plot", subtitle = "How many people survived in each Pclass?")


# Embarked 상관관계 그래프
train %>% 
    ggplot(aes(Embarked, fill = Survived)) +
    geom_bar(position = "fill") + 
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = percent) +
    labs(x = "Embarked", y = "Rate",
         title = "Bar plot", subtitle = "How many people survived in each Embarked?")


# Familysize 상관관계 그래프
train %>% 
    ggplot(aes(FamilySized, fill = Survived)) + 
    geom_bar(position = "fill") + 
    scale_fill_brewer(palette = "Set1") + 
    scale_y_continuous(labels = percent) +
    labs(x = "FamilySized", y = "Rate",
         title = "Bar plot", subtitle = "Survival rate by FamilySized")


# Age Group 상관관계 그래프
train %>% 
    ggplot(aes(Age.Group, fill = Survived)) + 
    geom_bar(position = "fill") + 
    scale_fill_brewer(palette = "Set1") + 
    scale_y_continuous(labels = percent) +
    labs(x = "Age group", y = "Rate",
         title = "Bar plot", subtitle = "Survival rate by Age group")


# title 상관관계 그래프
train %>% 
    ggplot(aes(title, fill = Survived)) + 
    geom_bar(position = "fill") + 
    scale_fill_brewer(palette = "Set1") + 
    scale_y_continuous(labels = percent) +
    labs(x = "title", y = "Rate",
         title = "Bar plot", subtitle = "Survival rate by passengers title")

# ticket size 상관관계 그래프
train %>% 
    ggplot(aes(ticket.size, fill = Survived)) + 
    geom_bar(position = "fill") + 
    scale_fill_brewer(palette = "Set1") + 
    scale_y_continuous(labels = percent) +
    labs(x = "ticket.size", y = "Rate",
         title = "Bar plot", subtitle = "Survival rate by ticket.size")


#---------------------------------------------------
# 머신러닝에 사용할 변수 선택해서 저장

# Id number 제외하고 실제로 사용할 7개 입력변수와 1개의 타겟변수를 선택, 저장 
train <- train %>% 
    select("Pclass", "Sex", "Embarked", "FamilySized",
           "Age.Group", "title", "ticket.size", "Survived")

# Submit을 위해서 Id 열벡터 추출해서 ID에 저장 
ID <- test$PassengerId

# Id와 Survived를 제외한 나머지 6개 변수들을 선택, 저장 
test <- test %>% 
    select("Pclass", "Sex", "Embarked", "FamilySized",
           "Age.Group", "title", "ticket.size")


#---------------------------------------------------
# 로지스틱 회귀분석

surv_logit <- glm(Survived ~ ., data=train, family=binomial(link="logit"))
summary(surv_logit)

# title에 따른 생존여부

