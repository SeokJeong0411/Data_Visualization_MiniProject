library(data.table)
library(tidyverse)
library(shiny)
library(hrbrthemes)
library(gganimate)
library(readxl)
library(showtext)
library(flexdashboard)
library(ggrepel)
library(fresh)
library(ggalt)

font_add_google('Nanum Pen Script', 'nanumpen')
showtext_auto()

df2015_2019 <- fread('2015_2019.csv', header=T)

country3 <- as.list(df2015_2019$Country)
country3 <- unique(country3)

colname <- as.list(c('Happiness Rank','Happiness Score','GDP','Family',
                     'Life Expectancy','Freedom','Generosity'))

ui <- fluidPage(
    use_googlefont('Nanum Pen Script'),
    use_theme(create_theme(
        theme = "default",
        bs_vars_font(
            family_sans_serif = "'Nanum Pen Script', cursive",
            size_base = "20px"
        ),
        bs_vars_color(
            brand_primary = 'orange',
            gray_base = '#3B220C'
        )
    )),
    # 1. 헤드 패널
    headerPanel(h1('4조의 행복한 헤네시스')),
    tags$head(tags$style('h1 {color:#F2A830;font-size:50px;}')),

    # 2.
    navlistPanel(widths = c(3,9),
                 id = "tabset",
                 "행복 지수",
                 tabPanel('내가 정하는 행복 지수',
                          fluidRow(
                              column(3,
                                     sliderInput('weight_E',
                                                 'Economy: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01),
                                     sliderInput('weight_F',
                                                 'Family: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01),
                                     sliderInput('weight_H',
                                                 'Healthy: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01),
                                     sliderInput('weight_Fr',
                                                 'Freedom: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01),
                                     sliderInput('weight_T',
                                                 'Trust: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01),
                                     sliderInput('weight_G',
                                                 'Generosity: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01),
                                     sliderInput('weight_D',
                                                 'Dystopia Residual: ',
                                                 min = 1,
                                                 max = 5,
                                                 value = 1,
                                                 step = 0.01)),
                              column(9,
                                     plotOutput('plots999', height = 1000)),)
                 ),
                 tabPanel('대륙별 데이터',
                          selectInput('sel3',
                                      'y column: ',
                                      list('Happiness Score',
                                           'Economy (GDP per Capita)',
                                           'Family',
                                           'Health (Life Expectancy)',
                                           'Freedom',
                                           'Trust (Government Corruption)',
                                           'Generosity',
                                           'Dystopia Residual')),
                          plotOutput('plotbox')),
                 tabPanel('한국 .vs. 핀란드',
                          plotOutput('plotkorea')),
                 tabPanel('행복도의 변화',
                          plotOutput('plotIncrease')),
                 "여러 지수",
                 tabPanel('행복에 대한 다양한 변수의 상관관계',
                          fluidRow(
                              column(6,
                                     selectInput('sel1',
                                                 'x column: ',
                                                 list('Economy (GDP per Capita)',
                                                      'Family',
                                                      'Health (Life Expectancy)',
                                                      'Freedom',
                                                      'Trust (Government Corruption)',
                                                      'Generosity',
                                                      'Dystopia Residual'))),
                              column(6,
                                     selectInput('sel2',
                                                 'y column: ',
                                                 list('Economy (GDP per Capita)',
                                                      'Family',
                                                      'Health (Life Expectancy)',
                                                      'Freedom',
                                                      'Trust (Government Corruption)',
                                                      'Generosity',
                                                      'Dystopia Residual')))
                          ),
                          plotOutput('plots')),
                 tabPanel('국가별 행복지수 관련 지표 변화추이',
                          selectInput('countries3',
                                      '<국가>를 선택하시오',
                                      choices=country3),
                          selectInput('things',
                                      '<궁금한 지표>를 고르시오',
                                      choices=colname),
                          plotOutput('plotcon')),
                 "GIF 움직이는 사진",
                 tabPanel('GDP .vs. 행복도지수',
                          helpText('로딩이 오래 걸릴 수 있으니 기다려 주세요.'),
                          imageOutput('plotAni')),
                 tabPanel('상위10개국 연도별 행복도',
                          helpText('로딩이 오래 걸릴 수 있으니 기다려 주세요.'),
                          imageOutput('plotAni2'))
    )
)