library(data.table)
library(tidyverse)
library(shiny)
library(hrbrthemes)
library(gganimate)
library(readxl)

ui <- fluidPage(
    # 1. 헤드 패널
    headerPanel(h1('4조 ')),
    tags$head(tags$style('h1 {color:pink;font-size:50px;}')),
    
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
                 "GIF 움직이는 사진",
                 tabPanel('GDP .vs. 행복도지수',
                          helpText('로딩이 오래 걸릴 수 있으니 기다려 주세요.'),
                          imageOutput('plotAni')),
                 tabPanel('상위10개국 연도별 행복도',
                          helpText('로딩이 오래 걸릴 수 있으니 기다려 주세요.'),
                          imageOutput('plotAni2'))
    )
)

# 데이터 불러오기
df2015 <- fread('2015.csv')
df2015_2019 = read.csv(file = "2015_2019.csv", header=T)
score <- read_excel("happiness_score.xlsx")

# 전처리
df2015_2019$Continent <- NA

df2015_2019$Continent[which(df2015_2019$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                       "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                       "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                       "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                       "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                       "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                       "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"

df2015_2019$Continent[which(df2015_2019$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                       "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                       "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                       "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                       "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                       "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                       "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                       "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                       "Bulgaria", "Albania", "Ukraine"))] <- "Europe"

df2015_2019$Continent[which(df2015_2019$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                       "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                       "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                       "Haiti"))] <- "North America"

df2015_2019$Continent[which(df2015_2019$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                       "Colombia", "Ecuador", "Bolivia", "Peru",
                                                       "Paraguay", "Venezuela"))] <- "South America"

df2015_2019$Continent[which(df2015_2019$Country %in% c("New Zealand", "Australia"))] <- "Australia"


df2015_2019$Continent[which(is.na(df2015_2019$Continent))] <- "Africa"

df2015$Continent <- NA

df2015$Continent[which(df2015$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                             "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                             "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                             "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                             "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                             "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                             "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"

df2015$Continent[which(df2015$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                             "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                             "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                             "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                             "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                             "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                             "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                             "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                             "Bulgaria", "Albania", "Ukraine"))] <- "Europe"

df2015$Continent[which(df2015$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                             "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                             "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                             "Haiti"))] <- "North America"

df2015$Continent[which(df2015$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                             "Colombia", "Ecuador", "Bolivia", "Peru",
                                             "Paraguay", "Venezuela"))] <- "South America"

df2015$Continent[which(df2015$Country %in% c("New Zealand", "Australia"))] <- "Australia"


df2015$Continent[which(is.na(df2015$Continent))] <- "Africa"





server <- function(input,output){
    output$plots <- renderPlot({
        ggplot(df2015, aes(x=get(input$sel1), y=get(input$sel2))) + 
            geom_point(size=4, color = 'coral1') + geom_smooth() +
            labs(x = input$sel1, y = input$sel2) +
            theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0),
                  axis.text.y = element_text(color = "grey20", size = 12, angle = 0),  
                  axis.title.x = element_text(color = "grey20", size = 20, angle = 0),
                  axis.title.y = element_text(color = "grey20", size = 20, angle = 90))
    })
    
    output$plots999 <- renderPlot({
        df2015[ , "score" ] <- df2015[,'Economy (GDP per Capita)']*input$weight_E +
            df2015[,'Family']*input$weight_F +
            df2015[,'Health (Life Expectancy)']*input$weight_H +
            df2015[,'Freedom']*input$weight_Fr +
            df2015[,'Trust (Government Corruption)']*input$weight_T +
            df2015[,'Generosity']*input$weight_G +
            df2015[,'Dystopia Residual']*input$weight_D
        ggplot(df2015, aes(x=Country, y=score), width = 3000, height = 3500) +
            geom_segment( aes(x=Country, xend=Country, y=0, yend=score), color="skyblue") +
            geom_point( color="blue", size=4, alpha=0.6) +
            theme_light() +
            coord_flip() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank()
            )
    })
    output$plotbox <- renderPlot({
        ggplot(df2015 , aes(x = Continent, y = get(input$sel3))) +
            geom_boxplot(aes(fill=Continent)) + theme_bw() + ylab(input$sel3) +
            theme_minimal() +
            theme(axis.title = element_text(size = (8)),
                  axis.text.x = element_text(color = "grey20", size = 12, angle = 0),
                  axis.text.y = element_text(color = "grey20", size = 12, angle = 0),  
                  axis.title.x = element_text(color = "grey20", size = 20, angle = 0),
                  axis.title.y = element_text(color = "grey20", size = 20, angle = 90),
                  legend.text = element_text(color = "grey20", size = 12, angle = 0))
    })
    output$plotkorea <- renderPlot({
        hp_score <- score %>% 
            filter(`Country name` %in% c("Finland", "South Korea"))
        
        hp_score %>% 
            ggplot( aes(x=year, y=score, group=`Country name`, color = `Country name`)) +
            geom_line(size = 1) +
            scale_color_manual(values = c("#f0e130", "#bf94e4"))+
            xlim(2005, 2020) +
            scale_x_binned(n.breaks=18) +
            labs(x = "Year", y = "Happiness Score",
                 title = "한국과 핀란드의 행복 점수 비교",
                 subtitle = "2006 ~ 2019",
                 caption = "World Happiness report") +
            theme_ipsum(grid = "Y") +
            geom_hline(linetype='dashed', yintercept = 5.445809501, color = "#668b8b", size = 0.5) +
            theme(plot.title = element_text(size = 20, hjust = 0.5),
                  plot.subtitle = element_text(size = 15, hjust = 0.5),
                  axis.text.x = element_text(color = "grey20", size = 12, angle = 0),
                  axis.text.y = element_text(color = "grey20", size = 12, angle = 0),  
                  axis.title.x = element_text(color = "grey20", size = 20, angle = 0, hjust = 0.5),
                  axis.title.y = element_text(color = "grey20", size = 20, angle = 90, hjust = 0.5))+
            geom_text(color = 'azure4', aes(2004, 5.445809501, label = "World Average", vjust = 1))
    })
    output$plotAni2 <- renderImage({
        don <- df2015_2019 %>% 
            filter(Country %in% c("Switzerland", "Iceland", "Denmark", "Norway", "Canada",
                                  "Finland", "Netherlands", "Sweden", "New Zealand",
                                  "Australia", "Austria"))
        p <- don %>%
            ggplot( aes(x=year, y=-Happiness.Rank, group=Country, color=Country)) +
            geom_line() +
            geom_point() +
            ggtitle("상위 10개 국가 연도별 행복지수 순위") +
            ylab("Happiness Rank") +
            xlab("Year") +
            transition_reveal(year) +
            theme_minimal() +
            theme(plot.title = element_text(size = 20, hjust = 0.5),
                  axis.text.x = element_text(color = "grey20", size = 12, angle = 0),
                  axis.text.y = element_text(color = "grey20", size = 12, angle = 0),  
                  axis.title.x = element_text(color = "grey20", size = 20, angle = 0),
                  axis.title.y = element_text(color = "grey20", size = 20, angle = 90),
                  legend.text = element_text(color = "grey20", size = 12, angle = 0))
        anim_save("outfile.gif", animate(p))
        list(src = "outfile.gif",
             contentType = 'image/gif')
        # width = 400,
        # height = 300
    }, deleteFile = T)
    output$plotAni <- renderImage({
        p <- ggplot(df2015_2019, aes(GDP, Happiness.Score, color = Continent, size = 9)) +
            geom_point() +
            theme_bw() +
            labs(title = 'Year: {frame_time}', x = 'GDP', y = 'Happiness Score') +
            transition_time(year) +
            ease_aes('linear') +
            theme_minimal() +
            theme(plot.title = element_text(size = 20, hjust = 0.5),
                  axis.text.x = element_text(color = "grey20", size = 12, angle = 0),
                  axis.text.y = element_text(color = "grey20", size = 12, angle = 0),  
                  axis.title.x = element_text(color = "grey20", size = 20, angle = 0),
                  axis.title.y = element_text(color = "grey20", size = 20, angle = 90),
                  legend.text = element_text(color = "grey20", size = 12, angle = 0))
        anim_save("outfile.gif", animate(p))
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
        )
    },deleteFile = T,)
}

shinyApp(ui, server)

