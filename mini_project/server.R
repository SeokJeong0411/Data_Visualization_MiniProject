# 데이터 불러오기
df2015 <- fread('2015.csv', encoding = "UTF-8")
df2015_2019 <- fread("2015_2019.csv", encoding = 'UTF-8')
score <- fread("happiness_score.csv", encoding = 'UTF-8')
df <- fread("worldhappy.csv", encoding = 'UTF-8')

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
            theme(axis.text.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.text.y = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),  
                  axis.title.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.title.y = element_text(color = "grey20", size = 30, angle = 90, family = 'nanumpen'))
    })
    
    output$plots999 <- renderPlot({
        df2015[ , "score" ] <- df2015[,'Economy (GDP per Capita)']*input$weight_E +
            df2015[,'Family']*input$weight_F +
            df2015[,'Health (Life Expectancy)']*input$weight_H +
            df2015[,'Freedom']*input$weight_Fr +
            df2015[,'Trust (Government Corruption)']*input$weight_T +
            df2015[,'Generosity']*input$weight_G +
            df2015[,'Dystopia Residual']*input$weight_D
        ggplot(df2015, aes(x=Country, y=score), width = 1000, height = 5500) +
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
            theme(axis.title = element_text(size = (8)),
                  axis.text.x = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
                  axis.text.y = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),  
                  axis.title.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.title.y = element_text(color = "grey20", size = 30, angle = 90, family = 'nanumpen'),
                  legend.text = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
                  legend.title = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'))
    })
    output$plotkorea <- renderPlot({
        hp_score <- score %>% 
            filter(`Country name` %in% c("Finland", "South Korea"))
        
        hp_score %>% 
            ggplot( aes(x=year, y=score, group=`Country name`, color = `Country name`)) +
            geom_line(size = 1) +
            scale_color_manual(values = c("#f0e130", "#bf94e4"))+
            scale_x_binned(n.breaks=18) +
            labs(x = "Year", y = "Happiness Score",
                 title = "Korea .vs. Finland") +
            geom_hline(yintercept = 5.445809501, color = "#668b8b", size = 0.5) +
            theme(plot.title = element_text(size = 40, family = 'nanumpen'),
                  axis.text.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.text.y = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),  
                  axis.title.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.title.y = element_text(color = "grey20", size = 30, angle = 90, family = 'nanumpen'),
                  legend.text = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
                  legend.title = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'))
        
    })
    output$plotIncrease <- renderPlot({
        d06<-df %>% 
            filter(`year` %in% "2006")%>%select(`Country name`,HS06=`Life Ladder`)
        d19<-df %>% 
            filter(`year` %in% "2019")%>%select(`Country name`,HS19=`Life Ladder`)
        
        score<-inner_join(d06,d19)%>% mutate(score_diff= HS19-HS06)%>% filter(score_diff>0)
        score$`Country name` <- factor(score$`Country name`, levels=as.character(score$`Country name`))
        
        ggplot(score, aes(x=HS06, xend=HS19, y=`Country name`, group=`Country name`)) +
            geom_dumbbell(size=2, color="#e3e2e1",
                          colour_x = "#5b8124", colour_xend = "#bad744",
                          dot_guide=TRUE, dot_guide_size=0.25) +
            labs(x=NULL,
                 y=NULL,
                 title=" Country Happiness Scores Increased ") +
            theme(plot.background=element_rect(fill="#f7f7f7"),
                  panel.background=element_rect(fill="#f7f7f7"),
                  panel.grid.minor=element_blank(),
                  panel.grid.major.y=element_blank(),
                  panel.grid.major.x=element_line(),
                  axis.ticks=element_blank(),
                  legend.position="top",
                  panel.border=element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold", size = 40, family = 'nanumpen'),
                  axis.text.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.text.y = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'))  
    }, width=600 , height = 900)
    output$plotcon <- renderPlot({
        ggplot(df2015_2019[df2015_2019$Country==input$countries3, ],
               aes(x=year, y=get(input$things), label=get(input$things))) +
            geom_line(color='grey') +
            geom_point(shape=15, color="tomato", fill="tomato", size=5) +
            geom_text_repel(size=20, color='tomato', family = 'nanumpen') +
            theme(axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank(),
                  axis.text.x = element_text(color='tomato',size=20),
                  axis.title = element_text(size=20),
                  
            )+
            labs(x='YEAR', y=input$things) +
            ggtitle(paste(input$countries3, '.vs.', input$things)) +
            theme(plot.title = element_text(size = 40, family = 'nanumpen'),
                  axis.text.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.title.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
                  axis.title.y = element_text(color = "grey20", size = 30, angle = 90, family = 'nanumpen'),
                  legend.text = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
                  legend.title = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'))
    })
    output$plotAni2 <- renderImage({
        don <- df2015_2019 %>% 
            filter(Country %in% c("Switzerland", "Iceland", "Denmark", "Norway", "Canada",
                                  "Finland", "Netherlands", "Sweden", "New Zealand",
                                  "Australia", "Austria"))
               # p <- don %>%
               #     ggplot( aes(x=year, y=-`Happiness Rank`, group=Country, color=Country)) +
               #     geom_line() +
               #     geom_point() +
               #     ggtitle("상위 10개 국가 연도별 행복지수 순위") +
               #     ylab("Happiness Rank") +
               #     xlab("Year") +
               #     transition_reveal(year) +
               #     theme(plot.title = element_text(size = 40, family = 'nanumpen'),
               #           axis.text.x = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
               #           axis.text.y = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
               #           axis.title.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
               #           axis.title.y = element_text(color = "grey20", size = 30, angle = 90, family = 'nanumpen'),
               #           legend.text = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
               #           legend.title = element_text(size = 20, family = 'nanumpen'))
               # anim_save("outfile2.gif", animate(p))
        list(src = "outfile2.gif",
             contentType = 'image/gif')
        # width = 400,
        # height = 300
    }, deleteFile = F
    )
    output$plotAni <- renderImage({
               # p <- ggplot(df2015_2019, aes(GDP, `Happiness Score`, color = Continent, size = 9)) +
               #         geom_point() +
               #         theme_bw() +
               #         labs(title = 'Year: {frame_time}', x = 'GDP', y = 'Happiness score') +
               #         transition_time(year) +
               #         ease_aes('linear') +
               #         theme(plot.title = element_text(size = 30, family = 'nanumpen'),
               #             axis.text.x = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
               #             axis.text.y = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
               #             axis.title.x = element_text(color = "grey20", size = 30, angle = 0, family = 'nanumpen'),
               #             axis.title.y = element_text(color = "grey20", size = 30, angle = 90, family = 'nanumpen'),
               #             legend.text = element_text(color = "grey20", size = 20, angle = 0, family = 'nanumpen'),
               #             legend.title = element_text(size = 20, family = 'nanumpen'))
               # anim_save("outfile1.gif", animate(p))
        list(src = "outfile1.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
        )
    },deleteFile = F,
    )
}