# NBA All Star Game Shiny App

library(shiny)
library(tidyverse)
library(gganimate)
library(shinythemes)
library(shinyWidgets)
library(jpeg)
library(grid)
library(ggimage)

# Start by loading the data.

load('data_complete.Rdata')

# Edit data. One complete dataset for statistical comparison and another for
# comparing shots.

data <- data_complete

data_shots <- data_complete %>% 
  filter(yearSeason > 1997) %>% 
  filter(!is.na(typeShot))

# Load image for shot charts.

img <- readJPEG('NBA_Court.jpg')

# Set names for y-axis labels to be displayed reactively.

stat_options <- c('Percent of Total Fan Vote' = 'percent_vote', 
                  'Fan Votes' = 'votes', 'Minutes' = 'minutes', 
                  'Game Outcome' = 'outcomeGame', 'Points' = 'pts', 
                  'Field Goals Made' = 'fgm', 'Field Goals Attempted' = 'fga', 
                  'Field Goal %' = 'pctFG', 'Free Throws Made' = 'ftm', 
                  'Free Throws Attempted' = 'fta', 'Free Throw %' = 'pctFT', 
                  '3PT Field Goals Made' = 'fg3m', 
                  '3PT Field Goals Attempted' = 'fg3a', 
                  '3PT Field Goal %' = 'pctFG3', 
                  '2PT Field Goals Made' = 'fg2m', 
                  '2PT Field Goals Attempted' = 'fg2a', 
                  '2PT Field Goal %' = 'pctFG2', 'Offensive Rebounds' = 'oreb', 
                  'Defensive Rebounds' = 'dreb', 'Total Rebounds' = 'treb', 
                  'Assists' = 'ast', 'Steals' = 'stl', 'Blocks' = 'blk', 
                  'Turnovers' = 'tov', 'Personal Fouls' = 'pf')

# Define UI for application

ui <- fluidPage(
  
  # Explore Different Themes. Chose to go with 'readable'.
  
  theme = shinythemes::shinytheme("readable"),
  
  # Background Image
  
  setBackgroundImage(src = "https://wallpapermemory.com/uploads/356/blue-red-background-hd-1920x1080-455739.jpg"),
  
  # Set Application Title, Subtitle
  
  h1(strong('NBA All Star', style = 'color: white'), align = 'center'), 
  h2('Exploring NBA All Star Game Data from 1951-2020', 
     style = 'color: white', align = 'center'),
  br(),
  
  # Set up navigation bar with tab information.
  
  navbarPage("",
    
    # Exploration Page
    
    navbarMenu('Explore', 
      tabPanel("Player Statistics",
             h2("NBA All Star Game Player Comparisons",
                style = 'color: white', align = 'center'),
             br(),
             fluidRow(
               column(5,
                      wellPanel(
                        p(strong('Instructions')),
                        p('Select the players for whom you wish to see a specificied statistic.'),
                        p(strong('Data Disclaimers')),
                        p('1952 onwards - Minutes Available', style = "font-size: 10pt"),
                        p('1974 onwards - Offensive & Defensive Rebounds, Steals, Blocks Available', style = "font-size: 10pt"),
                        p('1975 onwards - Fan Votes Available', style = "font-size: 10pt"),
                        p('1978 onwards - Turnovers Available', style = "font-size: 10pt"),
                        p('1980 onwards - 2PT & 3PT Field Goals Differentiated Between', style = "font-size: 10pt"),
                        p('1999 - No All Star Game due to Lockout Shortened Season', style = "font-size: 10pt"),
                      )),
               column(4, 
                      wellPanel(selectInput('namePlayer1', 'Player(s)', 
                                      data$namePlayer, 
                                      selected = 'Kobe Bryant',
                                      multiple = TRUE))),
               column(3,
                      wellPanel(selectInput('stat1', 'Statistic', 
                                      setNames(
                                        names(data %>% 
                                                select(
                                                  percent_vote, votes, minutes, 
                                                  outcomeGame, pts, fgm, fga, 
                                                  pctFG, ftm, fta, pctFT, fg3m, 
                                                  fg3a, pctFG3, fg2m, fg2a, 
                                                  pctFG2, oreb, dreb, treb, ast, 
                                                  stl, blk, tov, pf)), 
                                        c('Percent of Total Fan Vote', 
                                          'Fan Votes', 'Minutes', 
                                          'Game Outcome', 'Points', 
                                          'Field Goals Made', 
                                          'Field Goals Attempted', 
                                          'Field Goal %', 'Free Throws Made', 
                                          'Free Throws Attempted', 
                                          'Free Throw %', 
                                          '3PT Field Goals Made', 
                                          '3PT Field Goals Attempted', 
                                          '3PT Field Goal %', 
                                          '2PT Field Goals Made', 
                                          '2PT Field Goals Attempted', 
                                          '2PT Field Goal %', 
                                          'Offensive Rebounds', 
                                          'Defensive Rebounds', 
                                          'Total Rebounds', 'Assists', 'Steals', 
                                          'Blocks', 'Turnovers', 
                                          'Personal Fouls')))))),
             fluidRow(plotOutput('p1')),
             br()
             ),
             
      tabPanel('Shot Charts',
               h2("NBA All Star Game Player Shot Charts",
                  style = 'color: white', align = 'center'),
               br(),
               fluidRow(
                 column(7,
                        wellPanel(
                          p(strong('Instructions')),
                          p('Select the players and games for which you would like to see shot charts.'),
                          p(strong('Data Disclaimers')),
                          p('Player shot tracking available from 1998 onwards.'),
                          p('The All Star Game did not take place in 1999 due to the season being shortened by a lockout.')),
                        wellPanel(selectInput('namePlayer2', 'Player(s)', 
                                              data_shots$namePlayer, 
                                              selected = 'Kobe Bryant',
                                              multiple = TRUE)),
                        wellPanel(selectInput('year2', 'Year', 
                                              data_shots$yearSeason,
                                              selected = c(1998, 2000, 2001, 
                                                           2002, 2003, 2004, 
                                                           2005, 2006, 2007, 
                                                           2008, 2009, 2010, 
                                                           2011, 2012, 2013, 
                                                           2014, 2015, 2016, 
                                                           2017, 2018, 2019, 2020),
                                              multiple = TRUE))),
                 column(5, plotOutput('p2', height = 501.495))),
               br())),
    
    # Model Page
    
    tabPanel("Player Popularity Model",
             h3("Considering the Effects of Player Popularity on the All Star Game",
                        style = 'color: white', align = 'center'),
             br(),
             p("Since 1975, fan voting has played a role in determining which 
               players start in the All Star game. I wanted to see whether the 
               popularity of a player, in relation to the other players on his 
               team, might be correlated to the amount of playing time he 
               receives. In other words, do fans get to see the players they 
               most want to watch play more minutes in the All Star Game?",
               style = 'color: white'),
             br(),
             plotOutput('regression_mins'),
             br(),
             p('From the regression results above, it appears as if in most 
               years, the answer is yes! Being more popular is generally 
               positively correlated with minutes received. Going on from here, 
               does player popularity impact the number of points a player 
               scores in the game?',
               style = 'color: white'),
             br(),
             plotOutput('regression_pts'),
             br(),
             p('Again, the answer seems to be yes! As a player increases in 
               popularity ranking on his team, it seems as if, on average, he 
               tends to score more points in the game. This makes sense as the 
               more minutes one plays, the likelier they are to score more. 
               Finally, I wanted to ask one more related question. Does player 
               popularity affect the number of shots a player attempts in a 
               game? My hypothesis, of course, was that the two would be 
               positively correlated if more popular players had more chances 
               to put up shots.',
               style = 'color: white'),
             br(),
             plotOutput('regression_fga'),
             br(),
             p('As seen above, being more popular in the fan vote generally is 
               correlated with more shot attempts in a game. However, these 
               three effects are not always consistent across games which 
               allows for the posing of further questions regarding the purpose 
               and method of All Star games. Perhaps, if the league desires to 
               make the game align more closely with the desires of its fans, 
               minutes and shot attempts may need to be granted to players 
               according to the measure of how much fans want to see them play.',
               style = 'color: white'),
             br()
             ),
    
    # Project Background and Data Sources Page
    
    tabPanel("Background",
             h3("Project Background and Motivations",
                style = 'color: white', align = 'center'),
             br(),
             p("As a basketball fan who has always enjoyed the NBA All Star game, 
               this past year's edition was particularly impactful due to the 
               greater significance imparted upon it by the passing of 
               Kobe Bryant and his daughter Gigi in a helicopter crash that 
               claimed nine lives just days before All Star weekend. In light of
               this, I chose to center this final project around the All Star 
               game, working with data merged together from the publicly 
               available nbastatr package as well as data I personally scraped 
               from basketballreference.com and espn.com.",
               style = 'color: white'),
             p('My code may be accessed via ', a(href = 'https://github.com/joshmathews99/NBA-All-Star-Games', 'this', .noWS = "outside"), ' GitHub repository.', .noWS = c("after-begin", "before-end"),
               style = 'color: white'), 
             ),
    
    # Contact Information Page
    
    tabPanel("Contact", 
             h3('About the Creator',
                style = 'color: white', align = 'center'),
             br(),
             p("Hello! My name is Josh Mathews and I'm currently a junior at 
               Harvard College (class of 2021) from Winnipeg, Canada primarily 
               studying Human Developmental & Regenerative Biology as well as 
               Comparative Religion. I'm always happy to connect and if 
               you'd like, you can reach me at ", a(href = 'mailto:joshmathews@college.harvard.edu', 'joshmathews@college.harvard.edu', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"),
               style = 'color: white')),
    
    # In Memoriam / Dedication Page
    
    tabPanel('In Memoriam',
             h2('In Memory of Kobe and Gigi Bryant', style = 'color: white', align = 'center'),
             br(),
             fluidRow(
               column(6,
                      img(src = 'https://i.iheart.com/v3/re/new_assets/5e388c974c5fb9888956eb37?ops=gravity(%22north%22)%2Ccover(1440%2C808)', 
                          height = 378.75, width = 675)),
               column(6, img(src = 'https://a.espncdn.com/combiner/i?img=%2Fphoto%2F2020%2F0126%2Fr657485_1296x729_16%2D9.jpg', 
                             height = 378.75, width = 673.125)))
    )))


# Define server logic

server <- function(input, output, session) {
    output$p1 <- renderPlot({
      values1 <- reactive(data %>% filter(namePlayer %in% input$namePlayer1) %>% 
                            select(yearSeason, namePlayer, input$stat1) %>% distinct(.keep_all = TRUE) %>% pull())
      data1 <- reactive(data %>% filter(namePlayer %in% input$namePlayer1) %>% 
        select(yearSeason, namePlayer, input$stat1) %>% distinct())
      ggplot(data1(), aes(as.factor(yearSeason), values1(), colour = namePlayer)) + 
        geom_col(fill = 'white', position = position_dodge(preserve = 'single')) + theme_minimal() + 
        labs(x = 'Season', y = names(stat_options[which(stat_options == input$stat1)]), colour = 'Player')
    })
    output$p2 <- renderPlot({
      data_shots %>% 
        filter(namePlayer %in% input$namePlayer2) %>% 
        filter(yearSeason %in% input$year2) %>% 
        ggplot(aes(locationX, locationY, colour = namePlayer, group = yearSeason, shape = isShotMade)) + 
        annotation_custom(rasterGrob(img, 
                                     width = unit(.935, "npc"), 
                                     height = unit(.935, "npc")), 
                          -Inf, Inf, -Inf, Inf) + 
        geom_point(alpha = 1, size = 2, na.rm = TRUE) + 
        scale_y_continuous(limits = c(-52, 418)) + 
        scale_x_continuous(limits = c(-250, 250)) + 
        theme_classic() + 
        scale_shape_manual(values=c(1, 16), labels = c('Missed', 'Made')) + 
        theme(axis.text = element_blank(), axis.ticks = element_blank(), 
              axis.title = element_blank(), axis.line = element_blank()) + 
        labs(colour = 'Player', shape = 'Shot Success')
    })
    output$regression_mins <- renderPlot({
      load('regressions_mins_plot.Rdata')
      plot_regressions_mins
    })
    output$regression_pts <- renderPlot({
      load('regressions_pts_plot.Rdata')
      plot_regressions_pts
    })
    output$regression_fga <- renderPlot({
      load('regressions_fga_plot.Rdata')
      plot_regressions_fga
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
