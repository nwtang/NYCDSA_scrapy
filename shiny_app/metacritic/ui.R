library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)


years <- c(1996:2019)
consoles <- c("PC","N64","Gamecube","Wii","Wii U",
              "Switch","Dreamcast","Playstation",
              "Playstation 2","Playstation 3",
              "Playstation 4","Xbox",
              "Xbox 360","Xbox One")
genres <- c("Action","Action Adventure","Adventure",
            "Fighting","Platformer","Shooter",
            "Horror","Action RPG",
            "Western RPG","Japanese RPG","Strategy",
            "MMO","Sports","Racing","Simulation",
            "Puzzle","Miscellaneous")
metacritic <- read.csv(file="./metacritic_reviews_final.csv")
developers <- metacritic %>% group_by(developer) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% filter(n>850) %>% mutate_if(is.factor,as.character) %>% .$developer
reviewers <- metacritic %>% group_by(reviewer) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% filter(n>1400) %>% mutate_if(is.factor,as.character) %>% .$reviewer

shinyUI(dashboardPage(
  dashboardHeader(title = "Metacritic Games"),
  dashboardSidebar(sidebarMenu(
    menuItem("Consoles over the Years",tabName="tsconsole",icon=icon("gamepad")),
    menuItem("Genres over the Years",tabName="tsgenre",icon=icon("shapes")),
    menuItem("Video Games Ranked",tabName="rankings",icon=icon("list-ol")),
    menuItem("Boxplots of Review Scores",tabName="boxplots",icon=icon("box")),
    menuItem("Reviewer vs. Reviewer Plots",tabName="scatter",icon=icon("chart-bar"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tsconsole",
              box(title="Percentage of Reviews by Console",status="primary",solidHeader=TRUE,
                  plotOutput("ts_console",height=800),width=12)
              ),
      tabItem(tabName = "tsgenre",
              box(title="Percentage of Reviews by Genre",status="primary",solidHeader=TRUE,
                  plotOutput("ts_genre",height=800),width=12)
      ),
      tabItem(tabName = "rankings",
              box(title="Filters",status="info",solidHeader=TRUE,
                  sliderInput("rank_year",label=h5(strong("Year")),min=1996,max=2019,
                              value=c(1996,2019),sep=""),
                  selectizeInput(inputId="rank_console",
                                 label="Console",
                                 choices=c("All",consoles)),
                  selectizeInput(inputId="rank_genre",
                                 label="Genre",
                                 choices=c("All",genres)),
                  selectizeInput(inputId="rank_dev",
                                 label="Developer",
                                 choices=c("All",developers),
                                 multiple=TRUE,
                                 options=list(maxItems=1),
                                 selected="All"),
                  width=3),
              box(title="Rankings",status="primary",solidHeader=TRUE,dataTableOutput("rank_table"),
                  width=9)
              ),
      tabItem(tabName = "boxplots",
              box(title="Filters",status="info",solidHeader=TRUE,
                  radioButtons("box_x","X-Axis Variable:",
                               c("Console"="con",
                                 "Genre"="gen",
                                 "Developer"="dev",
                                 "Reviewer"="rev")),
                  sliderInput("box_year",label=h5(strong("Year")),min=1996,max=2019,
                              value=c(1996,2019),sep=""),
                  selectizeInput(inputId="box_console",
                                 label="Console",
                                 choices=c(consoles),
                                 multiple=TRUE,
                                 options=list(maxItems=length(consoles),placeholder="Leave blank to plot all consoles")
                                 ),
                  selectizeInput(inputId="box_genre",
                                 label="Genre",
                                 choices=c(genres),
                                 multiple=TRUE,
                                 options=list(maxItems=length(genres),placeholder="Leave blank to plot all genres")
                                 ),
                  selectizeInput(inputId="box_dev",
                                 label="Developer",
                                 choices=c(developers),
                                 multiple=TRUE,
                                 options=list(maxItems=10,placeholder="Leave blank to plot only top developers")
                                 ),
                  selectizeInput(inputId="box_rev",
                                 label="Reviewer",
                                 choices=c(reviewers),
                                 multiple=TRUE,
                                 options=list(maxItems=10,placeholder="Leave blank to plot only most frequent reviewers")
                                 ),
                  width=3),
              box(title="Boxplots",status="primary",solidHeader=TRUE,plotOutput("boxplot",height=800),
                  width=9)),
      tabItem(tabName = "scatter",
              box(title="Filters",status="info",solidHeader=TRUE,
                  selectizeInput(inputId="scat_rev1",
                                 label="Reviewer 1",
                                 choices=reviewers,
                                 multiple=TRUE,
                                 options=list(maxItems=1),
                                 selected="IGN"),
                  selectizeInput(inputId="scat_rev2",
                                 label="Reviewer 2",
                                 choices=reviewers,
                                 multiple=TRUE,
                                 options=list(maxItems=1),
                                 selected="GameSpot"),
                  sliderInput("scat_year",label=h5(strong("Year")),min=1996,max=2019,
                              value=c(1996,2019),sep=""),
                  selectizeInput(inputId="scat_console",
                                 label="Console",
                                 choices=c(consoles),
                                 multiple=TRUE,
                                 options=list(maxItems=length(consoles),placeholder="Leave blank to plot all consoles")
                  ),
                  selectizeInput(inputId="scat_genre",
                                 label="Genre",
                                 choices=c(genres),
                                 multiple=TRUE,
                                 options=list(maxItems=length(genres),placeholder="Leave blank to plot all genres")
                  ),
                  selectizeInput(inputId="scat_dev",
                                 label="Developer",
                                 choices=c(developers),
                                 multiple=TRUE,
                                 options=list(maxItems=length(developers),placeholder="Leave blank to plot all developers")
                  ),
                  radioButtons("scat_color","Color by:",
                               c("None"="none",
                                 "Console"="con",
                                 "Genre"="gen"
                                 )
                               ),
                  width=3),
              box(title="Reviewer vs. Reviewer Scatterplot",status="primary",solidHeader=TRUE,plotOutput("scatter",height=800),
                  width=9)
      )
    )
  )
))
