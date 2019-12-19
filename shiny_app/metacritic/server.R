library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(forcats)

metacritic <- read.csv(file="./metacritic_reviews_final.csv")
developers <- metacritic %>% group_by(developer) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% filter(n>850) %>% mutate_if(is.factor,as.character) %>% .$developer
reviewers <- metacritic %>% group_by(reviewer) %>% summarise(n=n()) %>% 
  arrange(desc(n)) %>% filter(n>1400) %>% mutate_if(is.factor,as.character) %>% .$reviewer
# metacritic$console <- factor(metacritic$console,levels=c("PC","N64","Gamecube","Wii","Wii U",
#                                                          "Switch","Dreamcast","Playstation",
#                                                          "Playstation 2","Playstation 3",
#                                                          "Playstation 4","Xbox",
#                                                          "Xbox 360","Xbox One"))
# metacritic$genre <- factor(metacritic$genre,levels=c("Action","Action Adventure","Adventure",
#                                                      "Fighting","Platformer","Shooter",
#                                                      "Horror","Action RPG",
#                                                      "Western RPG","Japanese RPG","Strategy",
#                                                      "MMO","Sports","Racing","Simulation",
#                                                      "Puzzle","Miscellaneous"))

shinyServer(function(input, output) {
  # Plot for console timeseries
   output$ts_console <- renderPlot({
     metacritic_by_year <- metacritic %>% group_by(year) %>% summarise(n_year=n())
     metacritic_group <- metacritic %>% group_by(year,console) %>% summarise(n=n()) %>% 
       inner_join(metacritic_by_year,by="year") %>% mutate(pct = n/n_year) %>% select(-n,-n_year) %>% 
       spread(key=console,value=pct,fill=0) %>% gather(key=console,value=pct,-year)
     metacritic_group$console <- factor(metacritic_group$console,levels=c("PC","N64","Gamecube","Wii","Wii U",
                                                                          "Switch","Dreamcast","Playstation",
                                                                          "Playstation 2","Playstation 3",
                                                                          "Playstation 4","Xbox",
                                                                          "Xbox 360","Xbox One"))
     metacritic_group %>% 
       ggplot(aes(x=year,y=pct,group=console))+geom_area(aes(x=year,y=pct,fill=console))+
       labs(fill='console')+scale_x_continuous(name="Year",limits=c(1996,2019),breaks=seq(1996,2020,2))+
       scale_y_continuous(name="Fraction of Total Reviews",limits=c(0,1),breaks=seq(0,1,0.25))+
       theme(text = element_text(size=16))
   })
   
   # Plot for genre timeseries
   output$ts_genre <- renderPlot({
     metacritic_by_year <- metacritic %>% group_by(year) %>% summarise(n_year=n())
     metacritic_group <- metacritic %>% group_by(year,genre) %>% summarise(n=n()) %>% 
       inner_join(metacritic_by_year,by="year") %>% mutate(pct = n/n_year) %>% select(-n,-n_year) %>% 
       spread(key=genre,value=pct,fill=0) %>% gather(key=genre,value=pct,-year)
     metacritic_group$genre <- factor(metacritic_group$genre,levels=c("Action","Action Adventure","Adventure",
                                                                      "Fighting","Platformer","Shooter",
                                                                      "Horror","Action RPG",
                                                                      "Western RPG","Japanese RPG","Strategy",
                                                                      "MMO","Sports","Racing","Simulation",
                                                                      "Puzzle","Miscellaneous"))
     metacritic_group %>% 
       ggplot(aes(x=year,y=pct,group=genre))+geom_area(aes(x=year,y=pct,fill=genre))+
       labs(fill='genre')+scale_x_continuous(name="Year",limits=c(1996,2019),breaks=seq(1996,2020,2))+
       scale_y_continuous(name="Fraction of Total Reviews",limits=c(0,1),breaks=seq(0,1,0.25))+
       theme(text = element_text(size=16))
   })

   # Function to determine which variables to filter for rankings
   filter_rank <- function(df,vr,val) {
     if (val=="All") {
       return(df)
     } else {
       return(df %>% filter(UQ(as.symbol(vr))==val))
     }
   }
   
   # Rankings table
   rank_start = reactive({input$rank_year[1]})
   rank_end = reactive({input$rank_year[2]})
   rank_console = reactive({input$rank_console})
   rank_genre = reactive({input$rank_genre})
   rank_dev = reactive({input$rank_dev})
   output$rank_table <- renderDataTable({datatable(
     metacritic %>% filter_rank("console",rank_console()) %>% filter_rank("genre",rank_genre()) %>% 
       filter_rank("developer",rank_dev()) %>% filter(year>=rank_start() & year<=rank_end()) %>%
       group_by(console,developer,game,year,genre) %>% 
       summarise(AvgScore = round(mean(score,na.rm=TRUE),1)) %>% arrange(desc(AvgScore)) %>%
       select(Year=year,Console=console,Game=game,Developer=developer,Genre=genre,AvgScore)
   )})
   
   # Function for filtering boxplots
   filter_box <- function(df,vr,val,limit=NULL,lf=NULL) {
     if (is.null(val)) {
       if (is.null(limit)) {
         return(df)
       } else {
         return(df %>% filter(UQ(as.symbol(vr)) %in% lf[1:limit]))
       }
     } else {
       return(df %>% filter(UQ(as.symbol(vr)) %in% val))
     }
   }
   
   # Boxplots
   box_start = reactive({input$box_year[1]})
   box_end = reactive({input$box_year[2]})
   box_console = reactive({input$box_console})
   box_genre = reactive({input$box_genre})
   box_dev = reactive({input$box_dev})
   box_rev = reactive({input$box_rev})
   output$boxplot <- renderPlot({
      metacritic %>% filter_box("console",box_console()) %>%
         filter_box("genre",box_genre()) %>%
         filter_box("developer",box_dev()) %>%
         filter_box("reviewer",box_rev()) %>%
         filter(year>=box_start() & year<=box_end()) %>%
         ggplot(aes(x=console,y=score,fill=console,alpha=0.5))+
         geom_boxplot(aes(show.legend=FALSE))+
         xlab("Console")+ylab("Review Score")+
         theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1), text = element_text(size=16))}) 
   observeEvent(c(input$box_console,input$box_genre,input$box_dev,input$box_rev,input$box_x), {
     output$boxplot <- renderPlot({
       switch(input$box_x,
              con = metacritic %>% filter_box("console",box_console()) %>%
                filter_box("genre",box_genre()) %>%
                filter_box("developer",box_dev()) %>%
                filter_box("reviewer",box_rev()) %>%
                filter(year>=box_start() & year<=box_end()) %>%
                ggplot(aes(x=console,y=score,fill=console,alpha=0.5))+
                geom_boxplot(show.legend=FALSE)+
                xlab("Console")+ylab("Review Score")+
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1), text = element_text(size=16)),
              gen = metacritic %>% filter_box("console",box_console()) %>%
                filter_box("genre",box_genre()) %>%
                filter_box("developer",box_dev()) %>%
                filter_box("reviewer",box_rev()) %>%
                filter(year>=box_start() & year<=box_end()) %>%
                ggplot(aes(x=genre,y=score,fill=genre,alpha=0.5))+
                geom_boxplot(show.legend=FALSE)+
                xlab("genre")+ylab("Review Score")+
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1), text = element_text(size=16)),
              dev = metacritic %>% filter_box("console",box_console()) %>%
                filter_box("genre",box_genre()) %>%
                filter_box("developer",box_dev(),limit=10,lf=developers) %>%
                filter_box("reviewer",box_rev()) %>%
                filter(year>=box_start() & year<=box_end()) %>%
                ggplot(aes(x=developer,y=score,fill=developer,alpha=0.5))+
                geom_boxplot(show.legend=FALSE)+
                xlab("Developer")+ylab("Review Score")+
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1), text = element_text(size=16)),
              rev = metacritic %>% filter_box("console",box_console()) %>%
                filter_box("genre",box_genre()) %>%
                filter_box("developer",box_dev()) %>%
                filter_box("reviewer",box_rev(),limit=10,lf=reviewers) %>%
                filter(year>=box_start() & year<=box_end()) %>%
                ggplot(aes(x=reviewer,y=score,fill=reviewer,alpha=0.5))+
                geom_boxplot(show.legend=FALSE)+
                xlab("Reviewer")+ylab("Review Score")+
                theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1), text = element_text(size=16))
       )
     })
  })
   
   # Scatter plots
   scat_start = reactive({input$scat_year[1]})
   scat_end = reactive({input$scat_year[2]})
   scat_console = reactive({input$scat_console})
   scat_genre = reactive({input$scat_genre})
   scat_dev = reactive({input$scat_dev})
   scat_rev1 = reactive({input$scat_rev1})
   scat_rev2 = reactive({input$scat_rev2})
   output$scatter <- renderPlot({
      switch(input$scat_color,
             none=metacritic %>% filter_box("console",scat_console()) %>%
                filter_box("genre",scat_genre()) %>%
                filter_box("developer",scat_dev()) %>%
                filter_box("reviewer",c(scat_rev1(),scat_rev2())) %>%
                filter(year>=scat_start() & year<=scat_end()) %>%
                select(-X) %>% pivot_wider(names_from=reviewer,values_from=score,values_fn=list(score=mean)) %>%
                ggplot(aes(x=UQ(as.symbol(scat_rev1())),y=UQ(as.symbol(scat_rev2()))))+geom_point(size=2.5)+geom_abline()+
                theme(text = element_text(size=16)),
            con=metacritic %>% filter_box("console",scat_console()) %>%
               filter_box("genre",scat_genre()) %>%
               filter_box("developer",scat_dev()) %>%
               filter_box("reviewer",c(scat_rev1(),scat_rev2())) %>%
               filter(year>=scat_start() & year<=scat_end()) %>%
               select(-X) %>% pivot_wider(names_from=reviewer,values_from=score,values_fn=list(score=mean)) %>%
               ggplot(aes(x=UQ(as.symbol(scat_rev1())),y=UQ(as.symbol(scat_rev2()))))+
               geom_point(aes(color=console), size=2.5)+geom_abline()+
               theme(text = element_text(size=16)),
            gen=metacritic %>% filter_box("console",scat_console()) %>%
               filter_box("genre",scat_genre()) %>%
               filter_box("developer",scat_dev()) %>%
               filter_box("reviewer",c(scat_rev1(),scat_rev2())) %>%
               filter(year>=scat_start() & year<=scat_end()) %>%
               select(-X) %>% pivot_wider(names_from=reviewer,values_from=score,values_fn=list(score=mean)) %>%
               ggplot(aes(x=UQ(as.symbol(scat_rev1())),y=UQ(as.symbol(scat_rev2()))))+
               geom_point(aes(color=genre), size=2.5)+geom_abline()+
               theme(text = element_text(size=16))
            )
   })
})
