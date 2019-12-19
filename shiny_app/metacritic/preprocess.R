library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

metacritic <- read.csv(file="./metacritic_reviews.csv")

# Create column for year, parse genre into three columns
metacritic <- metacritic %>% mutate(year=lubridate::year(as.POSIXct(release_date,format="%m/%d/%Y"))) %>%
  separate(genre,c("genre1","genre2","genre3"),",") %>% mutate(genre2=trimws(genre2),genre3=trimws(genre3)) %>%
  filter(year>1995)

# Assign a more general genre based on the following rules:
# Anything in genres_lv1 gets assigned if it's found in the three columns
# After that, assignments go in the following order if found: Action RPG, RPG, Action Adventure, Action
# Only 8 NAs are left after this, all can be assigned to Action
genres_lv1 <- c("Platformer","Shooter","Fighting","Beat-'Em-Up","Horror","Rhythm","Simulation",
                "Strategy","Racing","Driving","Sports","Miscellaneous","Adventure",
                "Massively Multiplayer Online","Massively Multiplayer","MOBA","Western-Style",
                "Japanese-Style","Console-style RPG","PC-style RPG","Puzzle")
metacritic <- metacritic %>% mutate(genre_final = case_when(
  metacritic$genre1 %in% genres_lv1 ~ metacritic$genre1,
  (!(metacritic$genre1 %in% genres_lv1) & (metacritic$genre2 %in% genres_lv1)) ~ metacritic$genre2,
  (!(metacritic$genre1 %in% genres_lv1) & !(metacritic$genre2 %in% genres_lv1) & 
     (metacritic$genre3 %in% genres_lv1)) ~ metacritic$genre3,
  (!(metacritic$genre1 %in% genres_lv1) & !(metacritic$genre2 %in% genres_lv1) & 
     !(metacritic$genre3 %in% genres_lv1)) & ((metacritic$genre1 == "Action RPG") | 
                                                (metacritic$genre2 == "Action RPG") | (metacritic$genre3 == "Action RPG")) ~ "Action RPG",
  (!(metacritic$genre1 %in% genres_lv1) & !(metacritic$genre2 %in% genres_lv1) & 
     !(metacritic$genre3 %in% genres_lv1)) & ((metacritic$genre1 == "Role-Playing") | 
                                                (metacritic$genre2 == "Role-Playing") | (metacritic$genre3 == "Role-Playing")) ~ "Role-Playing",
  (!(metacritic$genre1 %in% genres_lv1) & !(metacritic$genre2 %in% genres_lv1) & 
     !(metacritic$genre3 %in% genres_lv1)) & ((metacritic$genre1 == "Action Adventure") | 
                                                (metacritic$genre2 == "Action Adventure") | (metacritic$genre3 == "Action Adventure")) ~ 
    "Action Adventure",
  (!(metacritic$genre1 %in% genres_lv1) & !(metacritic$genre2 %in% genres_lv1) & 
     !(metacritic$genre3 %in% genres_lv1)) & ((metacritic$genre1 == "Action") | 
                                                (metacritic$genre2 == "Action") | (metacritic$genre3 == "Action")) ~ "Action")
)
metacritic$genre_final <- metacritic$genre_final %>% replace_na("Action")

# Rename some of the genres for grouping
metacritic <- metacritic %>% select(-genre1,-genre2,-genre3,genre=genre_final)
metacritic$genre <- recode(metacritic$genre, 
                           "Beat-'Em-Up"="Fighting",
                           "Massively Multiplayer Online"="Strategy",
                           "Massively Multiplayer"="Strategy",
                           "MOBA"="Strategy",
                           "Console-style RPG"="Japanese RPG",
                           "Japanese-Style"="Japanese RPG",
                           "Driving"="Racing",
                           "Western-Style"="Western RPG",
                           "PC-style RPG"="Western RPG",
                           "Role-Playing"="Western RPG",
                           "Rhythm"="Miscellaneous")

# Reformat console names
metacritic$console <- recode(metacritic$console,
                             "pc"="PC","n64"="N64","gamecube"="Gamecube","wii"="Wii","wii-u"="Wii U",
                             "switch"="Switch","dreamcast"="Dreamcast","ps"="Playstation",
                             "ps2"="Playstation 2","ps3"="Playstation 3",
                             "ps4"="Playstation 4","xbox"="Xbox",
                             "xbox360"="Xbox 360","xboxone"="Xbox One")

# Group duplicate developer names for big developers
dev_replace <- function(df,detected,replaced) {
  return(df %>% mutate(developer=replace(developer,str_detect(developer,detected),replaced)))
}

metacritic <- metacritic %>% 
  dev_replace("Nintendo","Nintendo") %>% 
  dev_replace("Bandai","BandaiNamcoGames") %>%
  dev_replace("Namco","BandaiNamcoGames") %>%
  dev_replace("EA","ElectronicArts") %>%
  dev_replace("Rockstar","RockstarGames") %>%
  dev_replace("Ubisoft","Ubisoft") %>%
  dev_replace("SquareEnix","SquareEnix") %>%
  dev_replace("SquareSoft","SquareEnix") %>%
  dev_replace("Sony","SonyInteractiveEntertainment") %>%
  dev_replace("SCE","SonyInteractiveEntertainment") %>%
  dev_replace("Sega","Sega") %>%
  dev_replace("Microsoft","MicrosoftGameStudios") %>%
  dev_replace("Konami","Konami") %>%
  dev_replace("Koei","Koei") %>%
  dev_replace("Capcom","Capcom") %>%
  dev_replace("Atlus","Atlus")

write.csv(metacritic,file="metacritic_reviews_final.csv")