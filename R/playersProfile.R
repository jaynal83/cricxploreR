# Scrapping player's profile from ESPNcricinfo

library(rvest)
library(stringr)
library(purrr)
library(dplyr)


get_player_characteristics <- function(player_url){
  
  player_id <- unlist(str_split(player_url,pattern = "[/.]"))
  player_id <- player_id[str_detect(player_id, pattern = "[0-9]")]
  
  html_page_player <- read_html(player_url)
  player_info <- html_page_player %>%
    html_nodes(".ciPlayerinformationtxt") %>%
    html_text()
  
  player_name <- str_trim(unlist(str_split(player_info[1], pattern = "\n"))[2])
  player_dob <- mdy(player_info[str_detect(player_info, pattern = "Born")])
  
  player_country <- html_page_player %>%
    html_nodes(".ciPlayerlhscountrytxt") %>%
    html_text() %>%
    first()
  
  playing_role <- str_trim(
    str_replace(
      player_info[str_detect(player_info, pattern = "Playing role")], 
      pattern = "Playing role", 
      replacement = ""
    )
  )
  
  batting_style <- str_trim(
    str_replace(
      player_info[str_detect(player_info, pattern = "Batting style")], 
      pattern = "Batting style", 
      replacement = ""
    )
  )
  
  bowling_style <- str_trim(
    str_replace(
      player_info[str_detect(player_info, pattern = "Bowling style")], 
      pattern = "Bowling style", 
      replacement = ""
    )
  )
  
  if(length(player_id)==0)
    player_id <- NA
  
  
  if(length(player_name)==0)
    player_name <- NA
  
  if(length(player_dob)==0)
    player_dob <- NA
  
  if(length(player_country)==0)
    player_country <- NA
  

  if(length(playing_role)==0)
    playing_role <- NA
  
  if(length(batting_style)==0)
    batting_style <- NA
  
  if(length(bowling_style)==0)
    bowling_style <- NA

  df_player_char <- data.frame(player_id, 
                               player_name, 
                               player_dob, 
                               player_country,
                               playing_role, 
                               batting_style, 
                               bowling_style,
                               stringsAsFactors = F)
  df_player_char
}

# ODI player profile
df_player_odi<-map_dfr(odi_player_url, get_player_characteristics)

df_player_odi_noNA <- df_player_odi %>%
  filter(!is.na(player_id))

write.csv(df_player_odi_noNA, file = "df_player_odi.csv", row.names = F)

# Test plyaer profile
df_player_test<-map_dfr(test_player_url, get_player_characteristics)

df_player_test_noNA <- df_player_test %>%
  filter(!is.na(player_id))

write.csv(df_player_test_noNA, file = "df_player_test.csv", row.names = F)

# T20 plyaer profile
df_player_t20<-map_dfr(t20_player_url, get_player_characteristics)

df_player_t20_noNA <- df_player_t20 %>%
  filter(!is.na(player_id))

write.csv(df_player_t20_noNA, file = "df_player_t20.csv", row.names = F)

