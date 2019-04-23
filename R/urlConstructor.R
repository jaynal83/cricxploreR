# Construct urls for individual players for all country 
library(rvest)
library(stringr)
library(purrr)

# Function to get
# 1. Country url
# 2. test url
# 3. odi url
# 4. t20 url
# to contruct players url later on

get_country_matchtype_url <- function(url){
  html_page <- read_html(url)

  country_urlA <- html_page %>%
    html_nodes(".ciPlayersHomeCtryList a") %>%
    html_attr("href") 
  
  countryID <- html_page %>%
    html_nodes(".ciPlayersHomeCtryList option") %>%
    html_attr("value") 
  countryID <- countryID[nchar(countryID)>0]
  country_urlB <- str_c("index.html?country=",countryID)
  
  country_url <- str_c("http://www.espncricinfo.com/ci/content/player/", 
                       c(country_urlA, country_urlB)) 
  
  test_url <- str_c(country_url,";class=1") %>%
    str_replace(pattern = "index", replacement = "caps")
  
  
  odi_url <- str_c(country_url,";class=2") %>%
    str_replace(pattern = "index", replacement = "caps")
  
  t20_url <- str_c(country_url,";class=3") %>%
    str_replace(pattern = "index", replacement = "caps")
  
  out <- list(country_url = country_url, 
              test_url = test_url,
              odi_url = odi_url,
              t20_url= t20_url)
  out
}

# Function to construct url for individual players for all countries and for all match type

get_player_url <- function(url){
  html_page <- read_html(url)
  player_url <- html_page %>%
    html_nodes(".ciPlayername a") %>%
    html_attr("href")
  player_url <- str_c("http://www.espncricinfo.com",player_url)
  player_url
}

base_url <- get_country_matchtype_url(
  url = "http://www.espncricinfo.com/ci/content/player/index.html"
)

# Player url for all countries
test_player_url <- unlist(map(base_url$test_url, get_player_url))
odi_player_url <- unlist(map(base_url$odi_url, get_player_url))
t20_player_url <- unlist(map(base_url$t20_url, get_player_url))

# get worldcup 2019 squad urls

get_cwc19_squad_urls <- function(url){
  html_page <- read_html(url)
  squad_url <- html_page %>%
    html_nodes(".squads_list a") %>%
    html_attr("href")
  squad_url <- str_c("http://www.espncricinfo.com", squad_url)
  country_name <- html_page %>%
    html_nodes(".squads_list a") %>%
    html_text()
  country_name <- str_trim(str_replace(country_name, pattern = "Squad", replacement = ""))
  out <- data.frame(country_name = country_name,
                    squad_url = squad_url, 
                    stringsAsFactors = F)
  out
}

cwc19_squad_urls <- get_cwc19_squad_urls(url = "http://www.espncricinfo.com/ci/content/squad/index.html?object=1144415")
