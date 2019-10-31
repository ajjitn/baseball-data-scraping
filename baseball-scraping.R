library(tidyverse)
library(rvest)
library(purrr)
# to calculate age from birthdate function
library(eeptools)
# library(attempt)


# Tell Fox who I am and that I'm not malicious!
httr::set_config(httr::user_agent("ajjit123@gmail.com; Scraping data for research"))

# Fox hosts a MLB player list, where you can filter by team. Each Team has a
# TeamId from 1 to 32 (for some reason 27 and 28 aren't used).
teamids <- c(seq(1, 26), seq(29, 32))
url_list <- paste0("https://www.foxsports.com/mlb/players", "?teamId=", teamids)

get_table <- function(url) {
    Sys.sleep(0.5)
    url %>%
        read_html() %>%
        html_nodes(xpath = '//*[@id="wisfoxbox"]/section[2]/div/div[1]/div/table') %>%
        html_table() %>%
        # this returns a list of 1, so lets flatten
        pluck(1) %>%
        as_tibble() %>%
        #sometimes Draft year is int, coerce to char for easy joining with map_df
        mutate(`Draft Year` = as.character(`Draft Year`))
}


#loop through all urls, get tables, and bind them together into a single df
players <- map_df(url_list, get_table) 

# Data cleaning, renaming columns, calculating age from birthday, etc
players <- players %>%
    separate(Player, c("last_name", "first_name")) %>% 
     rename_all(
      funs(
        stringr::str_to_lower(.) %>%
        stringr::str_replace_all(., ' ', '_')
      )) %>% 
      mutate(birthdate = lubridate::mdy(birthdate),
            age_exact = age_calc(birthdate, units = "years"),
            age = floor(age_exact))

write_csv(players, "players_2019.csv")


# loop through all urls, get tables, and bind them together into a single df
# Sometimes there are errors so do this safely in a try catch
# players <- map_try_catch(url_list, get_table, .e = ~ print(.x))
# Draft year is sometimes integer and sometimes character, force all as character
# players <- map(players, ~ mutate_if(., is.numeric, as.character))

