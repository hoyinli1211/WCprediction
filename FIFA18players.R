#suppressMessages(library(tidyverse))
#library(stringr)
#suppressMessages(library(lubridate))
#suppressMessages(library(cowplot))

# Note that I set warnings to FALSE because of some annoying (and intermittent)
# issues with RJavaTools.

#library(tabulizer)
urlPlayer <- "https://github.com/davidkane9/wc18/raw/master/fifa_player_list_1.pdf"
out <- extract_tables(urlPlayer, output = "data.frame")
#We now have a 32 element list, each item a data frame of information about the 23 players on each team. Let’s combine this information into a single tidy tibble.

# Note how bind_rows() makes it very easy to combine a list of compatible
# dataframes.

pdf_data <- bind_rows(out) %>% 
  as_tibble() %>% 
  
  # Make the variable names more tidy-like.
  
  rename(team = Team,
         number = X.,
         position = Pos.,
         name = FIFA.Popular.Name,
         birth_date = Birth.Date,
         shirt_name = Shirt.Name,
         club = Club,
         height = Height,
         weight = Weight) %>% 
  
  # Country names are contentious issues. I modify two names because I will
  # later need to merge this tibble with data from Wikipedia, which uses
  # different names.
  
  mutate(team = case_when(
    team == "Korea Republic" ~ "South Korea",
    team == "IR Iran" ~ "Iran",
    TRUE ~ team)) %>% 
  
  # league and club should be separate variables. We also want birth_date to be
  # a date and to have an age variable already calculated.
  
  mutate(birth_date = dmy(birth_date),
         league = str_sub(club, -4, -2),
         club = str_sub(club, end = -7),
         age = interval(birth_date, "2018-06-14") / years(1))
