
### STEP 1: ACCESSING THE API AND CLEANING THE DATA


library(httr)
library(jsonlite)
library(tidyverse)

api_key <- Sys.getenv("TMDB_KEY") #subbed out for security reasons
all_movies <- data.frame()

for (year in 2000:2024) {
  message("Fetching Year: ", year)
  
  for (page in 1:5) {
    response <- GET('https://api.themoviedb.org/3/discover/movie',
                    query=list(
                      api_key = api_key,
                      primary_release_year = year,
                      sort_by = 'popularity_desc',
                      page = page
                    ))
    data <- fromJSON(content(response, 'text'))
    temp_df <- data$results
    
    temp_df <- temp_df %>% select(id, title, release_date, popularity, vote_average, vote_count)
    
    all_movies <- bind_rows(all_movies, temp_df)
    
    Sys.sleep(0.2)
  }
}

view(all_movies)

get_movie_details <- function(movie_id, api_key) {
  url <- paste0('https://api.themoviedb.org/3/movie/', movie_id)
  response <- GET(url, query = list(api_key = api_key))
  
  if (status_code(response) == 200) {
    details <- fromJSON(content(response, 'text'))
    return(data.frame(
      id = details$id,
      budget = details$budget,
      revenue = details$revenue,
      runtime = details$runtime
    ))
  } else {
    return(NULL)
  }
}


library(purrr)
message('Starting deep pull for 2500 movies... this may take 10-15 minutes.')

details_df <- map_dfr(all_movies$id, ~{
  Sys.sleep(0.05)
  get_movie_details(.x, api_key)
}, .progress = TRUE)

message('Pull complete! Merging now...')
final_movie_dataset <- left_join(all_movies, details_df, by = 'id')

write_csv(final_movie_dataset, 'raw_movie_data.csv')
message("All Done! Data is merged and saved to 'tmdb_final_data.csv'")

### Check to make sure tables merged - CHECK
head(final_movie_dataset)



### STEP 2: Scraping a website for Oscar winners

library(rvest)
library(dplyr)
library(stringr)

wiki_url <- 'https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture'
page <- read_html(wiki_url)
tables <- page %>% html_nodes("table.wikitable")

oscar_winners_modern <- bind_rows(
  html_table(tables[[8]], fill=TRUE),
  html_table(tables[[9]], fill=TRUE),
  html_table(tables[[10]], fill=TRUE),
  html_table(tables[[11]], fill=TRUE)
)
# cleaning the year column
oscar_winners_clean <- oscar_winners_modern %>%
  rename(Year = 1, Film = 2) %>%
  mutate(Year_Numeric = as.numeric(str_extract(Year, '\\d{4}'))) %>%
  group_by(Year_Numeric) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    Film_Clean = str_trim(gsub("\\.*?]|\\(.*?\\)", "", Film)),
    Is_Oscar_Winner = "Yes"
  ) %>%
  filter(Year_Numeric >= 2000)

# clean up the main dataset before merging
final_movie_dataset <- final_movie_dataset %>%
  select(-any_of(c('Year_Numeric', 'Production_Company', 'Is_Oscar_Winner'))) %>%
  left_join(oscar_winners_clean, by = c("title" = "Film_Clean")) %>%
  mutate(Is_Oscar_Winner = ifelse(is.na(Is_Oscar_Winner), 'No', Is_Oscar_Winner))




# check to see if works
final_movie_dataset %>%
  filter(title %in% c("Gladiator", "Parasite", "The Departed", "Oppenheimer")) %>%
  select(title, Is_Oscar_Winner)

winners_only <- final_movie_dataset %>%
  filter(Is_Oscar_Winner == 'Yes')
view(winners_only)


### STEP 3: Adding Macroeconomic Context (FRED)
library(tidyquant)


movie_labor_data <- tq_get("CEU5051200001", get = "economic.data", from = "2000-01-01")


movie_labor_yearly <- movie_labor_data %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg_industry_employment = mean(price))

# Merge into final dataset
final_movie_dataset <- final_movie_dataset %>%
  mutate(release_year = year(as.Date(release_date))) %>%
  left_join(movie_labor_yearly, by = c("release_year" = "year"))

# Final view check
head(final_movie_dataset)



