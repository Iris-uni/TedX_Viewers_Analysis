# Webscraping 


# Load packages 
library(rvest)
library(httr)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(readr)

# Webscraping function
webscraping_tedx_data <- function(url) {
  
  # Scrape base data
  page <- read_html(url)
  
  # Extract first layer information 
  # speaker name
  # title
  # year of talk
  full_titles <- page %>% html_elements("h3.title a") %>% html_text2()
  split_titles <- str_split(full_titles, "\\|", simplify = TRUE)
  speaker_all <- str_trim(split_titles[,1])
  title_all <- str_trim(split_titles[,2])
  year_all <- as.numeric(page %>% html_elements(".post-categories a") %>% html_attr("href") %>% str_extract("\\d{4}"))
  
  # Extract second layer information
  # Extract talk urls
  talk_urls_all <- page %>% html_elements("h3.title a") %>% html_attr("href")
  
  # Extract third layer information
  # Views vector (make sure length matches talks)
  views_all <- c(6737, 756, 2830, 447, 1027, 1549, 4525, 11949, 2667, 10, 3300000, 825, 867, 895, 1021, 215, 
                 1100, 2138, 19, 5708, 8184, 1200, 692, 4762, 1358, 1241, 1014, 733, 951, 2081, 31, 5718, 1182, 
                 2504, 17193, 6732, 864, 416, 1181, 40835, 970, 13903, 53293, 643, 995, 5170, 998, 
                 12555, 2140, 1886, 4928, 1777, 1154, 1078, 592, 6803, 895, 5589, 934, 3415, 9215, 
                 673, 63, 3114, 1942, 2495, 2858, 1542, 1883)
  
  
  # Filter out music acts where title contains "Live in Concert"
  keep <- !grepl("Live in Concert", title_all, ignore.case = TRUE)
  # Subset all vectors by keep
  speaker <- speaker_all[keep]
  title <- title_all[keep]
  year <- year_all[keep]
  talk_urls <- talk_urls_all[keep]
  views <- views_all[keep]
  
  df <- tibble(
    speaker = speaker,
    title = title,
    year = year,
    talk_urls = talk_urls,
    views = views
  )
  
  # Gender cleaning + Gender assignment 
  # Clean speaker names: remove titles, trim spaces
  ted_df <- ted_df %>%
    mutate(
      speaker_clean = str_remove_all(speaker, regex("(?i)(prof\\.?\\s*)?(dr\\.?\\s*)?", ignore_case = TRUE)) %>%
        str_trim()
    )
  
  # Split speaker_clean into individual names (up to 3 speakers)
  ted_df <- ted_df %>%
    mutate(
      speaker_split = str_split(speaker_clean, "&|,")
    ) %>%
    mutate(
      first_name_1 = map_chr(speaker_split, ~ str_trim(.x)[1] %>% word(1)),
      first_name_2 = map_chr(speaker_split, ~ ifelse(length(.x) >= 2, str_trim(.x)[2] %>% word(1), NA)),
      first_name_3 = map_chr(speaker_split, ~ ifelse(length(.x) >= 3, str_trim(.x)[3] %>% word(1), NA))
    ) %>%
    select(-speaker_split)
  
  # Read in name-gender-df
  gender_df <- read_csv("/Users/iris/Documents/TedX_Viewers_Analysis/name_gender_dataset.csv") %>%
    select(Name, Gender)
  
  # Join gender for each first name
  ted_df <- ted_df %>%
    left_join(gender_df, by = c("first_name_1" = "Name")) %>%
    rename(gender_1 = Gender) %>%
    left_join(gender_df, by = c("first_name_2" = "Name")) %>%
    rename(gender_2 = Gender) %>%
    left_join(gender_df, by = c("first_name_3" = "Name")) %>%
    rename(gender_3 = Gender)
  
  # Combine into overall gender column
  ted_df <- ted_df %>%
    rowwise() %>%
    mutate(
      gender = {
        genders <- c(gender_1, gender_2, gender_3)
        known_genders <- na.omit(genders[genders %in% c("male", "female")])
        
        if (length(known_genders) == 0) {
          "none"
        } else if (all(known_genders == "female")) {
          "female"
        } else if (all(known_genders == "male")) {
          "male"
        } else {
          "both"
        }
      }
    ) %>%
    ungroup()
}
  
  # View the final data
  ted_df
  
  
  
  
  
    

  
 









