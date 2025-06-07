### 1 Webscraping 

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
  
  # DIRECT VARIABLES
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
  # Views vector (replace with Selenium function)
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
  
  # Create a dataframe for variables
  ted_df <- tibble(
    speaker = speaker,
    title = title,
    year = year,
    talk_urls = talk_urls,
    views = views
  )
  
  #INDIRECT VARIABLES
  # 1. Titles 
  titles <- ted_df$title
  personal_dev <- "(How to Empower Your Brain and Mind)|(My Recipe for a Successful and Happy Life)|(How Attitude Can Change Our Schools)|(Why Bad Feelings Are Good and Normal)|(Free to Fail)|(Dealing With Emotions in the Modern World)|(The Most Extraordinary Journey of Our Lives)|(How a Pink Elephant Can Change Your Life)|(Performance Without Stress and Stage Fright)|(How We Can Overcome The Fear of Rejection)"
  society <- "(Why We Need a Culture of Vaginas)|(Our Refugee System Is Failing. How Can We Do Better)|(The Power of Diversity)|(Cars: It’s a Question of Culture)|(Make Metaphors Matter in the Conversation About Global Inequalities)|(Proud Woman)|(Why Integration Is Give and Take)"
  environment <- "(The D.I.Y. Energy Revolution — Balcony Photovoltaics Is That Easy)|(How Artificial Intelligence Is Cleaning Up Our Rivers)|(Navigating the Recycling Jungle)|(A Look at the Agriculture of the Future – With Agri-Photovoltaics)|(Climate Change Is a Climate Chance!)|(Plastic – 11 Tips on How to Use Less)|(Without Chimney and Exhaust)|(Flying Wind Farms – Soaring to Climate Neutrality Faster)"
  innovation <-"(Decisions Under Uncertainty: Poker Concepts for the Business World)|(I’m Listening To Bats. Professionally. Here Is What I Learned)|(Automation of AI Design)|(Finding Elephants Doesn’t Make Neural Nets Almighty)|(Use Biological Cells to Build Implants!)|(Water in Pixels: Optimizing Water Productivity on the Planetary Scale)"
  health <- "(Cancer: Behind the Mask)|(Why We Should All Care About Children Nutrition)|(How Your Immune System Is Fighting for You)|(Counteracting the Effects of Sitting)"
  art <- "(Vocal Painting)|(Laughter Needs No Reason!)|(Rap As Positive Rebellion)|(How to Improve Life Within Our Communities Through the Power of Art)|(Think Jazz)|(Art as a Creative Motor for Your Company)"
  entrepreneurship <- "(Entrepreneurship as the Ultimate Form of Activism)|(Forget Networking — Or Do It Right!)|(Get the Winning Spirit Back – 4 Steps Out of Business Depression)|(How to Start a Startup)|(Entrepreneurship as a Source of Happiness)"
  education <- "(Putting it Simply: Language is for Everyone)|(Just a Game — Why Innovation Needs Room to Play)|(A Cosmic Thought)|(The 10 Stages of Listening)|(Digital Media and Today’s Communication)|(5 Lessons Learned From Photography)"
  social <- "(Why Volunteering Is Sexy)|(Entrepreneurship as the Ultimate Form of Activism)|(Bridging Social and Cultural Gaps Through Biking)|(Youth & Politics – Why Change Sometimes Gets Stuck)|(Choose Your Favorite Problem and Contribute Solving It)|(More Courage, Less Fear of Contact)|(Profit for Good: How Your Washing Machine Will Save the World)"
  philosophy <- "(Listen to Your Heart: How to Prevent Unnecessary Loss)|(The Ugly Face of Beauty)|(Kill the Messenger!)|(The Beauty of Sharing Life With Strangers)|(New Normal – Lost Privacy)|(Why I Walk Through 70 Countries)|(The Fluid ‘Nature’ of Modernity: Just Like a River)"
  
  ted_df <- ted_df %>%
    mutate(
      topic = case_when(
        str_detect(titles, pattern = personal_dev) ~"personal development", 
        str_detect(titles, pattern = society) ~ "society", 
        str_detect(titles, pattern = environment) ~ "environment", 
        str_detect(titles, pattern = innovation) ~ "innovation", 
        str_detect(titles, pattern = health) ~ "health", 
        str_detect(titles, pattern = art) ~ "art", 
        str_detect(titles, pattern = entrepreneurship) ~ "entrepreneurship", 
        str_detect(titles, pattern = education) ~ "education", 
        str_detect(titles, pattern = social) ~ "social", 
        str_detect(titles, pattern = philosophy) ~ "philosophy"
      )
    )
  
  # 2 Gender
  # 2.1 Clean speaker names (remove Dr., Prof., etc.)
  ted_df <- ted_df %>%
    mutate(
      speaker_clean = str_remove_all(speaker, regex("(?i)(prof\\.?\\s*)?(dr\\.?\\s*)?")) %>%
        str_trim()
    )
  
  # 2.2 Split into max 2 speakers and extract first names
  ted_df <- ted_df %>%
    mutate(
      speaker_parts = str_split(speaker_clean, "&|,")
    ) %>%
    mutate(
      first_name_1 = word(str_trim(sapply(speaker_parts, function(x) x[1])), 1),
      first_name_2 = word(str_trim(sapply(speaker_parts, function(x) if(length(x) >= 2) x[2] else NA)), 1)
    ) %>%
    select(-speaker_parts)
  
  # 2.3 Load gender dataset
  # add manual entries
  manual_gender <- tibble::tibble(
    Name = c("Petronela", "Jochem", "Maria-Xenia", "Zweierpasch", "Rym", "Wladislav"),
    Gender = c("female", "male", "female", "male", "female", "male") 
  )
  
  # Load and clean gender dataset
  gender_df <- read_csv("/Users/iris/Documents/TedX_Viewers_Analysis/name_gender_dataset.csv", show_col_types = FALSE) %>%
    mutate(
      Gender = case_when(
        Gender == "M" ~ "male",
        Gender == "F" ~ "female",
        TRUE ~ "none"
      )
    ) %>%
    select(Name, Gender) %>%
    distinct(Name, .keep_all = TRUE)
  
  # Combine both, prioritizing manual entries
  gender_df_combined <- bind_rows(manual_gender, gender_df) %>%
    distinct(Name, .keep_all = TRUE)
  
  # 2.4 Join gender info for each first name
  ted_df <- ted_df %>%
    left_join(gender_df_combined, by = c("first_name_1" = "Name")) %>%
    rename(gender_1 = Gender) %>%
    left_join(gender_df_combined, by = c("first_name_2" = "Name")) %>%
    rename(gender_2 = Gender)
  
  # 2.5 Assign overall gender
  ted_df <- ted_df %>%
    rowwise() %>%
    mutate(
      gender = {
        genders <- na.omit(c(gender_1, gender_2))
        known_genders <- genders[genders %in% c("male", "female")]
        
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
  ted_dataframe <- webscraping_tedx_data("https://www.tedxfreiburg.com/talks/")
  ted_dataframe
  View(ted_dataframe)
  ted_dataframe$speaker[ted_dataframe$gender == "none"]

  
 