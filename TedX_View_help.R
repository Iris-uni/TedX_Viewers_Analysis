# Webscraping 


#Install and load packages 
install.packages("rvest")
install.packages("httr")
install.packages("tibble")
library(rvest)
library(httr)
library(tibble)
library(tidyverse)

# url to scrape
url <- "https://www.tedxfreiburg.com/en/talks-en/"  


# Scraping TedX Freiburg Data
install.packages("stringr")
library(stringr)
library(purrr)



scrape_tedx_data <- function(url) {
  page <- read_html(url)
  
  full_titles <- page %>% html_elements("h3.title a") %>% html_text2()
  split_titles <- str_split(full_titles, "\\|", simplify = TRUE)
  speaker <- str_trim(split_titles[,1])
  title <- str_trim(split_titles[,2])
  year <- page %>% html_element("post-categories a") %>% html_attr("href") %>% str_extract("\\d{4}")
  talk_urls <- page %>% html_elements("h3.title a") %>% html_attr("href")
  views <- c(6737, 756, 2830, 447, 1027, 1549, 4525, 11949, 2667, 129, 3300000, 825, 867, 895, 1021, 215, 1100, 2138, 5125, 5708, 8184, 1200, 692, 4762, 1358, 1241, 1014, 733, 951, 2081, 1224, 5718, 1182, 2504, 17193, 6732, 864, 416, 1181, 40835, 970, 13903, 53293, 643, 995, 5170, 998, 12555, 2140, 1886, 4928, 1777, 1154, 1078, 592, 6803, 895, 5589, 934, 3415, 9215, 673, NA, 3114, 1942, 2495, 2858, 1542, 1883)
  
  
  tibble(
    speaker = speaker,
    title = title, 
    year = year, 
    talk_urls = talk_urls,
    Views = views
  )
}

ted_df <- scrape_tedx_data(url)

# Create More complex variables
names <- ted_df$speaker
ptn_male_name <- "(Ben)|(David)|(Jochem)|(Sebastian)|(Vincent)|(Carsten)|(Danjo)|(Felix)|(Zsolt)|(Ralf)|(Max)|(Alexander)|(El Flecha)|(Phillip)|(Till)|(Markus)|(Gottfried)|(Josef)|(Simon)|(Jan)|(Philipp)|(Norman)|(Wladislav)|(Matthias)|(Marius)|(Michael)|(Claus)|(Boris)|(Bart)|(Sebastian)|(Werner)|(Max)|(Jannis)|(Maurizio)|(Gabriele)|(Ernesto)|(Matthias)|(Rolf)|(Fabio)|(Zweierpasch)"
ptn_nonbin_name <- "(Sound)|(Anna)"

ted_df <- ted_df %>%
  mutate(
    gender = case_when(
      str_detect(names, pattern = ptn_male_name) ~"male", 
      str_detect(names, pattern = ptn_nonbin_name) ~ "nonbinary", 
      TRUE ~ "female"
    )
  )

titles <- ted_df$title
personal_dev <- "(How to Empower Your Brain and Mind)|(My Recipe for a Successful and Happy Life)|(How Attitude Can Change Our Schools)|(Why Bad Feelings Are Good and Normal)|(Free to Fail)|(Dealing With Emotions in the Modern World)|(The Most Extraordinary Journey of Our Lives)|(How a Pink Elephant Can Change Your Life)|(Performance Without Stress and Stage Fright)|(How We Can Overcome The Fear of Rejection)"
society <- "(Why We Need a Culture of Vaginas)|(Our Refugee System Is Failing. How Can We Do Better)|(The Power of Diversity)|(Cars: It’s a Question of Culture)|(Make Metaphors Matter in the Conversation About Global Inequalities)|(Proud Woman)|(Why Integration Is Give and Take)"
environment <- "(The D.I.Y. Energy Revolution — Balcony Photovoltaics Is That Easy)|(How Artificial Intelligence Is Cleaning Up Our Rivers)|(Navigating the Recycling Jungle)|(A Look at the Agriculture of the Future – With Agri-Photovoltaics)|(Climate Change Is a Climate Chance!)|(Plastic – 11 Tips on How to Use Less)|(Without Chimney and Exhaust)|(Flying Wind Farms – Soaring to Climate Neutrality Faster)"
innovation <-"(Decisions Under Uncertainty: Poker Concepts for the Business World)|(I’m Listening To Bats. Professionally. Here Is What I Learned)|(Automation of AI Design)|(Finding Elephants Doesn’t Make Neural Nets Almighty)|(Use Biological Cells to Build Implants!)|(Water in Pixels: Optimizing Water Productivity on the Planetary Scale)"
health <- "(Cancer: Behind the Mask)|(Why We Should All Care About Children Nutrition)|(How Your Immune System Is Fighting for You)|(Counteracting the Effects of Sitting)"
art <- "(Vocal Painting)|(Live in Concert)|(Laughter Needs No Reason!)|(Rap As Positive Rebellion)|(How to Improve Life Within Our Communities Through the Power of Art)|(Think Jazz)|(Art as a Creative Motor for Your Company)"
entrepreneur <- "(Entrepreneurship as the Ultimate Form of Activism)|(Forget Networking — Or Do It Right!)|(Get the Winning Spirit Back – 4 Steps Out of Business Depression)|(How to Start a Startup)|(Entrepreneurship as a Source of Happiness)"
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
      str_detect(titles, pattern = entrepreneur) ~ "entrepreneur", 
      str_detect(titles, pattern = education) ~ "education", 
      str_detect(titles, pattern = social) ~ "social", 
      str_detect(titles, pattern = philosophy) ~ "philosophy"
    )
  )

ted_df

# viewstitle# views <- page %>% html_node(".talk-views") %>% html_text(trim = TRUE)
#   tibble(title = title,speaker = speaker,views = views


# body -> div -> main -> div -> article -> div -> section -> div -> div -> div -> div -> div -> div -> article -> div