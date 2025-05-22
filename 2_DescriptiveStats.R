### 2.1 Descriptive Stats (independent variables excluded from model)

# Packages 
library(tidyverse)
library(dplyr)

# Top speakers by average views
top_speakers <- ted_df  %>%
  group_by(speaker) %>%
  summarise(avg_views = mean(views, na.rm = TRUE)
            ) %>%
  slice_head(n = 10) %>%
  arrange(desc(avg_views))

top_speakers

# Titles with highest views
top_titles <- ted_df  %>%
  arrange(desc(views)) %>%
  select(title, views) %>%
  head(10)

top_titles

# Visualizations 
# Number of TEDx talks over time 
ggplot(ted_df, aes(x = as.factor(year))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of TEDx Talks Over Time",
       x = "Year", 
       y = "Count of Talks") +
  theme_get()

# Top speakers by average view
 ggplot(top_speakers, aes(x = speaker, y =avg_views)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Speakers by Number of Views",
       x = "Speaker", 
       y = "Number of Views") +
  theme_get()

 # Titles with highest views
 ggplot(top_titles, aes(x = title, y = views)) +
   geom_col(fill = "steelblue") +
   coord_flip() + 
   labs(title = "Top 10 Titles by Number of Views",
        x = "Speaker", 
        y = "Number of Views") +
   theme_get()
 

### 2.2 Descriptive Stats (independent variables included in model)