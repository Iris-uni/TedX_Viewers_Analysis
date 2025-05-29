### 2.1 Descriptive Stats (independent variables excluded from model)

# Packages 
library(tidyverse)
library(dplyr)

# Top speakers by average views
top_speakers <- ted_df  %>%
  group_by(speaker) %>%
  summarise(avg_views = mean(views, na.rm = TRUE)
            ) %>%
  arrange(desc(avg_views)) %>%
  slice_head(n = 10)

top_speakers

# Titles with highest views
top_titles <- ted_df  %>%
  arrange(desc(views)) %>%
  select(title, views) %>%
  slice_head(n = 10)

top_titles


# Visualizations 

# Top speakers by average view
ggplot(top_speakers, aes(x = reorder(speaker, avg_views), y = avg_views)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  labs(title = "Top 10 Speakers by Number of Views",
       x = "Speaker", 
       y = "Number of Views") +
  theme_get()

 # Titles with highest views
 ggplot(top_titles, aes(x = reorder(title, views), y = views)) +
   geom_col(fill = "steelblue") +
   coord_flip() + 
   labs(title = "Top 10 Titles by Number of Views",
        x = "Titles", 
        y = "Number of Views") +
   theme_get()
 

### 2.2 Descriptive Stats (independent variables included in model)
# Number of speakers by gender 
ted_df %>%
   count(gender) %>%
   ggplot(aes(x = reorder(gender, n), y = n)) +
   scale_y_continuous(breaks = scales::pretty_breaks()) +
   geom_col(fill = "steelblue") +
   labs(title = "Number of speakers by gender",
        x = "Gender", 
        y = "Count of Speakers") +
   theme_get()
# or
ted_df %>%
  count(gender) %>%
  ggplot(aes(x = "", y = n, fill = gender, label = n)) +
  geom_col(width = 0.6) +
  geom_text(position = position_stack(vjust = 0.5), size = 5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Cumulative Number of Speakers by Gender", fill = "Gender") +
  theme_void(base_size = 14)

# Relation between views and gender 
views_gender <- ted_df %>%
  groupy_by(gender)
  mutate(
    female_views <- views[gender == "female"], 
    male_views <- views[gender == "male"], 
    non_bin_views <- views[gender == "nonbinary"]
  )
  
# Boxplot of number of views per gender 
  views_gender_graph <- ted_df %>%
    ggplot(aes(x = gender, y = views, fill = gender)) +
    geom_boxplot() +
    labs(
      title = "Number of Views per Gender",
      x = "Gender",
      y = "Number of Views"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")

  views_gender_graph

  library(scales)
  
  ggplot(ted_df, aes(x = gender, y = views, fill = gender)) +
    geom_boxplot() +
    scale_y_log10(labels = label_number(scale_cut = cut_si(" "))) +
    labs(
      title = "Views per Gender (Log Scale)",
      x = "Gender",
      y = "Views (log10)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
install.packages("scales")
library(scales)
library(ggplot2)

ggplot(ted_df, aes(x = gender, y = views, fill = gender)) +
  geom_boxplot() +
  scale_y_log10(labels = label_number(scale_cut = cut_si(" "))) +
  labs(
    title = "Views per Gender (Log Scale)",
    x = "Gender",
    y = "Views (log10)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
  
  ggplot(ted_df, aes(x = gender, y = views, fill = gender)) +
    geom_boxplot() +
    scale_y_log10(labels = label_number(scale_cut = cut_si(" "))) +
    labs(
      title = "Views per Gender (Log Scale)",
      x = "Gender",
      y = "Views (log10)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")

  
# Number of talks per topic
  ted_df %>%
    count(topic) %>%
    ggplot(aes(x = reorder(topic, n), y = n)) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    geom_col(fill = "steelblue") +
    coord_flip() + 
    labs(title = "Number of talks per topic",
         x = "Topicsr", 
         y = "Count of Talks") +
    theme_get()

# Relation between topics and views   
  ggplot(ted_df, aes(x = reorder(topic, views, FUN = median), y = views, fill = topic)) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si(" "))) +
    labs(
      title = "Views per Topic (log scale)",
      x = "Topic", y = "Views"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Number of talks per year 
  ted_df %>%
    count(year) %>%
    ggplot(aes(x = as.factor(year), y = n)) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    geom_col(fill = "steelblue") +
    labs(title = "Number of TEDx Talks Over Time",
         x = "Year", 
         y = "Count of Talks") +
    theme_get()

# Relation between year and views
  ggplot(ted_df, aes(x = as.factor(year), y = views, fill = as.factor(year))) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si(" "))) +
    labs(
      title = "Views per Year",
      x = "Year", y = "Views"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  
  
  
  