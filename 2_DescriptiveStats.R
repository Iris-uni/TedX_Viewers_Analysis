### 2 Descriptive Statistics


### 2.1 Independent variables - excluded from the model

# Load Packages 
library(tidyverse)
library(dplyr)
library(scales)

# Numbers
# Speakers with most views
top_speakers <- ted_dataframe  %>%
  group_by(speaker) %>%
  summarise(views) %>%
  arrange(desc(views)) %>%
  slice_head(n = 10)

top_speakers

# Titles with most views
top_titles <- ted_dataframe  %>%
  arrange(desc(views)) %>%
  select(title, views) %>%
  slice_head(n = 10)

top_titles


# Visualizations 
# Speakers with most views
ggplot(top_speakers, aes(x = reorder(speaker, views), y = views)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  labs(title = "Top 10 Speakers by Number of Views",
       x = "Speaker", 
       y = "Number of Views") +
  theme_get()

 # Titles with most views
 ggplot(top_titles, aes(x = reorder(title, views), y = views)) +
   geom_col(fill = "steelblue") +
   coord_flip() + 
   labs(title = "Top 10 Titles by Number of Views",
        x = "Titles", 
        y = "Number of Views") +
   theme_get()
 

### 2.2 Independent variables - included in the model

# Gender - Numbers and Visualizations
# Share of talks by gender 
ted_dataframe %>%
  count(gender) %>%
  ggplot(aes(x = "", y = n, fill = gender, label = n)) +
  geom_col(width = 0.6) +
  geom_text(position = position_stack(vjust = 0.5), size = 5) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Share of talks by Gender", fill = "Gender") +
  theme_void(base_size = 14)

# Relationship: Views and gender 
ggplot(ted_dataframe, aes(x = gender, y = views, fill = gender)) +
    geom_boxplot() +
    scale_y_log10(labels = label_number(scale_cut = cut_si(" "))) +
    labs(
      title = "Views per Gender (Log Scale)",
      x = "Gender",
      y = "Views (log10)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
# Topic - Numbers and Visualizations
# Number of talks per topic
ted_dataframe %>%
    count(topic) %>%
    ggplot(aes(x = reorder(topic, n), y = n)) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    geom_col(fill = "steelblue") +
    coord_flip() + 
    labs(title = "Number of talks per topic",
         x = "Topicsr", 
         y = "Count of Talks") +
    theme_get()

# Relation: Topics and views   
  ggplot(ted_dataframe, aes(x = reorder(topic, views, FUN = median), y = views, fill = topic)) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si(" "))) +
    labs(
      title = "Views per Topic (log scale)",
      x = "Topic", y = "Views"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Year - Numbers and Visualizations
# Number of talks per year 
ted_dataframe %>%
    count(year) %>%
    ggplot(aes(x = as.factor(year), y = n)) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    geom_col(fill = "steelblue") +
    labs(title = "Number of TEDx Talks Over Time",
         x = "Year", 
         y = "Count of Talks") +
    theme_get()

# Relation: Year and views
  ggplot(ted_dataframe, aes(x = as.factor(year), y = views, fill = as.factor(year))) +
    geom_boxplot(outlier.alpha = 0.3) +
    scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si(" "))) +
    labs(
      title = "Views per Year",
      x = "Year", y = "Views"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  