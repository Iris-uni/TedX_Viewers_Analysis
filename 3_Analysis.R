### 3 Data Analytics


# 3.1 Data Transformations
# Log-transform the views (outlier)
ted_dataframe$log_views <- log10(ted_dataframe$views) 

# Density-Distribution of log-transformed views 
ggplot(ted_dataframe, aes(x = log_views)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  labs(title = "Log-Transformed Distribution of TEDx Views",
       x = "Log10(Views)", 
       y = "Density") +
  theme_minimal()

# 3.2 Encode independent variables: Categorical variables as factors
ted_dataframe$gender_fac <- as.factor(ted_dataframe$gender)
ted_dataframe$topic_fac <- as.factor(ted_dataframe$topic)
ted_dataframe$year_fac <- as.factor(ted_dataframe$year)

# 3.3 Fit the model
model <- lm(log_views ~ gender_fac + topic_fac + year_fac, data = ted_dataframe)
summary(model)

# 3.4 Check diagnostics 
par(mfrow = c(2,2))
plot(model)
# 1) Linearity 
# 2) Normality 
# 3) Homoscedasticity 
# 4) Outliers

# 3.5 Check for multicolinearity 
library(car)
vif(model)
# no multicollinearity (all VIFs around 1-3)

# 3.6 Hypothesis Tests
# Effect of topic, adjusting for other independent variables
model_red1 <- lm(log(views) ~ gender + year, data = ted_dataframe)
model_full <- lm(log(views) ~ gender + topic + year, data = ted_dataframe)
anova(model_red1, model_full)

# Effect of gender, adjusting for other independent variables
model_red2 <- lm(log(views) ~ topic + year, data = ted_dataframe)
anova(model_red2, model_full)


# Effect of year, adjusting for other independent variables
model_red3 <- lm(log(views) ~ topic + gender, data = ted_dataframe)
anova(model_red3, model_full)

# None of the independent variables have a significant effect on the independent variable.
# Taken together, they only explain 14.47 % of the total variance. 
# Hint: Other variables are potentially stronger


# Potential other variables:
# Language of talk 
# German vs. Non-German speakers 
# Location (Freiburg vs. other Geman cities vs. international places)