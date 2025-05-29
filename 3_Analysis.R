# Data Analytics

# 3.1 Data Transformations
# Log-transform the views (because of outlier)
ted_df$log_views <- log10(ted_df$views) 

# Density of log-transofmed views 
ggplot(ted_df, aes(x = log_views)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", size = 1) +
  labs(title = "Log-Transformed Distribution of TEDx Views",
       x = "Log10(Views)", 
       y = "Density") +
  theme_minimal()


# 3.2 Encode independent variables: Ensure categorical variables are factors
ted_df$gender_fac <- as.factor(ted_df$gender)
ted_df$topic_fac <- as.factor(ted_df$topic)
ted_df$year_fac <- as.factor(ted_df$year)

# 3.3 Fit the model
model <- lm(log_views ~ gender_fac + topic_fac + year_fac, data = ted_df)
summary(model)

# 3.4 Check diagnostics 
par(mfrow = c(2,2))
plot(model)
# 1) Linearity 
# 2) Normality 
# 3) Homoscedasticity 
# 4) Outliers

# 3.5 Check for multicolinearity 
install.packages("car")
library(car)
vif(model)
# no multicollinearity (all VIFs around 1-3)

# 3.6 Hypothesis Tests
# Effect of topic, adjusting for other independent variables
model_red1 <- lm(log(views) ~ gender + year, data = ted_df)
model_full <- lm(log(views) ~ gender + topic + year, data = ted_df)
anova(model_red1, model_full)

# Effect of gender, adjusting for other independent variables
model_red2 <- lm(log(views) ~ topic + year, data = ted_df)
anova(model_red2, model_full)


# Effect of year, adjusting for other independent variables
model_red3 <- lm(log(views) ~ topic + gender, data = ted_df)
anova(model_red3, model_full)

# None of the independent variables have a significantly effect on the independent variable.
# Taken together, they only explain 14.47 % of the total variance. 
# Hint: Other variables are potentially stronger


# Potential other variables:
# Langauge of talk 
# German vs. Non-German speakers 