# TURTLE GAMES NOTEBOOK
###############################################################################

# Import data, install libraries, and view the data.

# Install libraries.
install.packages("tidyverse")
install.packages("corrplot")
install.packages("caTools")
install.packages("Metrics")
install.packages("moments")
library(tidyverse)
library(skimr)
library(corrplot)
library(DataExplorer)
library(ggplot2)
library(plotly)
library(caTools)
library(Metrics)
library(moments)

# Import the turtle_reviews.csv
reviews <- read.csv('turtle_reviews.csv', header = T)

# View the first 10 rows.
head(reviews, 10)

# Get the dimensions (the number of rows and columns)
dim(reviews)

# View the structure of the data frame.
str(reviews)

# Note: There are 2000 rows and 11 columns.
#       All the data types are correct but I might turn the product category
#       into a factor if needed.


###############################################################################

# Determine the descriptive statistics.

summary(reviews)

# Notes:
# - Age: between 17 and 72, and mean of 34.
# - Income (renumeration): between £12.3k and £122.3, and mean of £48k.
# - Spending score: between 1 and 99, and mean of 50.
# - Loyalty points: between 25 and 6847. IQR is small (1751-772 = 979)
# for the range which is is 6822. Median = 1276 and Mean = 1578.

# Create a data profile report.
DataExplorer::create_report(reviews)

# Notes:
# - No missing values (will double check)
# - Bar plots show that "language" and "platform" only have one value so I
# will delete this columns.
# - Bar plots also showed that there are more females than males, graduates
# are the most common education level, then PhD, with a basic education level
# the least.

###############################################################################

# Data cleaning and wrangling.

# 1. Remove columns that aren't needed for this analysis:
# "language", "platform", "review", "summary".

# Drop columns
reviews_cleaned <- select(reviews, -language, -platform, -review, -summary)

# View reviews_cleaned.
head(reviews_cleaned, 5)


# 2. Rename columns: remuneration..k.. and spending_score (1-100).

# Rename columns.
reviews_cleaned <- reviews_cleaned %>%
  rename(income_k = remuneration..k..,
         spending_score = spending_score..1.100.)

# View reviews_cleaned.
head(reviews_cleaned, 5)


# 3. Check the unique values in the string columns.
lapply(reviews_cleaned[c("gender", "education")], unique)

# Note:
# - Gender column just has "Male" and "Female".
# - Education column - some of the values are slightly confusing
# and there is inconsistent capitalisation.


# 4. Changing the values in the education column.

reviews_cleaned <- reviews_cleaned %>%
  mutate(education = recode(education, 
                            "Basic" = "Not completed school", 
                            "diploma" = "Completed school",
                            "graduate" = "Graduate", 
                            "postgraduate" = "Postgraduate"))

# Check unique values
unique(reviews_cleaned$education)


# 5. Check for missing values.
sum(is.na(reviews_cleaned))

# Note: There are no missing values.


# 6. Check for duplicates.
sum(duplicated(reviews_cleaned))

# Note: There are 203 duplicates. 
# Check how many duplicates in the original data frame which included the
# reviews and sumamries.
sum(duplicated(reviews))

# Note: There were 0 duplicates. This means that whilst each row is unique
# as it contains a unique review, there are instances where one person wrote
# more than one review, and sometimes wrote more than one review on the same
# product. Therefore, I'm not going to remove these duplicates because they 
# are most likely "good" duplicates that just indicate that multiple reviews
# have been done by a single customer.


# 7. Create a "customer_id" column.

reviews_cleaned <- reviews_cleaned %>%
  group_by(gender, age, income_k, spending_score, loyalty_points, education) %>%
  mutate(customer_id = cur_group_id()) %>%
  ungroup()

# View the new data frame.
head(reviews_cleaned, 10)

# The customer_id column is on the far right of the data frame and 
# is not ordered.

# Move the customer_id column to the far left.
reviews_cleaned <- reviews_cleaned %>%
  select(customer_id, everything())

# View the new data frame.
head(reviews_cleaned)

# Sort the data frame by customer_id ascending.
reviews_cleaned <- reviews_cleaned %>%
  arrange(customer_id)

# View the new data frame.
head(reviews_cleaned)


# 8. Create a new data frame grouped by individual customer, so that my
# analysis is accurate. Otherwise, I would have duplicate values.
reviews_customer <- reviews_cleaned %>%
  distinct(customer_id, .keep_all = TRUE )

# View the new data frame.
head(reviews_customer)
summary(reviews_customer)

# Note: From the environment I can see that my data set has gone from 2000
# rows to 782 rows. This means that whilst there were 2000 reviews, there
# are only 782 individual customers.

# 9. Plot a histogram for loyalty points as I saw this was skewed in the data
# profile report.
hist(reviews_customer$loyalty_points,
          main = "Distribution of Loyalty Points",
          xlab = "Loyalty Points",
          ylab = "Frequency",
          col = "skyblue")

# Note: Most loyalty points are under 2000 even though there are loyalty points
# in all bins up to nearly 7000.

# 10. Plot a box plot for loyalty points to visualise the spread.
boxplot(reviews_customer$loyalty_points,
        main = "Spread of loyalty points",
        ylab = "Loyalty Points",
        col = "skyblue")

# Note: We can see the same thing here - most loyalty points are clustered
# around about 700 - 1300, and there are lots of values that are considered
# outliers.


# 11. Plot a violin plot (with a box plot) for loyalty points to visualise 
# the spread.
ggplot(reviews_customer, aes(x = 1, y = loyalty_points)) +
  geom_violin(fill = 'skyblue') +
  geom_boxplot(fill = 'lightyellow', width = 0.25,
               outlier.color = 'purple', outlier.size = 1,
               outlier.shape = 'circle') +
  labs(title = "Spread of loyalty points",
       y = "Loyalty points") +
  theme_classic()

# Note: Again, we can clearly see that most values are clustered below 2000
# and there are many outliers.
# However, I'm not going to remove these outliers as it doesn't make sense to
# do so given the context of the  data. As seen in the histogram, there is
# consistently values above 2000 (for example, no large gap with no loyalty
# points then suddenly 7000 loyalty points) suggesting that these values aren't
# the result of faulty data entry.
# It also makes sense that there would be lots of customers who would only buy
# the occasional product so have lower loyalty points, whilst there would be
# some very loyal customers who regularly buy products so would have a greater
# number of loyalty points. 


###############################################################################
###############################################################################

# Exploratory analysis

# 1. Calculate the descriptive statistics for loyalty points.
summary(reviews_customer$loyalty_points)

# Summary:
# Min = 25    Max = 6847
# 1st Quartile = 701    Median = 1187    3rd Quartile = 1658
# Mean = 1497

# Calculate the range, IQR, and the standard deviation.

# Range
max(reviews_customer$loyalty_points) - min(reviews_customer$loyalty_points)

# = 6822

# IQR
IQR(reviews_customer$loyalty_points)

# = 957

# Standard deviation
sd(reviews_customer$loyalty_points)

# =1313.242


# Summary: Range is large for the size of the IQR. 
# The standard deviation is high as it measures how much, on average, the
# values in the dataset deviate from the mean.
# Using the mean and the standard deviation, we can calculate the coefficient
# of variation which measures how dispersed the data points are around the mean.
# A low coefficient of variation is preferable (values between 0 and 1)
# Using the formula CV = sd/mean we get that the CV = 0.88 (2dp).
# This is a high result indicating a high amount of variability in loyalty 
# points.


# 2. Create pairplots with the numerical variables.
pairs(reviews_customer[, c('age', 'income_k', 'spending_score', 
                           'loyalty_points')])

# Note: Looking at the pairplots, I can see some linear correlation between
# income and loyalty points, and loyalty points and spending score. There is
# clustering between spending scores and income.


# 3. Create a correlation matrix.

# Create a variable using the relevant numeric variables.
numeric_data <- reviews_customer %>% 
  select(age, income_k, spending_score, loyalty_points)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data)
  
# Create the correlation plot
corrplot(correlation_matrix, method = "circle", tl.col = "black", 
         tl.cex = 0.7)

# View the correlation matrix
correlation_matrix

# Interpretation: loyalty points and income, and loyalty points and spending
# score have a moderate positive correlation.


# 4. Create scatterplots of:
# - Loyalty points vs spending scores
# - Spending scores vs loyalty points
# - Income vs loyalty points

# Note: I'm not going to plot income against loyalty points as loyalty points
# do not impact someone's income.


# Scatterplot 1: Loyalty points vs. spending scores (with line of best fit)
ggplot(reviews_customer, aes(x=loyalty_points, y=spending_score))+
  geom_point(alpha=0.5) +
  geom_smooth(method = lm) +
  labs(title = "Loyalty points vs Spending scores",
       x = "Loyalty points",
       y = "Spending scores") +
  theme_classic()

# Note: We can see the there are lots of points that aren't close to the line
# of best fit e.g. for those with a low loyalty points but high spending scores.
# I'm going to plot a spline which will fit a trend line that isn't necessarily
# a straight line.

# Scatterplot 2: Loyalty points vs. spending scores (with spline)
ggplot(reviews_customer, aes(x=loyalty_points, y=spending_score))+
  geom_point(alpha=0.5) +
  geom_smooth() +
  labs(title = "Loyalty points vs Spending scores",
       x = "Loyalty points",
       y = "Spending scores") +
  theme_classic()

# Note: This still struggles to take into account those values for low
# loyalty points.

# Scatterplot 3: Spending scores vs. loyalty points (with line of best fit)
ggplot(reviews_customer, aes(x=spending_score, y=loyalty_points))+
  geom_point(alpha=0.5) +
  geom_smooth(method = lm) +
  labs(title = "Spending scores vs Loyalty points",
       x = "Spending scores",
       y = "Loyalty points") +
  theme_classic()

# Note: We can see the there are lots of points that aren't close to the line
# of best fit e.g. for those with high spending scores.
# I'm going to plot a spline which will fit a trend line that isn't necessarily
# a straight line.

# Scatterplot 4: Spending scores vs. loyalty points (spline)
ggplot(reviews_customer, aes(x=spending_score, y=loyalty_points))+
  geom_point(alpha=0.5) +
  geom_smooth() +
  labs(title = "Spending scores vs Loyalty points",
       x = "Spending scores",
       y = "Loyalty points") +
  theme_classic()

# Note: Not much changed. Nearly a straight line, but struggling to take into
# account those high value spending scores.


# Scatterplot 5: Income vs. loyalty points (with line of best fit)
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points))+
  geom_point(alpha=0.5) +
  geom_smooth(method = lm) +
  labs(title = "Income vs Loyalty points",
       x = "Income (k£)",
       y = "Loyalty points") +
  theme_classic()

# Note: We can see the there are lots of points that aren't close to the line
# of best fit e.g. for those with a higher income. Note that for those with a
# lower income, they are much closer to this line of best fit. It's more
# difficult to predict the loyalty points for those on a high income.
# I'm going to plot a spline which will fit a trend line that isn't necessarily
# a straight line.

# Scatterplot 6: Income vs. loyalty points (with spline)
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points))+
  geom_point(alpha=0.5) +
  geom_smooth() +
  labs(title = "Income vs Loyalty points",
       x = "Income (k£)",
       y = "Loyalty points") +
  theme_classic()

# Note: Not much changed. Still roughly linear but with more margin of error
# for the higher income values.


# 5. Create a "loyalty_category" column to compare low, medium, and high
# loyalty.
# As mentioned in my Jupyter notebook, the splitting of loyalty points into
# categories is quite arbitrary as I can't speak to the business stakeholders
# about what they class as low, medium, or high loyalty. As there are lots of
# customers with lower levels of loyalty, splitting the high loyalty by the
# upper quartile gives too low a boundary for high loyalty.
# Therefore, I'm taking the first 25% as low loyalty, the top 10% as high 
# loyalty, and the rest as medium loyalty.

# Split the data into the "low" and "high" loyalty categories.
low <- quantile(reviews_customer$loyalty_points, 0.25)
high <- quantile(reviews_customer$loyalty_points, 0.90)

# Create a "loyalty_category" column.
reviews_customer <- reviews_customer %>%
  mutate(loyalty_category = case_when(
    loyalty_points <= low ~ "Low loyalty",
    loyalty_points >= high ~ "High loyalty",
    TRUE ~ "Medium loyalty"))

# View the loyalty_category and loyalty_points columns.
head(reviews_customer[, c("loyalty_points", "loyalty_category")])


# 6. Bar plot of number of customers in each loyalty category.

# Order the categories - low, medium, high
reviews_customer$loyalty_category <- factor(reviews_customer$loyalty_category,
                    levels = c("Low loyalty", "Medium loyalty", "High loyalty"))

# Plot the bar plot.
ggplot(reviews_customer, aes(x=loyalty_category))+
  geom_bar(fill = 'skyblue') +
  labs(title = "Number of customers in each loyalty category",
       x = "Loyalty category",
       y = "Number of customers") +
  theme_classic()


# 7. Scatterplot of income and loyalty points (with loyalty category)
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points,
                             colour = loyalty_category))+
  geom_point(alpha=0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Income vs Loyalty points",
       x = "Income (k£)",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  scale_color_manual(values = c("Low loyalty" = "coral", 
                                "Medium loyalty" = "orchid", 
                                "High loyalty" = "purple")) +
  theme_classic()

# Note: Here we can see that, while low and medium loyalty customers span from
# low to high income, high loyalty customers have high incomes of above £60k.


# Now remove the points but leave the splines.
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points,
                             colour = loyalty_category))+
  geom_smooth(se = FALSE) +
  labs(title = "Income vs Loyalty points",
       x = "Income (k£)",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  scale_color_manual(values = c("Low loyalty" = "coral", 
                                "Medium loyalty" = "orchid", 
                                "High loyalty" = "purple")) +
  theme_classic()


# 8. Scatterplot of spending scores and loyalty points (with loyalty category)
ggplot(reviews_customer, aes(x=spending_score, y=loyalty_points,
                             colour = loyalty_category))+
  geom_point(alpha=0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Spending scores vs Loyalty points",
       x = "Spending scores",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  scale_color_manual(values = c("Low loyalty" = "coral", 
                                "Medium loyalty" = "orchid", 
                                "High loyalty" = "purple")) +
  theme_classic()


# Note: Whilst those with low and medium loyalty cover the range of spending
# scores, those with high loyalty only have high spending scores.
# Note that this also shows how customers can have low loyalty points but a 
# high spending score, suggesting perhaps that spending score is related to the
# amount they tend to spend when they shop, but loyalty points likely take into
# account both the monetary value of purchases and the number of purchases.

# Now remove the points but leave the splines.
ggplot(reviews_customer, aes(x=spending_score, y=loyalty_points,
                             colour = loyalty_category))+
  geom_smooth(se = FALSE) +
  labs(title = "Spending scores vs Loyalty points",
       x = "Spending scores",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  scale_color_manual(values = c("Low loyalty" = "coral", 
                                "Medium loyalty" = "orchid", 
                                "High loyalty" = "purple")) +
  theme_classic()


# 9. Scatterplot of income and loyalty points (with loyalty category)
# with gender as a facet layer.
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points,
                             colour = loyalty_category))+
  geom_point(alpha=0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Income vs Loyalty points",
       x = "Income (k£)",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  scale_color_manual(values = c("Low loyalty" = "coral", 
                                "Medium loyalty" = "orchid", 
                                "High loyalty" = "purple")) +
  facet_wrap(~gender) +
  theme_minimal()

# Note: Very similar trends for both gender.


# 10. Grouped bar chart showing the percentage of each gender in each loyalty
# category.

# Calculate percentages within each gender
reviews_summary <- reviews_customer %>%
  group_by(gender, loyalty_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create grouped bar chart
ggplot(reviews_summary, aes(x = loyalty_category, y = percentage, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loyalty category distribution by gender",
       x = "Loyalty Category",
       y = "Percentage",
       fill = "Gender") +
  theme_classic() +
  scale_fill_manual(values = c("Male" = "darkblue", "Female" = "forestgreen")) 

# Note: There is little difference between the genders suggesting that gender
# plays little role in deciding which loyalty category the customer will be in.


# 11. Scatterplot of income and loyalty points (with education)
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points,
                             colour = education))+
  geom_point(alpha=0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Distribution of loyalty points and income by education level",
       x = "Income (k£)",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  theme_classic() +
  scale_color_viridis_d(option = "viridis") 


# 12. Scatterplot of income and loyalty points (with education)
# with gender as a facet layer.
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points,
                             colour = education))+
  geom_point(alpha=0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Distribution of loyalty points and income by education level
       and gender",
       x = "Income (k£)",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  facet_wrap(~gender) +
  theme_minimal() +
  scale_color_viridis_d(option = "viridis") 

# Now with the data points.
ggplot(reviews_customer, aes(x=income_k, y=loyalty_points,
                             colour = education))+
  geom_smooth(se = FALSE) +
  labs(title = "Distribution of loyalty points and income by education level
       and gender",
       x = "Income (k£)",
       y = "Loyalty points",
       colour = "Loyalty Category") +
  facet_wrap(~gender) +
  theme_minimal() +
  scale_color_viridis_d(option = "viridis") 


# 13. Bar plot of average loyalty points by age

# Create age bins
reviews_customer <- reviews_customer %>%
  mutate(age_group = cut(age, breaks = seq(min(age), max(age), by = 5), 
                         include.lowest = TRUE))

# Summarize data by age bins
age_summary <- reviews_customer %>%
  group_by(age_group) %>%
  summarise(avg_loyalty = mean(loyalty_points), .groups = "drop")

# Bar chart of binned ages
ggplot(age_summary, aes(x = age_group, y = avg_loyalty)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average loyalty points by age group",
       x = "Age Group",
       y = "Average Loyalty Points") +
  theme_classic()


# 14. Another bar plot of loyalty points by age but with different age
# categories.

# Create an age_category column.
reviews_customer <- reviews_customer %>%
  mutate(age_category = case_when(
    age < 25 ~ "Under 25",
    age >= 25 & age < 40 ~ "25 - 39",
    age >= 40 & age < 60 ~ "40 - 59",
    age > 60 ~ "60+"))

# Reorder the age_category column
reviews_customer$age_category <- factor(reviews_customer$age_category, 
                          levels = c("Under 25", "25 - 39", "40 - 59", "60+"))

# Calculate the average loyalty points per age category (using the mean)
age_loyalty_mean <- reviews_customer %>%
  group_by(age_category) %>%
  summarise(avg_loyalty_points = mean(loyalty_points))

# Create bar plot
ggplot(age_loyalty_mean, aes(x = age_category, y = avg_loyalty_points)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average loyalty points by age group",
       x = "Age Group",
       y = "Average Loyalty Points") +
  theme_classic()

# Note: Under 25s have the fewest lotalty points, 25-39 has the highest - 
# because of children?
# Calculate the average loyalty points per age category (using the median)
# The high loyalty points might be skewing the results so I'm going to use
# the median as well.

# Calculate the average loyalty points per age category (using the median)
age_loyalty_median <- reviews_customer %>%
  group_by(age_category) %>%
  summarise(avg_loyalty_points = median(loyalty_points))

# Create bar plot
ggplot(age_loyalty_median, aes(x = age_category, y = avg_loyalty_points)) +
  geom_col(fill = "skyblue") +
  labs(title = "Median loyalty points by age group",
       x = "Age Group",
       y = "Median Loyalty Points") +
  theme_classic()

# Note: Slightly different results. Here, 40-59 has the highest median loyalty
#  points.


# 15. Income and loyalty points as a scatterplot with loyalty category 
# animation.

# Create an animated scatter plot using loyalty_category in the frame parameter.
inlplc <- plot_ly(reviews_customer,
                  x = ~income_k,
                  y = ~loyalty_points,
                  color = ~ loyalty_category,
                  colors = c("Low loyalty" = "coral",
                             "Medium loyalty" = "orchid",
                             "High loyalty" = "purple"),
                  type = 'scatter',
                  mode = 'markers',
                  frame = ~loyalty_category)

# Customise the plot layout.
inlplc <- inlplc %>%
  layout(
    title = "Income vs Loyalty Points by Loyalty Category",
    title = list(font = 16),
    xaxis = list(title = "Income (k£)"),
    yaxis = list(title = "Loyalty Points"),
    showlegend = FALSE)

# Show plot
inlplc

# Save the plot as an HTML file
htmlwidgets::saveWidget(inlplc, "animated_plot.html")


# 16. Create a pie chart of loyalty categories.

# Calculate the percentages.
loyalty_pie <- reviews_customer %>%
  group_by(loyalty_category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create pie chart.
# Ensure correct order of factor levels
loyalty_pie$loyalty_category <- factor(loyalty_pie$loyalty_category, 
                  levels = c("Low loyalty", "Medium loyalty", "High loyalty"))

ggplot(loyalty_pie, aes(x = "", y = percentage, fill = loyalty_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  theme_void() +  
  labs(title = "Loyalty Category Distribution", fill = "Loyalty Category") +
  scale_fill_manual(values = c("Low loyalty" = "coral", 
                               "Medium loyalty" = "orchid", 
                               "High loyalty" = "purple"))


# 17. Spending scores and loyalty points as a scatterplot with loyalty category 
# animation.

# Create an animated scatter plot using loyalty_category in the frame parameter.
sslplc <- plot_ly(reviews_customer,
                  x = ~spending_score,
                  y = ~loyalty_points,
                  color = ~ loyalty_category,
                  colors = c("Low loyalty" = "coral",
                             "Medium loyalty" = "orchid",
                             "High loyalty" = "purple"),
                  type = 'scatter',
                  mode = 'markers',
                  frame = ~loyalty_category)

# Customise the plot layout.
sslplc <- sslplc %>%
  layout(
    title = "Spending scores vs Loyalty Points by Loyalty Category",
    title = list(font = 16),
    xaxis = list(title = "Spending Scores"),
    yaxis = list(title = "Loyalty Points"),
    showlegend = FALSE)

# Show plot
sslplc

# Save the plot as an HTML file
htmlwidgets::saveWidget(sslplc, "animated_plot2.html")

###############################################################################
###############################################################################

# Create a multiple linear regression model.

# Can we predict the accumulation of loyalty points given the existing 
# features using a MLR model?

# 1. View the correlation matrix again.
# Create the correlation plot
corrplot(correlation_matrix, method = "circle", tl.col = "black", 
         tl.cex = 0.7)

# Note: There was moderate correlation with the loyalty_points, income,
# spending score variables, but there is a weak negative correlation between
# age and spending score. I'm therefore going to start off with the age, income,
# spending score, and loyalty points variables.


# 2. Scale the independent variables using z-score standardisation.
# This is a better choice than normalisation when the data has outliers which
# this data does.
# I'm scaling this data to improve the model performance. I don't have to worry
# about the outliers losing significance as it's loyalty points which has lots
# of outliers. As loyalty points is the y variable, I don't need to scale it.
reviews_customer <- reviews_customer %>%
  mutate(
    age_scaled = scale(age),
    income_scaled = scale(income_k),
    spending_score_scaled = scale(spending_score))


# 3. Split the data into training and testing.
# I'm going to use stratified sampling so that the proportion of different
# values of loyalty points is approximately the same in both training and
# testing data sets.

# Set the seed so this can be reproduced.
set.seed(42)

# Create the split.
split <- sample.split(reviews_customer$loyalty_points, SplitRatio = 0.8)

# Split the data into training and testing.
train_data <- subset(reviews_customer, split == TRUE)
test_data <- subset(reviews_customer, split == FALSE)

# Check the dimensions in each data set. Training should be 80%, testing
# should be 20%.
dim(train_data)
dim(test_data)

# 4. Train the model
MLRmodel <- lm(loyalty_points ~ age_scaled+income_scaled+spending_score_scaled, 
              data=train_data)

# 5. Model summary
summary(MLRmodel)

# Interpreting the summary:
# - p-values: All the p-values are very small (significantly less than 0.05)
# meaning that all the independent variables are statistically significant.
# Age, income, and spending score all contribue significantly to predicting
# loyalty points.
# - R-squared: The model explains 81.1% of the variance in loyalty points which
# is a very good results. The adjusted R-squared is only slighly lower which
# means that adding variables hasn't taken away much value to the model.
# - Residual Standard Error is 573.2 which is high as the range of loyalty
# is about 6500. This means that there are probably large residuals that make 
# predictions less accurate.
# - The coefficients show that income and spending score have a significantly
# greater effect on loyalty points than age. This is because age has a much
# smaller coefficient than income and spending score.

# 7. Plot y_test against y_pred.

# Create a data frame with actual and predicted values.
y_test <- test_data$loyalty_points
y_pred_test <- predict(MLRmodel, test_data)

MLR_results <- data.frame(Actual = y_test, 
                           Predicted = y_pred_test)

ggplot(MLR_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Loyalty Points",
       x = "Actual Loyalty Points",
       y = "Predicted Loyalty Points") +
  theme_classic()

# Interpretation of the plot: Lots of these data points are not close to the 
# dotted red line, so linear regression might not be the best model to use.

# 8. Evaluate how well the model performed on the test data.
mae(y_test, y_pred_test)
mse(y_test, y_pred_test)
rmse(y_test, y_pred_test)

# Interpretation of the evaluation:
# - The errors are high. The RMSE, which puts more weight on large residuals,
# is very high (579.78). Loyalty points have a range of 6822 so the
# average error (MAE) being 459.66 means that on average, the predicted values 
# are 459.66 from the actual value. This means that the MAE is 6.74% of the 
# range and the RMSE is 8.50% of the range.


# 9. Check for multicollinearity
car::vif(MLRmodel)

# Interpretation of VIF values: All are about 1 so there is no
# multicollinearity.


# 10. Check for normality.

# Get the residuals from the model.
residuals <- MLRmodel$residuals

# Create a Q-Q plot of the residuals.
qqnorm(residuals)
qqline(residuals, col = "red")

# Interpretation: The residuals look roughly normal, although the values above
# the line suggest that the model might be underestimating the size of outliers.

# Shapiro-Wilk test:
shapiro.test(residuals)

# Interpretation: The p-values is significantly less than 0.05 meaning that
# there is significant evidence to suggest that the residuals do not follow a
# normal distribution. However, this is not necessarily a problem as the sample
# size is reasonably large so the assumption is less important because of the 
# central limit theorem. Is the residuals are significantly skewed, this can
# still be a problem. I'll plot a histogram of the residuals.

# Histogram of the residuals.
hist(residuals)

# Note: We can can see that the histogram looks like it's not far from having a
# normal distribution so the residuals violating the normality of residuals
# is probably ok.


# 11.  Check for homoscedasticity.

# Plot a residual plot.

y_pred_train <- predict(MLRmodel)

ggplot(data = data.frame(y_pred_train, residuals), aes(x = y_pred_train, 
                                                       y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual plot", x = "y_pred", 
       y = "Residuals") +
  theme_classic()

# Interpretation: There is a clear pattern of residuals here - they are not
# random. They look like they are in a U shape. This means that there is 
# heteroscedasticity which violates an assumption of linear regression. This 
# indicates that the relationship between the independent variables and the 
# loyalty points (the dependent variable) is not linear. It suggests that the 
# model is often not capturing the correct relationship between the independent 
# variables and the dependent variable. It could suggest that the model is 
# underfitting the data as the model might be too simple to be a good model for 
# the complexity of the data.

# I'm going to try doing polynomial MLR to see whether it captures the
# relationship between the variables better.


# 12. Train the model
MLRmodel_2 <- lm(loyalty_points ~ age_scaled + I(age_scaled^2) + 
                        income_scaled + I(income_scaled^2) + 
                        spending_score_scaled + I(spending_score_scaled^2), 
                      data = train_data)

# 13. Model summary
summary(MLRmodel_2)

# Interpretation:
# # - p-values: All the p-values are very small (significantly less than 0.05)
# meaning that all the independent variables are statistically significant.
# Age, income, and spending score all contribue significantly to predicting
# loyalty points.
# - R-squared: The model explains 82.76% of the variance in loyalty points which
# is a very good result. The adjusted R-squared is only slighly lower which
# means that adding variables hasn't taken away much value to the model. This
# R-squared is slightly better than for the normal MLR model.
# - Residual Standard Error is 548.8 which is high as the range of loyalty
# is about 6500. This means that there are probably large residuals that make 
# predictions less accurate. However, this residual standard error is slightly
# better than for the previous model.
# - The coefficients show that that there is an inverted U-shape relationship
# between age_scaled and loyalty points as the coefficient for the quadratic
# term is negative. This means that at lower ages, loyalty points increase with
# age but at higher ages, loyalty points decrease.
# For income, the quadratic term is only slightly negative meaning that at very
# high income levels loyalty points might drop slightly. However, the normal
# coefficient is very high suggesting that overall there is a strong positive 
# effective between income and loyalty point accumulation.
# For spending scores, the small positive quadratic term suggests that at high
# spending scores, loyalty points increase even more. The normal coefficient is
# very high meaning that spending scores have a strong positive correlation with
# loyalty points.


# 14. Plot y_test against y_pred (polynomial regression)

# Create a data frame with actual and predicted values.
y_test <- test_data$loyalty_points
y_pred_test <- predict(MLRmodel_2, test_data)

MLR2_results <- data.frame(Actual = y_test, 
                          Predicted = y_pred_test)

ggplot(MLR_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Loyalty Points (Polynomial Regression)",
       x = "Actual Loyalty Points",
       y = "Predicted Loyalty Points") +
  theme_classic()

# Interpretation: The model isn't great at predicting loyalty points.


# 15. Evaluate how well the model performed on the test data.
mae(y_test, y_pred_test)
mse(y_test, y_pred_test)
rmse(y_test, y_pred_test)

# Interpretation of the evaluation:
# - The errors are high. The RMSE, which puts more weight on large residuals,
# is very high (576.22). Loyalty points have a range of 6822 so the
# average error (MAE) being 422.18 means that on average, the predicted values 
# are 422.18 from the actual value. However, these errors are slighlty lower
# than for the previous model.


# 16. Check for multicollinearity
car::vif(MLRmodel_2)

# Interpretation of VIF values: All are close to 1 so there is no
# multicollinearity.


# 17. Check for normality.

# Get the residuals from the model.
residuals <- MLRmodel_2$residuals

# Create a Q-Q plot of the residuals.
qqnorm(residuals)
qqline(residuals, col = "red")

# Interpretation: The residuals don't look normal (but aren't far off).

# Shapiro-Wilk test:
shapiro.test(residuals)

# Interpretation: The p-values is significantly less than 0.05 meaning that
# there is significant evidence to suggest that the residuals do not follow a
# normal distribution. However, this is not necessarily a problem as the sample
# size is reasonably large so the assumption is less important because of the 
# central limit theorem. Is the residuals are significantly skewed, this can
# still be a problem. I'll plot a histogram of the residuals.

# Histogram of the residuals.
hist(residuals)

# Note: We can can see that the histogram looks like it's not far from having a
# normal distribution so the residuals violating the normality of residuals
# is probably ok.


# 11.  Check for homoscedasticity.

# Plot a residual plot.

y_pred_train <- predict(MLRmodel_2)

ggplot(data = data.frame(y_pred_train, residuals), aes(x = y_pred_train, 
                                                       y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual plot", x = "y_pred", 
       y = "Residuals") +
  theme_classic()

# Interpretation: A very similar results to the normal linear regression.
# There is a clear pattern of residuals here - they are not
# random. They look like they are in a U shape. This means that there is 
# heteroscedasticity which violates an assumption of linear regression. This 
# indicates that the relationship between the independent variables and the 
# loyalty points (the dependent variable) is not linear. It suggests that the 
# model is often not capturing the correct relationship between the independent 
# variables and the dependent variable. It could suggest that the model is 
# underfitting the data as the model might be too simple to be a good model for 
# the complexity of the data.





















