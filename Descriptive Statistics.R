##Descriptive Statistics##

# Working directory
filepath <- "your_filepath/daft_v_2.csv"
data <- read.csv(filepath)



# cleaning the data
data_clean <- data[complete.cases(data$longitude, data$latitude), ]

data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)
data_clean <- data[complete.cases(data$longitude, data$latitude), ]

# Create a function to convert prices to a common unit (e.g., per month)
convert_to_monthly <- function(price_string) {
  numeric_value <- as.numeric(gsub("[^0-9.]+", "", price_string))  # Remove non-numeric characters
  
  if (grepl("week", tolower(price_string))) {
    return(numeric_value * 4)  # Assuming 4 weeks in a month for prices per week
  } else {
    return(numeric_value)
  }
}

# Apply the function to the "price" column and create a new column "numeric_price"
data_clean$price <- sapply(data_clean$price, convert_to_monthly)
# Remove outlier
data_clean <- data_clean[-1, ]

# Print the modified dataset
print(data_clean)

# libraries for plot
library(ggplot2)
library(ggmap)

# Summary statistics of rental prices
SS <- summary(data_clean$price)
print(SS)
price_mean <- mean(data_clean$price)
price_sd <- sd(data_clean$price)


# Distribution of rental prices
ggplot(data_clean, aes(x = price)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of Rental Prices in Dublin",
       x = "Price (Per Month)",
       y = "Frequency") +
  theme_minimal()

# Box plot of rental prices by property type
ggplot(data_clean, aes(x = property.type, y = price)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Rental Prices by Property Type",
       x = "Property Type",
       y = "Price (Per Month)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot comparing rental prices for furnished, unfurnished, and "Furnished or unfurnished" properties
data_clean <- data_clean[data_clean$furnish != "", ]
ggplot(data_clean, aes(x = furnish, y = price, fill = furnish)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Rental Prices by Furnishing Type",
       x = "Furnishing Type",
       y = "Average Price (Per Month)") +
  scale_fill_manual(values = c("Furnished" = "blue", "Unfurnished" = "red", "Furnished or unfurnished" = "gray")) +
  theme_minimal()

library(dplyr)

# Analyze the distribution of bedrooms and bathrooms
bedroom_distribution <- data_clean %>%
  group_by(bedroom) %>%
  summarize(count = n()) %>%
  arrange(bedroom)

bathroom_distribution <- data_clean %>%
  group_by(bathroom) %>%
  summarize(count = n()) %>%
  arrange(bathroom)

# Create bar plots for bedroom and bathroom distributions
ggplot(bedroom_distribution, aes(x = bedroom, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Distribution of Bedrooms",
       x = "Number of Bedrooms",
       y = "Count") +
  theme_minimal()

ggplot(bathroom_distribution, aes(x = bathroom, y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Distribution of Bathrooms",
       x = "Number of Bathrooms",
       y = "Count") +
  theme_minimal()

# Explore the relationship between bedrooms, bathrooms, and rental prices
ggplot(data_clean, aes(x = bedroom, y = bathroom, color = price)) +
  geom_point() +
  labs(title = "Relationship between Bedrooms, Bathrooms, and Rental Prices",
       x = "Number of Bedrooms",
       y = "Number of Bathrooms") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()

# Determine the most common combinations of bedroom and bathroom counts
common_combinations <- data_clean %>%
  group_by(bedroom, bathroom) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# View the top combinations
head(common_combinations)

ggplot(common_combinations, aes(x = factor(bedroom), y = factor(bathroom), fill = count)) +
  geom_tile() +
  geom_text(aes(label = count), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Top 10 Common Bedroom and Bathroom Combinations",
       x = "Bedrooms",
       y = "Bathrooms") +
  theme_minimal()
