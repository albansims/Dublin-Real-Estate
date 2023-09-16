filepath <- "C:/Users/Alban/Desktop/Datasets/daft_v_2.csv"
data <- read.csv(filepath)

library(sf)

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


# sf
dublin_sf <- st_as_sf(data_clean, coords = c("longitude", "latitude"), crs = 4326)
dublin_sf <- st_transform(dublin_sf, crs = 4326)

# libraries for plot
library(ggplot2)
library(ggmap)

# This requires a Google API Key in order to create a map from Google Maps
register_google(key = "AIzaSyCVcHiYYjhJi_mRLyHwNC602t4fwGtPy9c")

# Create a basemap of Dublin
dublin_map <- get_map(location = "Dublin, Ireland", zoom = 12)

# Create a ggplot2 plot with aesthetics and a basemap
ggmap(dublin_map) +
  geom_sf(data = dublin_sf,inherit.aes = FALSE, aes(color = price), size = 2) +  # Customize aesthetics
  scale_color_gradient(low = "blue", high = "red") +  # Specify color scale
  labs(title = "Dublin Real Estate Prices") +  # Add a title
  theme_minimal()  # Apply a minimal theme