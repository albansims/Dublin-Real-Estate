##Geospatial Analysis##

#Use own filepath for directory
filepath <- "your_filepath/daft_v_2.csv"
data <- read.csv(filepath)

# Import libraries for geospatial analysis
library(sf)
library(dplyr)
library(sp)

# Clean the data
data_clean <- data[complete.cases(data$longitude, data$latitude), ]

data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)
data_clean <- data[complete.cases(data$longitude, data$latitude), ]

# Create a function to convert prices to monthly
convert_to_monthly <- function(price_string) {
  numeric_value <- as.numeric(gsub("[^0-9.]+", "", price_string))  # Remove non-numeric characters
  
  if (grepl("week", tolower(price_string))) {
    return(numeric_value * 4) 
  } else {
    return(numeric_value)
  }
}

# Apply the function to the "price" column 
data_clean$price <- sapply(data_clean$price, convert_to_monthly)
# Remove outliers
data_clean <- data_clean[-1, ]

# Print the modified dataset
print(data_clean)


# Create sf data
dublin_sf <- st_as_sf(data_clean, coords = c("longitude", "latitude"), crs = 4326)
dublin_sf <- st_transform(dublin_sf, crs = 4326)

# Import libraries for plot
library(ggplot2)
library(ggmap)

# This requires a Google API Key in order to create a map from Google Maps. Tutorial on my github on how to acess one from Google Cloud service
register_google(key = "")

# Create a basemap of Dublin
dublin_map <- get_map(location = "Dublin, Ireland", zoom = 12)
# Create a basemap of County Dublin
dublin_county_map <- get_map(location = c(lon = -6.2672, lat = 53.349805), zoom = 10)

# Create a ggplot2 plot with aesthetics and a basemap
ggmap(dublin_map) +
  geom_sf(data = dublin_sf,inherit.aes = FALSE, aes(color = price), size = 2) +  
  scale_color_gradient(low = "blue", high = "red") +  
  labs(title = "Dublin Real Estate Prices") +  
  theme_minimal()  

# Create a ggplot2 of geographic distribution of property
ggmap(dublin_map) +
  geom_sf(data = dublin_sf, aes(color = property.type), size = 2, inherit.aes = FALSE) +
  scale_color_manual(values = c("House" = "blue", "Apartment" = "green", "Flat" = "red", "Studio" = "purple")) +
  labs(title = "Geographic Distribution of Property Types") +
  theme_minimal()

# Perform K-means clustering on the geometry (longitude and latitude) of dublin_sf
set.seed(123)
k <- 5  # Choose the number of clusters
coordinates <- st_coordinates(dublin_sf)
kmeans_result <- kmeans(coordinates, centers = k)

# Access cluster assignments for each point
cluster_assignments <- kmeans_result$cluster

# Add cluster assignments back to sf object
dublin_sf$cluster <- cluster_assignments
dublin_sf$cluster <- factor(dublin_sf$cluster)

# Plot the map with clusters
ggmap(dublin_map) +
  geom_sf(data = dublin_sf, inherit.aes = FALSE, aes(color = cluster), size = 2) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +  # Define your custom colors
  labs(title = "K-means Clustering of Dublin Real Estate") +
  theme_minimal()

## Remove Problematic Cluster
# Extract cluster assignments
cluster_assignments <- kmeans_result$cluster
# Find the indices of data points in cluster 3
cluster_3_indices <- which(cluster_assignments == 3)
# Get the corresponding rows from your original dublin_sf dataset
properties_in_cluster_3 <- dublin_sf[cluster_3_indices, ]
# Remove properties in cluster 3 from the dublin_sf dataset
dublin_sf <- dublin_sf[-cluster_3_indices, ]

# Perform K-means clustering on the county map with k = 10
set.seed(123)
k <- 10  # Choose the number of clusters
coordinates <- st_coordinates(dublin_sf)
kmeans_result <- kmeans(coordinates, centers = k)

# Access cluster assignments for each point
cluster_assignments <- kmeans_result$cluster

# Add cluster assignments back to sf object
dublin_sf$cluster <- cluster_assignments
dublin_sf$cluster <- factor(dublin_sf$cluster)

# Plot the map with clusters on bigger map
ggmap(dublin_county_map) +
  geom_sf(data = dublin_sf, inherit.aes = FALSE, aes(color = cluster), size = 2) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "yellow", "brown", "black", "grey", "pink")) +  # Define your custom colors
  labs(title = "K-means Clustering of Dublin Real Estate") +
  theme_minimal()

# Perform K-means clustering on the price with k = 5
set.seed(123)
k <- 5  # Choose the number of clusters
prices <- dublin_sf$price  # Replace "your_data" with the appropriate dataset and column name

# Perform K-means clustering on prices
kmeans_result <- kmeans(matrix(prices), centers = k)

# Access cluster assignments for each data point
cluster_assignments <- kmeans_result$cluster

# Add cluster assignments back to your dataset
dublin_sf$cluster <- cluster_assignments
dublin_sf$cluster <- factor(dublin_sf$cluster)

# Plot the map with clusters
ggmap(dublin_county_map) +
  geom_sf(data = dublin_sf, inherit.aes = FALSE, aes(color = cluster), size = 2) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "yellow", "brown", "black", "grey", "pink")) +  # Define your custom colors
  labs(title = "K-means Clustering of Dublin Real Estate by Price") +
  theme_minimal()

#library(spdep)
## Can't get Moran's to work
# Create a spatial weights matrix 
#w <- dnearneigh(dublin_sf, d1 = 10, d2 = 50)
# Convert 'w' to a listw object
#w_listw <- nb2listw(w)
# Calculate Global Moran's I
#global_moran <- moran.test(dublin_sf$price, listw = w_listw)
# Print the Global Moran's I test results
#print(global_moran)
# Visualize Global Moran's I on the map
#ggmap(dublin_map) +
#geom_sf(data = dublin_sf, inherit.aes = FALSE, aes(fill = global_moran$statistic), size = 2) +
#scale_fill_gradient(low = "blue", high = "red") +
#labs(title = "Global Moran's I of Rental Prices in Dublin") +
#theme_minimal()