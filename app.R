library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)
library(shinythemes)
library(rsconnect)
library(config)

filepath <- "daft_v_2.csv"
data <- read.csv(filepath)

#### This is all code from Geospatial Analysis ####
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
# Remove outliers (first three rows) for better price scale
data_clean <- data_clean[-(1:3), ]

# This requires a Google API Key in order to create a map from Google Maps
# Load the configuration file
config <- config::get()
google_api_key <- config$google_api_key
register_google(key = google_api_key)

# Create a basemap of Dublin
dublin_map <- get_map(location = "Dublin, Ireland", zoom = 12)
# Create a basemap of County Dublin
dublin_county_map <- get_map(location = c(lon = -6.2672, lat = 53.349805), zoom = 10)


#### Dashboard ####

ui <- fluidPage(
  titlePanel("Dublin Rental Properties"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", "Price Range", min = 500, max = 10000, value = c(500, 10000)),
      sliderInput("bedrooms", "Number of Bedrooms", min = 1, max = 10, value = c(1, 10)),
      sliderInput("bathrooms", "Number of Bathrooms", min = 1, max = 10, value = c(1, 10)),
      selectInput("propertyType", "Property Type", choices = c("All", "House", "Apartment", "Flat", "Studio"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("County Map", plotOutput("countyMap", height = "800", width = "100%")),
        tabPanel("City Map", plotOutput("cityMap", height = "800", width = "100%"))
      )
    )
  )
)

server <- function(input, output) {
  renderMap <- function(map_location, zoom_level) {
    filtered_data <- data_clean %>%
      filter(price >= input$priceRange[1], price <= input$priceRange[2],
             bedroom >= input$bedrooms[1], bedroom <= input$bedrooms[2],
             bathroom >= input$bathrooms[1], bathroom <= input$bathrooms[2])
    
    if (input$propertyType != "All") {
      filtered_data <- filtered_data %>%
        filter(property.type == input$propertyType)
    }
    
    dublin_map <- get_map(location = map_location, zoom = zoom_level)
    
    ggmap(dublin_map) +
      geom_point(data = filtered_data, aes(x = longitude, y = latitude, color = price), size = 2) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()
  }
  
  output$countyMap <- renderPlot({
    renderMap(c(lon = -6.2672, lat = 53.349805), 10)  # Adjust for County Map
  })
  
  output$cityMap <- renderPlot({
    renderMap(c(lon = -6.2603, lat = 53.3498), 12)  # Adjust for City Map
  })
}


shinyApp(ui = ui, server = server)

account_name <- config$account_name
shinyapps_token <- config$shinyapps_token
shinyapps_secret <- config$shinyapps_secret

rsconnect::setAccountInfo(name = account_name, 
                          token = shinyapps_token, 
                          secret = shinyapps_secret)
rsconnect::deployApp(appName = "dublin_rental_prices_app", forceUpdate = TRUE)
