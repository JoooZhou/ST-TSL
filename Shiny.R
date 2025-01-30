data <- read.csv(file = "/Users/jjjooo/UFL Dropbox/Yuhang Zhou/research/SP-TSL project/plots/Shiny/ILINet.csv", header = FALSE)
#data <- data[-1,]
clean <- data[c(2,3,4,6)]
colnames(clean) <- c("State", "Year", "Week", "Rate")
us_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
               "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
               "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
               "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
               "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
               "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
               "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

# Filter the data to include only rows where the State is in the list of U.S. states
clean <- clean[clean$State %in% us_states, ]
clean$Week <- as.numeric(clean$Week)
clean <- clean[!is.na(clean$Rate), ]
clean$Rate <- as.numeric(clean$Rate)

# Load necessary libraries
library(dplyr)
library(sf)
library(rnaturalearth)  # For U.S. states boundaries

# Load U.S. state boundaries in sf format
us_map_sf <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska"))  # Exclude Hawaii and Alaska for mainland map focus

# Convert state names in `us_map_sf` to lowercase for consistency
us_map_sf$name <- tolower(us_map_sf$name)

# Calculate centroids of each state
state_centroids <- us_map_sf %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(long = X, lat = Y) %>%
  bind_cols(us_map_sf %>% select(name))

# Manually add Hawaii and Alaska with their approximate centroid coordinates
additional_centroids <- data.frame(
  name = c("hawaii", "alaska"),
  long = c(-156.3319, -149.4937),
  lat = c(20.7967, 64.2008)
)

# Combine the manually added centroids with the original centroids
state_centroids <- bind_rows(state_centroids, additional_centroids)

# Assuming 'clean' is your filtered dataset with columns: State, Year, Week, Rate
# Convert state names to lowercase for consistency with `us_map_sf`
clean$State <- tolower(clean$State)

# Join `clean` data with `state_centroids` by state name to add coordinates
clean_with_coords <- clean %>%
  left_join(state_centroids, by = c("State" = "name"))

# View the resulting data with added coordinates
head(clean_with_coords)

#Data for Graph making
# Filter the data and exclude 2020 week 53
Monitor <- clean_with_coords %>%
  filter(((Year == 2019 & Week >= 1) | 
            (Year == 2020 & Week <= 40) | 
            (Year == 2018 & Week >= 41)) & 
           !(Year == 2020 & Week == 53))

# Adjust week numbers based on conditions
Monitor <- Monitor %>%
  mutate(
    Week = case_when(
      Year == 2018 ~ Week - 40,                          # For 2020, subtract 40 from Week
      Year == 2019 & Week >= 1 & Week <= 40 ~ Week + 12, # For 2021, Week 1-40, add 12
      Year == 2019 & Week >= 41 & Week <= 52 ~ Week - 12,# For 2021, Week 41-52, subtract 12
      Year == 2020 & Week >= 1 & Week <= 40 ~ Week + 12, # For 2022, Week 1-40, add 12
      TRUE ~ Week                                        # Leave other weeks unchanged
    )
  )

# View the resulting subset
head(Monitor)

Monitor <- Monitor %>%
  mutate(time = Week / 52)

Out <- Monitor[c(8,5,6,4)]

# Save the Out data frame as a text file without row names and column names
#write.table(Out, "Out.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)  # For U.S. states boundaries

# Load U.S. state boundaries in sf format
us_map_sf <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(!name %in% c("Hawaii", "Alaska"))  # Exclude Hawaii and Alaska for mainland map focus

# Convert state names in `us_map_sf` to lowercase for consistency
us_map_sf$name <- tolower(us_map_sf$name)

# Assuming 'clean' is your filtered dataset with columns: State, Year, Week, Rate
# Convert state names to lowercase for consistency with `us_map_sf`
clean$State <- tolower(clean$State)

# UI
ui <- fluidPage(
  titlePanel("US ILI Rates by State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = sort(unique(clean$Year))),
      sliderInput("week", "Select Week:", 
                  min = 1, max = 53, 
                  value = 1, 
                  step = 1)  # Initial dummy range; real range will be set dynamically
    ),
    
    mainPanel(
      leafletOutput("map", height = 500)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamically update week slider based on selected year
  observeEvent(input$year, {
    available_weeks <- clean %>%
      filter(Year == input$year) %>%
      pull(Week) %>%
      unique()
    
    # Set min, max, and value to match available weeks for the selected year
    updateSliderInput(session, "week", 
                      min = min(available_weeks, na.rm = TRUE),
                      max = max(available_weeks, na.rm = TRUE),
                      value = min(available_weeks, na.rm = TRUE))
    
    # Print available weeks to the console for debugging
    print(paste("Available weeks for year", input$year, ":", available_weeks))
  })
  
  # Filter data based on selected year and week
  filtered_data <- reactive({
    data <- clean %>%
      filter(Year == input$year, Week == input$week)
    
    # Print filtered data to the console for debugging
    print(paste("Filtering for Year:", input$year, "Week:", input$week))
    print(data)
    
    # Check if data is empty
    if (nrow(data) == 0) {
      showNotification("No data available for this year and week.", type = "error")
    }
    data
  })
  
  # Merge filtered data with US map data
  merged_data <- reactive({
    data <- filtered_data()
    
    # Ensure names match by converting to lowercase and joining by state names
    merged <- us_map_sf %>%
      left_join(data, by = c("name" = "State"))
    
    # Print merged data to console to check if Rates are included
    print("Merged data:")
    print(merged)
    
    merged
  })
  
  # Render the map
  output$map <- renderLeaflet({
    data <- merged_data()
    
    # Define color palette for ILI rates (low = green, mid = yellow, high = red)
    pal <- colorNumeric(palette = c("white", "black", "black"), 
                        domain = clean$Rate, 
                        na.color = "transparent")
    
    # Check if there's data to plot
    if (nrow(data) == 0 || all(is.na(data$Rate))) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addLabelOnlyMarkers(lng = -98.5, lat = 39.8, label = "No data available", 
                            labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE))
    } else {
      leaflet(data = data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(Rate),
          color = "#BDBDC3", weight = 1, opacity = 1, fillOpacity = 0.7,
          highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
          label = ~paste("State:", name, "<br>ILI Rate:", ifelse(is.na(Rate), "No Data", Rate))
        ) %>%
        addLegend(pal = pal, values = ~Rate, opacity = 0.7, title = "ILI Rate",
                  position = "bottomright")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)