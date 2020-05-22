library("dplyr")
library("lubridate")
library("leaflet")
library("ggplot2")
library("plotly")

# Reads file from data to analyze
data <- read.table("data/shootings-2018.csv", sep = ",", header = T,
                   stringsAsFactors = FALSE)

### Summary Section
# Number of total shootings. Found by how many rows were in my data
num_shootings <- nrow(data)

# Number of lives lost. Takes the sum of all those that were killed
lives_lost <- sum(data$num_killed, na.rm = TRUE)

# Finding the city that had the most killed and injured
most_impacted_city <- data %>%
  mutate(impact = num_killed + num_injured) %>%
  filter(impact == max(impact, na.rm = TRUE)) %>%
  pull(city)

# State with the most mass shootings (counting mass shootings for states)
# End results in a vector with 2 values (California, Illinois)
state_most_shootings <- data %>%
  group_by(state) %>%
  summarise(total = n()) %>%
  filter(total == max(total, na.rm = TRUE)) %>%
  pull(state)

# Finds total injuries from summing all injured from mass shootings
total_injuries <- sum(data$num_injured, na.rm = TRUE)

### Summary Table
# Groups data by month (after reading the date to be a month) into total
# injuries, total deaths, and number of mass shootings. It is a dataframe
summary_table <- data %>%
  mutate(date = parse_date_time(date, orders = "mdy")) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(
    total_injuries = sum(num_injured, na.rm = TRUE),
    total_deaths = sum(num_killed, na.rm = TRUE),
    number_of_shootings = n()
  )

### Specific place
# Looks at Lakewood's mass shooting data specifically (data will be accessed)
# more specifically in the index page
lakewood <- data %>%
  filter(city == "Lakewood")

### Map
# Creates a map marking places with mass shootings. The size of the markings are
# based on the impact of the shooting, which is calculated by adding killed and 
# injured. Added popup with Location, Date, Killed, Injured information
map <- leaflet(data = data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
    stroke = FALSE,
    radius = ~ (num_killed + num_injured),
    popup = paste0(
      "Location: ", data$address, ", ", data$city, ", ", data$state, "<br>",
      "Date: ", data$date, "<br>",
      "Killed: ", data$num_killed, "<br>",
      "Injured: ", data$num_injured, "<br>"
    )
  )

### Graph
# Contains a chart that shows impact from every state that had a mass shooting. It
# is a bar chart with x being the state, and y being the impact (killed and injured)
impacts_per_state <- ggplot(data = data) +
  geom_col(mapping = aes(x = state, y = num_killed + num_injured)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Injuries and Deaths Combined for every State in 2018 from Mass Shootings") +
  labs(x = "State", y = "Deaths and Injuries Count")
