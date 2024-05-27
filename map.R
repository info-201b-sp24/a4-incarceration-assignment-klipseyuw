library(dplyr)
library(readr)
library(sf)
library(maps)
library(ggplot2)

jail_population_data <- read_csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-jail-pop.csv")

most_present_year <- max(jail_population_data$year)

jail_population_state <- jail_population_data %>%
  filter(year == most_present_year) %>%
  group_by(state) %>%
  summarise(avg_population = mean(latinx_jail_pop + black_jail_pop + aapi_jail_pop + native_jail_pop + other_race_jail_pop, na.rm = TRUE))

abbv_to_name <- data.frame(
  abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                 "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                 "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                 "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                 "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                 "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                 "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                 "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

namejail_population_state <- jail_population_state %>%
  left_join(abbv_to_name, by = c("state" = "abbr"))
namejail_population_state <- namejail_population_state %>%
  mutate(name = tolower(name))

population_map <- map_data("state")

state_population_map <- inner_join(namejail_population_state, population_map, by = c("name" = "region"))

population_map <- ggplot(state_population_map, aes(long, lat, group = group, fill = avg_population)) +
  coord_fixed(1.2) +
  theme_classic() + 
  labs(
    title = "Average Incarcerated Colored Population by State in 2018",
    fill = "Population"
  ) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  geom_polygon(color = "white") 

