library(dplyr)
library(readr)
library(sf)
library(maps)
library(ggplot2)


jail_population_data <- read_csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-jail-pop.csv")

num_observations <- nrow(jail_population_data)

furthest_year <- min(jail_population_data$year)

most_present_year <- max(jail_population_data$year)

highest_prisoner_population <- jail_population_data %>%
  filter(year == most_present_year) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE))

highest_prisoner_population_county <- highest_prisoner_population$county_name
highest_prisoner_population_value <- highest_prisoner_population$total_jail_pop

lowest_prisoner_population <- jail_population_data %>%
  filter(year == most_present_year) %>%
  filter(total_jail_pop == min(total_jail_pop, na.rm = TRUE))

lowest_prisoner_population_county <- lowest_prisoner_population$county_name
lowest_prisoner_population_value <- lowest_prisoner_population$total_jail_pop

avg_prisoner_population <- jail_population_data %>%
  filter(year == most_present_year) %>%
  summarise(average_prisoner_population = mean(total_jail_pop, na.rm = TRUE)) %>%
  pull(average_prisoner_population)

avg_recent_colored_prisoner_population <- jail_population_data %>%
  filter(year == most_present_year) %>%
  summarise(avg_colored_prisoner_population = mean(latinx_jail_pop + black_jail_pop + aapi_jail_pop + native_jail_pop + other_race_jail_pop, na.rm = TRUE)) %>%
  pull(avg_colored_prisoner_population)

avg_recent_white_jail_population <- jail_population_data %>%
  filter(year == most_present_year) %>%
  summarise(avg_white_jail_pop = mean(white_jail_pop, na.rm = TRUE)) %>%
  pull(avg_white_jail_pop)

avg_furthest_year_colored_prisoner_population <- jail_population_data %>%
  filter(year == furthest_year) %>%
  summarise(avg_colored_prisoner_population = mean(latinx_jail_pop + black_jail_pop + aapi_jail_pop + native_jail_pop + other_race_jail_pop, na.rm = TRUE)) %>%
  pull(avg_colored_prisoner_population)

avg_furthest_year_white_jail_population <- jail_population_data %>%
  filter(year == furthest_year) %>%
  summarise(avg_white_jail_pop = mean(white_jail_pop, na.rm = TRUE)) %>%
  pull(avg_white_jail_pop)
