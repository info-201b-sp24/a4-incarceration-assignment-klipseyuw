library(dplyr)
library(tidyverse)

jail_population_data <- read_csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-jail-pop.csv")

trends_overtime <- jail_population_data %>%
  filter(year >= 1999) %>%
  group_by(year) %>%
  summarise(
    avg_prisoner_population = mean(total_jail_pop, na.rm = TRUE),
    avg_colored_prisoner_population = mean(latinx_jail_pop + black_jail_pop + aapi_jail_pop + native_jail_pop + other_race_jail_pop, na.rm = TRUE),
    avg_black_prisoner_population = mean(black_jail_pop, na.rm = TRUE),
    avg_aapi_prisoner_population = mean(aapi_jail_pop, na.rm = TRUE),
    avg_latinx_prisoner_population = mean(latinx_jail_pop, na.rm = TRUE),
    avg_native_prisoner_population = mean(native_jail_pop, na.rm = TRUE),
    avg_other_prisoner_population = mean(other_race_jail_pop, na.rm = TRUE),
    avg_white_jail_population = mean(white_jail_pop, na.rm = TRUE)
  ) %>%
  ggplot() +
  geom_line(aes(x = year, y = avg_white_jail_population, color = "White")) +
  geom_line(aes(x = year, y = avg_colored_prisoner_population, color = "Colored")) +
  geom_line(aes(x = year, y = avg_black_prisoner_population, color = "Black")) +
  geom_line(aes(x = year, y = avg_aapi_prisoner_population, color = "AAPI")) +
  geom_line(aes(x = year, y = avg_latinx_prisoner_population, color = "Latinx")) +
  geom_line(aes(x = year, y = avg_native_prisoner_population, color = "Native")) +
  geom_line(aes(x = year, y = avg_other_prisoner_population, color = "Other")) +
  geom_line(aes(x = year, y = avg_prisoner_population, color = "Total")) +
  labs(
    title = "Average Population of Jails from 1999 to 2018 by Race",
    x = "Year",
    y = "Average Population",
    color = "Race"
  )


