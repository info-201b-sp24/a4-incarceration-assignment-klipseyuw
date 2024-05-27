library(ggplot2)
library(dplyr)
library(tidyverse)

jail_population_data <- read_csv("https://github.com/melaniewalsh/Neat-Datasets/raw/main/us-jail-pop.csv")

population_overtime <- jail_population_data %>%
  filter(year >= 1999) %>%
  group_by(year) %>%
  summarise(
    colored_population = mean(latinx_jail_pop + black_jail_pop + aapi_jail_pop + native_jail_pop + other_race_jail_pop, na.rm = TRUE)
  )

population_overtime <-  ggplot(population_overtime, aes(x = year, y = colored_population)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Incarcerated Colored Population from 1999 to 2018",
       x = "Year",
       y = "Colored Population")
