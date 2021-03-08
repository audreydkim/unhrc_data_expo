# Analysis of UNHRC data

# Set up ------------------------------------------------------------------

library(tidyverse)
library(countrycode)

all_data <- read.csv("query_data/population.csv", skip = 14)
# If you have error with above^^ go to sessions>set working directory> 
# choose directory> choose your file

# Simple exploration
dim(all_data)
unique(all_data$Year)
length(unique(all_data$Country.of.origin))
length(unique(all_data$Country.of.asylum))

# Decided to look at only 2020 data
data <- all_data %>% 
  filter(Year == 2020) %>% 
  select(contains("Country"), Asylum.seekers)

# What if i wanted to know what the distinct countries are?
distinct_countries <- data %>% 
  select(Country.of.origin) %>% 
  distinct() %>% 
  View()

# What if i want to know where asylums are coming from from a specific
# country?
country_of_interest <- "ESP" # Spain

country_data <- data %>% 
  filter(Country.of.asylum..ISO. == country_of_interest)

country_name <- countrycode(country_of_interest, origin = 'iso3c', destination = 'country.name') # Get "Spain" from "ESP"

# High level questions ----------------------------------------------------

# From how many countries do asylum seekers come from (into country of interest)?
num_countries <- nrow(country_data)

country_data %>% 
  summarize(total_people = sum(Asylum.seekers)) %>% 
  pull()

top_10_countries <- country_data %>% 
  top_n(10, wt = Asylum.seekers) %>% # selects top 10 rows of the value Asulum.seekers
  arrange(-Asylum.seekers) %>% 
  select(Country.of.origin, Asylum.seekers)

# Map ---------------------------------------------------------------------

shapefile <- map_data("world")

# Get iso3 codes and join our data
shapefile <- shapefile %>% 
  mutate(Country.of.origin..ISO. = countrycode(region, origin = 'country.name', destination = 'iso3c')) %>% 
  left_join(country_data, by = "Country.of.origin..ISO.")

# Create the map
ggplot(data = shapefile) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Asylum.seekers)
               ) +
  labs(title = paste("Number of People Seeking Asylum in", country_name),
       x = "", y = "", fill = "Num. People") + 
  theme_minimal()


