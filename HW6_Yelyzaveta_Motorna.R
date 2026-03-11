### Instructions
# Each step is critical and may lead to have 0 if not done properly

# 1) Start a new session clicking Session -> New session above
# 2) All exercises have to be done in a new window (new session)
# and nothing else should be done in it
# 3) Write the solution after "# Solution:" comment and before "###"
# It should be the code that will do the exercise 
# and a comment if specified
# 4) When you're done, save the file as HWX_name_surname.R
# For example: HW1_Dmytro_Krukovets.R
# 5) Save the history of your work using save_history().
# For example: savehistory("HW1_Dmytro_Krukovets.Rhistory")
# 6) Send through Moodle both .R file and .Rhistory file

rm(list = ls())
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
counties <- read.csv("counties.csv")

colnames(counties)
# Exercise #1: Convert the counties dataset into a wide format 
# where the columns are state, metro, and region, and the values 
# are the total population in each combination.

# Solution:

counties %>% 
  select(state, metro, region, population) %>%
  group_by(state, metro, region) %>% 
  summarize(total_pop = sum(population)) %>% 
  pivot_wider(names_from=c('state', 'metro', 'region'), values_from=total_pop)
###

# Exercise #2: Calculate the percentage of the population in each metro 
# and non-metro area per region. Then, pivot the data wider 
# to have separate columns for metro and non-metro percentages.
# You'd have 8x4 table

# Solution:
counties %>% 
  group_by(region, metro) %>% 
  summarize(total_pop = sum(population)) %>% 
  pivot_wider(names_from=metro, values_from=total_pop) %>% 
  transmute(region,
            metro_perc = Metro / (Metro+Nonmetro) * 100,
         nonmetro_perc = Nonmetro / (Metro+Nonmetro) * 100) 
  
###

# Exercise #3: From the previous table remove NA in a way to have 4x3 table
# with regions and corresponding Metro and Nonmetro columns. 

# Solution:
counties %>% 
  group_by(region, metro) %>% 
  summarize(total_pop = sum(population, na.rm=T)) %>% 
  pivot_wider(names_from=metro, values_from=total_pop) %>% 
  transmute(region,
            metro_perc = Metro / (Metro+Nonmetro) * 100,
            nonmetro_perc = Nonmetro / (Metro+Nonmetro) * 100) 
###

# Exercise #4: Load the starwars dataset into sw (it's a built-in dataset).
# Check the summary, colnames, write a comment what is this dataset about.
# Is summary ok? :)
# Remove columns which causes problems.

# Solution: 
sw <- starwars

summary(sw)
colnames(sw)

typeof(sw$starships)
# dataset is aboutfeatures of starwars' characters
# the summary is not ok as some of the columns are lists

sw <- sw[, !(colnames(sw) %in% c("starships", "vehicles", "films"))]

###

# Exercise #5: Convert the starwars dataset into a long format 
# where each character’s name is kept, and their attributes 
# (height, mass, birth_year) are in a single column.

# Solution:
sw %>% 
  select(name, height, mass, birth_year) %>%
  pivot_longer(cols=c(height, mass, birth_year), names_to='attributes', values_to='value')
###

# Exercise #6-7: Use an inner_join() to merge the starwars dataset 
# with itself to find characters that have the same eye color and species.
# How much pairs do we have?

# Solution:
sw %>% 
  inner_join(sw, by=c('eye_color', 'species')) %>%  
  nrow()

# 483 pairs of datasets (with replications and self-pairs)

sw %>% 
  inner_join(sw, suffix=c('_x', '_y'), by=c('eye_color', 'species')) %>% 
  filter(name_x != name_y) %>% 
  nrow()/2

# 198 unique pairs

###

# Exercise #8-9: Create a summary table with the average mass 
# and height for each homeworld and save into homeworld_stats. 
# Then, join this dataset with a summary of the average birth year 
# per homeworld and save into birth_year_stats.
# Then join these 2 datasets together.

# Solution:

homeworld_stats <- sw %>% 
  group_by(homeworld) %>% 
  summarize(avg_weight = mean(mass, na.rm=T),
            avg_height = mean(height, na.rm=T))

birth_year_stats <- sw %>% 
  group_by(homeworld) %>% 
  summarize(avg_birth_year = mean(birth_year, na.rm=T)) %>% 
  full_join(homeworld_stats, by='homeworld')

###


savehistory("HW6_Yelyzaveta_Motorna.Rhistory")
