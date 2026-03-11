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

library(tidyverse)

# Exercise #1: Let's work with gapminder. Load gapminder dataset 
# (from library gapminder) into gpmind and write a comment describing it.

# Solution:
library(gapminder)
gpmind <- gapminder

colnames(gpmind)
summary(gpmind)
unique(gpmind$country)

# the dataset describes life expectancy, population and GDP per capita for different 
# countries in the world for 1952-2007 years

###

# Exercise #2: Count how much countries in each continent, note that 
# each country appear multiple times in the dataset (according to the amount of years)
# so you have to do something with the final table to give correct numbers.

# Solution:
gpmind %>% 
  group_by(continent) %>% 
  summarize(countries_num = n_distinct(country))

###

# Exercise #3: Check the average life expectancy for continents in a simple way 
# (just a mean) and then in the correct way (counting that countries have different 
# population). Write a comment about the difference between results and why second way can be called "correct way".

# Solution:
gpmind %>% 
  group_by(continent) %>% 
  summarize(avg_lifeExp = mean(lifeExp))

gpmind %>% 
  group_by(continent) %>% 
  summarize(true_avg_lifeExp = sum(lifeExp * pop) / sum(pop))

#  there are different values, with higher ones in the second case, because by 
# using mean() function the amount of population is not considered. The second way
# is correct as it is closer to actual mean of continent's population and not 
# a mean of the means of countries in the continent

###

# Exercise #4: Take only years 2002 and 2007 and calculate average
# life expectancy for continents in both simple and correct ways. Write a comment.

# Solution:
gpmind %>% 
  filter(year %in% c(2002, 2007)) %>% 
  group_by(continent) %>% 
  summarize(avg_lifeExp = mean(lifeExp),
            true_avg_lifeExp = sum(lifeExp * pop) / sum(pop))

# the values are almost identical for both ways. It might be the result of
# decreasing the number of years to just 2, thus the weighted average is closer 
# simple mean 

###

# Exercise #5-6: Calculate the total population in continents for 1952 and 2007 (in mlns). 
# Continue the pipe and then calculate the percentage change between 2007 and 1952.
# In other words, by how much % population grew in each continent from 1952 to 2007.
# Tip: Add to the pipe another group_by and summarize, you can call from previously 
# created table in a manner like pop[year==2007] inside of the summarize if on the 
# previous stage we have columns pop and year.

# Solution:
gpmind %>% 
  filter(year %in% c(1952, 2007)) %>% 
  group_by(year, continent) %>% 
  summarize(total_pop = sum(pop)) %>% 
  group_by(continent) %>% 
  summarize(perc_pop_change = (total_pop[year==2007] - total_pop[year==1952]) / total_pop[year==1952] * 100)
###

savehistory("HW5_Yelyzaveta_Motorna.Rhistory")
