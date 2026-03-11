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

# Exercise #1: Load the dataset budget_boost into df_budget.
# Check the column names, summary, first few rows, unique values
# in some columns that worth to check (or distribution using table()).
# Write a comment about the dataset and your findings on the structure. 

# Solution:
library(tidyverse)
df_budget <- read_csv("budget_boost.csv")

colnames(df_budget)
colnames(df_budget) <- c('ADMIN1', 'ADMIN2', 'INCO1', 'x2018', 'x2019', 'x2020', 'x2021', 'x2022', 'x2023', 'x2024')

summary(df_budget)
df_budget

unique(df_budget$ADMIN1)
unique(df_budget$ADMIN2)
unique(df_budget$INCO1)

# dataset is about money transfers to regions of Ukraine for 2018-2024 yy. 
# Data gives info about source of the revenues.

###

# Exercise #2: Select only the columns related to revenue type 
# and the years 2020-2024.

# Solution:
df_budget %>% 
  select(INCO1, 6:10)
  ###

# Exercise #3: Filter the dataset to include only state budget data.

# Solution:
df_budget %>% 
  filter(ADMIN1 == "state_budget")
###

# Exercise #4: Arrange the dataset in descending order 
# of 2024 budget values and select only columns 
# ADMIN1, ADMIN2, INCO1 and X2024. Take first 10 rows 
# (function head()) and write a comment on your findings.

# Solution:
df_budget %>% 
  select(ADMIN1, ADMIN2, INCO1, x2024) %>%
  arrange(desc(x2024)) %>% 
  head(10)

# in 2024 the biggest revenues were contributed to the state budget (which is expected).
# Within the local ones, the leaders ware Kyiv, Dnipropetrovska and Kharkivska regions,
# probably as a capital and two harshly damaged territories during the war.

###

# Exercise #5: Create a new column growth_2020_2024 showing 
# the budget growth from 2020 to 2024 in percentages (round to 1 digit).

# Solution:

df_budget %>% 
  mutate(growth_2020_2024 = round((x2024 - x2020) / x2020*100, 1))
###

# Exercise #6: Transmute the dataset to show only the revenue type and 
# its total growth from 2018 to 2024.

# Solution:
df_budget %>% 
  select(3:10) %>% 
  summarize(.by=INCO1, total_growth = sum(x2024 - x2018, na.rm=T) / sum(x2018, na.rm=T)*100)
###

# Exercise #7-9: 
# 1) Analyze more closely 2021, 2022, 2023. Select necessary columns.
# 2) Take tax revenues and non-tax revenues. 
# 3) Remove Crimea, Sevastopol, Donetska, Luhanska, Khersonska and Zaporizska oblasts
# 4) Create variable 2021-2023 that is a percentage change between 
# those years rounded to 1 digit.
# 5) Write a comment on structural changes during the full-scale invasion.

# Solution:
df_budget %>% 
  select(ADMIN2, INCO1, x2021, x2022, x2023) %>% 
  filter(INCO1 %in% c('tax_revenues', 'nontax_revenues')) %>% 
  filter(!ADMIN2 %in% c('crimea', 'sevastopol', 'donetska', 'luhanska', 'khersonska', 'Zaporizska')) %>%
  mutate(x2021_2023 = round((x2023 - x2021) / x2021*100, 1)) %>% 
  print(n=44)

# non-tax revenues for state has increased dramatically by 467% in 2021-2023,
# which is a result of the international support during the war. All local revenues
# that have increased by more than 100% are also related to non-tax ones and 
# the nearest to the battle zones (Mykolaivska, Sumska, Chernivetska, Chernihivska).
# Overall, both types of revenues have increased. The only negative percentage 
# change is the Kyiv non-tax revenue.

  
###



savehistory("HW4_Yelyzaveta_Motorna.Rhistory")
