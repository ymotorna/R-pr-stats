
# Exercise #12: Load the diabetes.csv into diab and 
# print the summary of the data. Discuss.

# Solution:

diab <- read.csv('diabetes.csv')
summary(diab)

# Exercise #13: Do the following actions with a line of code for each:
# 1) Find the mean of the BMI column in the dataset.
# 2) Select all rows where the Age is greater than 50.
# 3) Select all patients with Age more than 50 and diabetes


# Solution:

mean(diab$BMI)
diab[diab$Age > 50, ]
diab[diab$Age > 50 & diab$Outcome == 1, ]

# Exercise #14: Create a new factor column "bmi_level" in the dataset that indicates
# whether the BMI is greater than 25 (label as "High BMI" or "Normal BMI").

# Solution:

diab$bmi_lvl <- factor(ifelse(diab$BMI > 25, 'High BMI', 'Normal BMI'))
diab

# Exercise #15. In diabetes dataset compare average BMI
# of those who have diabetes and those who aren't

# Solution:

mean(diab$BMI[diab$Outcome == 1])
mean(diab$BMI[diab$Outcome == 0])

# Exercise #16. Create sorted diabetes by increasing BMI, 
# check average Outcome in the first half 
# and in the second half of the dataset. Explain the result.

# Solution:

diab[order(diab$BMI),]

# Exercise 17: Create a while loop where i starts at 26. 
# Print the "i is not divisible by 11" (where i is a current number) 
# in each iteration and stop if the square is divisible by 11.
# After the loop print "i is divisible by 11" using final "i".
# You may want to use paste0() function in this task

# Solution:
i <- 26

while (i**2 %%11 != 0) {
  paste(i, 'is not divisible by 11')
  i <- i+1
}
paste(i, 'is divisible by 11')


# Exercise #18. Create your own dataframe with students that attend these seminars.
# You need at least 7 students. Then you need columns:
# Name (up to you), Exam I score, Exam II score and put some grades from 0 to 20
# Then create column "passed", factor with values "y" or "n".
# Fill it if total grade is >=30 with "y","n" is otherwise.
# Reorder dataset from top student to "not that top" by exam II
# Also calculate mean and max score for exam I. 
# Show student names who has top grade in each exam and overall.
# Adjust dataset values in a way to have three different students in output :)

# Solution:



ml <- list(c(1, 2, 3), matrix(2:7, 3), 'txt')
ml
m <- ml[[2]]
ml[[2]][2, 1]
typeof(m)


# Exercise #1: Create "vec" with 100 elements from 0 to 10 using runif().
# Write a FOR loop to find the cumulative sum of vec and store in "cumul".

# Solution: 
vec <- runif(100, 0, 10)
cumsum <- 0

for (i in vec) {
  cumsum <- cumsum + i
}

cumsum
###

# Exercise #2: Write a function cube() that takes a number and returns its cube.
# Test it on some numbers: 4, -2

# Solution: 
cube <- function(num) {
  num**3
}

cube(3)
###

# Exercise #3: Modify the cube() function to allow for logical argument show_info 
# (default FALSE) that prints "x to the power of 3 equal to y" where x and y
# are actual numbers, if the show_info is TRUE. Test on 4 with T and -2 with F.

# Solution:
cube <- function(num, show_info = FALSE) {
  if (show_info) {
    print(paste(num, 'to the power of 3 equal to', num**3))
  }
  return(num**3)
}

cube(4, T)
cube(-2, F)
###

# Exercise #4: Create a list called student containing three elements: 
# a name (character), scores (numeric vector), and a boolean (passed) 
# indicating whether the average score is ≥60.

# Solution: 
student <- list(name='a', scores=c(1, 2))
student$passed <- mean(student$scores) >= 60
###
names(student)
# Exercise #5: Write a function list_summary() that takes a list and returns 
# its element names and types. Test it on the list "student" made previously.


# Solution: 
list_summary <- function(lst) {
  for (n in 1:length(lst)) {
    print(names(lst[n]))
    print(typeof(lst[[n]]))
  }
}

list_summary(student)

###

# Exercise #6: Create a dataset of city temperatures for five days 
# in five cities (temp_cities). Use lapply() to calculate the mean temperature 
# for each city.

# Solution:
temp_cities <- list(
  Kyiv = c(2, 3, 5, 4, 3),
  Lviv = c(0, -1, 2, 3, 1),
  Odessa = c(5, 6, 7, 8, 7),
  Kharkiv = c(1, 0, 2, 1, 2),
  Dnipro = c(3, 4, 5, 6, 5))

lapply(temp_cities, mean)
###




# Exercise #7: Write a function day_range() that calculates the temperature 
# range (max-min) for a day and use sapply() to apply it to temp.

# Solution:        
day_range <- function(vec) {                       # ????????????????????????????????
  return(max(vec) - min(vec))
}

sapply(1:length(temp_cities), function(i) {day_range(temp_cities[[i]]})
###

  
  
  
  
# Exercise #8: Write a function custom_summary() that calculates the mean, 
# median, and standard deviation of a numeric vector. Test it on vec

# Solution: 
custom_summary <- function(vec) {
  print(paste('mean:', mean(vec)))
  print(paste('median:', median(vec)))
  print(paste('standard deviation:', sd(vec)))
}

custom_summary(vec)
###

# Exercise #9: Create a function normalize() that takes a numeric vector and  
# rescales its values between 0 and 1. 
# Use sapply() to normalize each city in temp_cities.

# Solution: 
normalize <- function(vec) {
  return(vec=min(vec)/max(vec))
}

sapply(temp_cities, normalize)
###

# Exercise #10: Use function read_excel() from the library readxl 
# on urbanpop.xlsx file and save into pop_1. 
# Don't forget about setwd() routine or use full path

# Solution: 
library(readxl)

pop_1 <- read_excel('urbanpop.xlsx')
###

# Exercise #11: Use excel_sheets on urbanpop.xlsx. Discuss your findings.
# Then try to load the second sheet int pop_2 both by its number and
# its name from the result of excel_sheets

# Solution: 
urb_sheets <- excel_sheets('urbanpop.xlsx')
pop_2 <- read_excel('urbanpop.xlsx', sheet=2)
pop_2 <- read_excel('urbanpop.xlsx', sheet='1967-1974')
###

# Exercise #12*: Use lapply function to load data from urbanpop.xslx file, 
# from multiple sheets, into list "pop_list". 

# Solution: 

pop_list <- lapply(urb_sheets, function(sht) {read_excel('urbanpop.xlsx', sheet=sht)})

###

# Exercise #13: Merge 3 dataframes from pop_list using cbind() function into df. 
# Get rid of unnecessary columns "country from the 2nd and 3rd datasets
# inside the cbind function when you're doing the merge.

# Solution:
df <- cbind(pop_list[[1]], pop_list[[2]][-1], pop_list[[3]][-1])

###

# Exercise #14: We've got a data in a creepy format with k and M instead
# of thousands and millions. Save the second column from df into vec. 
# Use gsub() to change "k" to "e3" and "M" to "e6" which is a scientific 
# format for the data, which stands for 1000 and 1000000. 
# Then do them as numeric and save into vec_num

# Solution: 
vec <- df[2]
vec <- gsub('k', 'e3', vec)
vec <- gsub('M', 'e6', vec)

vec_num <- as.numeric(cat(vec))
typeof(vec_num)
###

# Exercise #15: Use the previous solution to the df and make it numeric.

# Solution:
df_num <- sapply(df, function(sht) {
  sht <- gsub('k', 'e3', sht)
  sht <- gsub('M', 'e6', sht)
  return(as.numeric(cat(sht)))
})

typeof(df_num[[1]])
###






# Exercise #1: Load responses.csv to df. Show the summary. 
# Then show all the column names and the first couple of rows. 
# Discuss about the dataset.

# Solution
df <- read.csv('responses.csv')

summary(df)
colnames(df)
head(rownames(df), 2)
###

# Exercise #2. Give us a summary about Gender. 
# How to make it useful? What function should be used on df$Gender?
# Discuss why this variable may be useful in the research

# Solution
summary(factor(df$Gender))
###

# Exercise #3. Use summary() for spiders column.
# Try to interpret each result. Then make plots
# with plot() for both

# Solution
summary(factor(df$Spiders))

plot(factor(df$Spiders))
plot(factor(df$Gender))
###

# Exercise #4. Do the summary for spiders,
# but for different genders. Make plots as well.
# For this you may'd like to create a subset.
# Discuss the result.

# Solution
male_spiders <- factor(df$Spiders[df$Gender=='male'])
female_spiders <- factor(df$Spiders[df$Gender=='female'])

summary(male_spiders)
summary(female_spiders)

par(mfrow=c(2, 1))
plot(male_spiders, main='Male')
plot(female_spiders, main='Female')
###


# Exercise #6. Repeat for Height and then for Weight.
# Discuss results.

# Solution
summary(df$Height)
summary(df$Weight)

par(mfrow=c(2, 1))
plot(factor(df$Height), main='Height')
plot(factor(df$Weight), main='Weight'))

###

# Exercise #7: Calculate the proportion of respondents who love music (4-5).

# Solution: 
nrow(df[df$Music >= 4, ]) / nrow(df)
###

# Exercise #8: Find how religious the youth is. Check variables 
# God and Religion, create a new variable Faith which is equal to the
# sum of God and Religion divided by 2, rounded to the bottom with floor()
# Check it with summary. Check the percentage of unreligious (1-2) people 
# who believe in God (4-5)

# Solution: 
summary(factor(df$God))

df$Faith <- floor((df$God + df$Religion) / 2)
summary(df$Faith)
summary(factor(df$Faith))

nrow(df[df$God >= 4 & df$Religion <= 2, ]) / nrow(df)
###

# Exercise #9: Advanced summaries. You'd like to show all 
# mins, maxs, means and medians. For this you have to work
# with numeric columns only to avoid errors. 
# Using sapply and function is.integer() find all numeric
# columns. Then subset df with only those columns into df_num.

# Solution: 
df_num <- df[, sapply(df, is.integer)]
###

# Exercise #10*: Continue with df_num. Use sapply 
# with functions min/max/mean/median and na.rm=T
# and then explain the result.

# Solution: 
numeric_summaries <- sapply(df_num, function(x) {
  list(
    min = min(x, na.rm = T),
    max = max(x, na.rm = T),
    mean = mean(x, na.rm = T),
    median = median(x, na.rm = T)
  )
})

num_summary <- tibble(
  variable = colnames(df_num),
  min_val = sapply(df_num, min, na.rm=T),   
  max_val = sapply(df_num, max, na.rm=T),
  mean_val = sapply(df_num, mean, na.rm=T),
  median_val = sapply(df_num, median, na.rm=T)
)
###


df %>% select(God)
select(df, God)




# Load necessary library
library(tidyverse)

# Exercise #1. Load data from "counties.csv" using function
# read_csv() instead of the read.csv().
# Show summary, first few rows and column names. 
# Discuss what is this dataset about.

# Solution:
counties <- read_csv('counties.csv')
summary(counties)
head(counties)
colnames(counties)

###

# Exercise #2: Rewrite in a pipe notation following function:
# sort(unique(counties$state),decreasing=T)

# Solution:
counties$state %>% 
  unique() %>% 
  sort(decreasing=T)
###

# Exercise #3: Select only the columns "state", "county", and "population" 
# from counties using select() function. Write into "counties_selected"

# Solution:
counties_selected <- counties %>% 
  select(state, county, population)
###

# Exercise #4: Select all columns except "population" and "region" 
# from counties. Use "-"

# Solution:
counties %>% 
  select(-population, -region)
###

# Exercise #5: Arrange counties in descending order based on 
# "private_work" percentage. Using arrange() and desc() functions.

# Solution:
counties %>%  
  arrange(desc(private_work)) 

###

# Exercise #6: Filter counties where "metro" is "Metro" and population 
# is greater than 500,000 using filter().

# Solution:
counties %>% 
  filter(metro == 'Metro' & population > 500000)
###

# Exercise #7: Create a new column "total_workforce" 
# as the sum of "private_work" and "public_work" percentages.

# Solution:
counties %>% 
  mutate(total_workforce = private_work + public_work)
###

# Exercise #8: Filter counties that belong to the "South" region and 
# have a population under 50000.

# Solution:
counties %>% 
  filter(region == 'South' & population < 50000)

###

# Exercise #9: Create a new variable "self_employed_pop" 
# as the number of self-employed individuals

# Solution:
counties$self_employed

self_employed_pop <- sum(counties$self_employed * counties$population)

counties %>% 
  mutate(self_employed_pop = self_employed * population)
###

# Exercise #10: Find the county with the highest percentage of public workers.
# You may use head() to take only top observation

# Solution:
counties %>% 
  arrange(desc(public_work)) %>% 
  head(1)
###

# Exercise #11: Filter counties where more than 60% of the population is male and sort by population descending.

# Solution:
colnames(counties)
counties$men

counties %>% 
  filter(men/population > 0.6) %>%
  arrange(desc(population))
###

# Exercise #12: For counties in a single pipe select variables
# state, county, region, population, men, women, citizens
# Then filter that there are more men than women and
# number of citizens is more than 10000 and
# the region is West or NorthEast.
# Then arrange by citizens in descending order. 

#Solution:
counties %>% 
  select(state, county, region, population, men, women, citizens) %>% 
  filter(men > women & citizens > 10000 & (region == 'West' | region == 'NorthEast')) %>%
  arrange(desc(citizens))
###

# Exercise #13: From counties select state, county, population, men, women, 
# self_employed, add the proportion of men called proportion_men, 
# take only counties with Proportion of men bigger than 60%,
# sort by ProportionMen in descending order and save into counties_tmp.

# Solution:
counties_tmp <- counties %>% 
  transmute(state, county, population, men, women, self_employed, 
            proportion_men = men/population*100) %>%
  filter(proportion_men > 60) %>%
  arrange(desc(proportion_men))
###

# Exercise #14. In counties_tmp remake proportion_men 
# not as %, but as decimal with a function mutate()

# Solution:
counties_tmp <- counties_tmp %>% 
  mutate(proportion_men = proportion_men/100)
###

# Exercise #15: Why the following doesn't work?


# Exercise #16: Use transmute over counties
# in a way to leave state, county, population and
# create proportion_citizens. Save into counties_tmp.

# Solution: 
counties_tmp <- counties %>% 
  transmute(state, county, population,
            proportion_Citizens=citizens/population)

counties$citizens
###

# Exercise #17: Compute the proportion of self-employed individuals and 
# filter only counties where it's above 10%, then sort by proportion descending.

# Solution:
counties %>% 
  filter(self_employed > 10) %>%
  arrange(desc(self_employed))

###

# Exercise #18: Create a pipeline that selects relevant columns, 
# filters counties with at least 50000 population, creates a new variable 
# for the male-to-female ratio, and sorts by this ratio.

# Solution:
counties %>% 
  select(population, men, women) %>% 
  filter(population >= 50000) %>%
  mutate(m_to_w = men/women*100) %>% 
  arrange(m_to_w)
###

# Exercise #19: Select state,county,population,private_work,public_work,self_employed,
# compute into total_employment the total percentage of employment
# by summing up  private, public, and self-employment percentages, then filter 
# only counties where it's below 98% from Texas and arrange by descending order.

# Solution:
counties %>% 
  transmute(state, county, population, private_work, public_work, self_employed,
            total_employment = private_work + public_work + self_employed) %>%
  filter(total_employment < 98 & state == 'Texas') %>% 
  arrange(desc(total_employment))

###





library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
counties <- read_csv("counties.csv")

# Exercise #1: Count the number of counties in each metro/non-metro category.

# Solution:

###

# Exercise #2: Count the number of counties in each state where the 
# average household income is above 60,000.



# Solution:
colnames(counties)
head(counties)

counties %>% 
  filter(income > 60000) %>% 
  count(state)
  
###

# Exercise #3: Count the number of counties in each state, weighted by 
# the number of employed people, and sort in descending order.


# Solution:
counties %>% 
  count(state, wt=employed, sort=T)
###

# Exercise #4: Count the number of metro and non-metro counties per state.

# Solution:
counties %>% 
  count(state, metro)

###

# Exercise #5: Find the average percentage of white people throughout all
# counties. Propose 2 approaches, one with mean() and another with summarize()


# Solution:
counties %>% 
  summarize(mean(white))

counties$white %>% 
  mean()
###

# Exercise #6: Find the average percentage of white people throughout all
# counties, which have income more than 70000

# Solution:
counties %>% 
  filter(income > 70000) %>% 
  summarize(mean(white))
###

# Exercise #7: Find the state with the maximum average household income.

# Solution:
counties %>% 
  group_by(state) %>% 
  summarize(avg_mean=mean(income)) %>% 
  arrange(desc(avg_mean)) %>%
  head(1)
  
###

# Exercise #8: Find the county with the lowest amount of people working 
# in the public sector.


# Solution:
counties %>% 
  select(public_work, population, county) %>%
  arrange(public_work/100*population) %>% 
  head(1)

###

# Exercise #9: Compute the total number of unemployed people in each state 
# and sort in descending order.

# Solution:
counties %>% 
  group_by(state) %>% 
  summarize(total_unemployed = sum(unemployment/100*population)) %>% 
  arrange(desc(total_unemployed))


###

# Exercise #10: Find the average and median number of people per county 
# working in private companies.

# Solution:
counties %>% 
  summarize(mean = mean(private_work/100*population), 
            median = median(private_work/100*population))

###

# Exercise #11: Calculate the percentage of the population that works 
# in construction per state.

# Solution:
counties %>% 
  group_by(state) %>% 
  summarize(total_construction = sum(construction/100*population) / sum(population))

###

# Exercise #12: Find the state with the highest average unemployment rate 
# among metro counties.

# Solution:
counties %>% 
  filter(metro == 'Metro') %>% 
  group_by(state) %>% 
  summarize(total_u = mean(sum(unemployment/100*population) / sum(population))) %>% 
  arrange(desc(total_u))
###

# Exercise #13: Compute the total number of counties per state
# and the average number of people walking to work per state.


# Solution:
counties %>% 
  group_by(state) %>% 
  summarize(counties_num = n(), 
            mean_walk_num = sum(walk/100*population) / n())

###

# Exercise #14: Find the state with the highest proportion of people working 
# in self-employment.


# Solution:
counties %>% 
  group_by(state) %>% 
  summarize(total_self_employed = sum(self_employed*population/100) / sum(population)) %>%
  arrange(desc(total_self_employed)) %>%
  head(1)
  
###

# Exercise #15: Compute the proportion of people in each state 
# who are U.S. citizens.

# Solution:
counties %>% 
  group_by(state) %>% 
  summarize(us_citizen = sum(native*population/100) / sum(population))

colnames(counties)
counties$native
###

# Exercise #16: For counties dataset do the following,
# Select variables state, population, white, black, hispanic, native, asian.
# Then for counties where there are more than 20% of black person 
# and more than 10% of hispanic and population is bigger than 150000
# Show the total amount (sum) of black, hispanic, native and asian by state.

# Solution:
counties %>% 
  select(state, population, white, black, hispanic, native, asian) %>% 
  filter(black > 20 & hispanic > 10 & population > 150000) %>% 
  group_by(state) %>% 
  summarize(total_black = sum(black*population/100), 
            total_hispanic = sum(hispanic*population/100), 
            total_native = sum(native*population/100), 
            total_asian = sum(asian*population/100))
###

save_bees <- read_csv('save_the_bees.csv')




# -------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
counties <- read_csv("counties.csv")

colnames(counties)
# Exercise #1: Convert the counties dataset into a wide format where the columns
# are county, state, metro or non-metro (after pivoting) and values are 
# population. Do it with a pivot_wider()

# Solution:
counties_wide <- counties %>% 
  select(state, county, metro, population) %>% 
  pivot_wider(names_from = metro, values_from = population)
  
###

# Exercise #2: Convert the counties dataset into a wide format where each for 
# each state the population is calculated by metro and non-metro. 
# So we'll have columns state, metro, nonmetro and 50 rows for each state.

# Solution:
counties %>%
  select(state,county,population,metro) %>%
  group_by(state,metro) %>%
  summarize(sumpop = sum(population)) %>%
  pivot_wider(names_from = metro, values_from = sumpop)
###

# Exercise #3: Convert the counties dataset into a wide format where each state 
# becomes a column and the values are the total population in that state. 
# Save into counties_pop_wide.

# Solution:
counties_pop_wide <- counties %>% 
  group_by(state) %>%
  summarise(population = sum(population)) %>%
  pivot_wider(names_from = "state", values_from = "population")
  
###

# Exercise #4: Convert the wide-format data back into a long format 
# where the state names become a column again from counties_pop_wide.

# Solution:
counties_pop_wide %>% 
  pivot_longer(cols = everything(), names_to = 'state', values_to = 'population')
###

# Exercise #5: Create a summary table showing the average income in each 
# metro and non-metro area per region, then pivot it wider by metro 
# and save into counties_reg_metro.

# Solution:
counties_reg_metro <- counties %>% 
  group_by(region, metro) %>% 
  summarize(mean_inc = sum(income*population)/sum(population)) %>% 
  pivot_wider(names_from = metro, values_from = mean_inc)
###

# Exercise #6: Create another table by region for average income calculated 
# correctly, save it into counties_reg_tot and connect it to counties_reg_metro 
# with the left_join() and resave the result as the counties_reg_tot.

# Solution:
counties_reg_tot <- counties %>% 
  group_by(region) %>% 
  summarize(mean_inc = sum(income*population)/sum(population))

counties_reg_tot <- counties_reg_metro %>% 
  left_join(counties_reg_tot, by = 'region')
###

# Exercise #7-8: Create a total population by region and metro, make it wider
# to have metro and nonmetro columns, save into counties_reg_metro_pop.
# Then do the same but just by region and save into counties_metro_pop.
# Then to counties_reg_metro_pop join the counties_metro_pop and 
# calculate the share of metro population in total population, same for 
# nonmetro. Then join the counties_reg_tot by region and give suffixes 
# _perc and _inc for percentages and income.
# At the last stage, compare mean_inc with mean_inc_new, which is calculated
# as mean income for metro and non-metro multiplied by the percentage 
# of population in metro and non-metro area. Are they the same? Why? 

# Solution:
counties_reg_metro_pop <- counties %>% 
  group_by(region, metro) %>% 
  summarize(total_pop = sum(population)) %>% 
  pivot_wider(names_from = metro, values_from = total_pop)

counties_metro_pop <- counties %>% 
  group_by(region) %>% 
  summarize(total_pop = sum(population))

counties_reg_metro_pop %>% 
  left_join(counties_metro_pop, by = 'region') %>%
  mutate(metro_share = Metro/total_pop*100, nonmetro_share = Nonmetro/total_pop*100) %>% 
  left_join(counties_reg_tot, by='region', suffix=c('_perc', '_inc')) %>% 
  mutate(mean_ins_new = mean_inc*Metro_perc/100 + mean_inc*Nonmetro_perc/100)

###

# Exercise #9: Perform a left join between the counties dataset and 
# a dataset containing state abbreviations, ensuring that no counties 
# are lost in the process.

# Solution:
library(readxl)
state_abbr <- read_excel('states_abbr.xlsx')

counties %>% 
  left_join(state_abbr, by='state') %>% 
  nrow()

nrow(counties)
###

# Exercise #10: Create a table with percentage of white, black, and other people 
# (columns) and metro / non-metro county (rows)
colnames(counties)
counties$white

# Solution:
counties %>% 
  group_by(metro) %>% 
  summarize(white = sum(white*population/100) / sum(population),
            black = sum(black*population/100) / sum(population),
            other = 1 - white - black) 
  
###

# Exercise #11: Find the population density for each state and 
# the average income for each state, then join them.
# Check the correlation between two.

colnames(counties)
counties$income
# Solution:
pop_dens <- counties %>% 
  group_by(state) %>% 
  summarize(pop_dens = sum(population) / sum(land_area))

inc <- counties %>% 
  group_by(state) %>% 
  summarize(mean_inc = sum(income*population) / sum(population))

dens_inc <- pop_dens %>% 
  left_join(inc, by='state') 

cor(dens_inc$pop_dens, dens_inc$mean_inc)
###

# Exercise #12: Find the average poverty per state and metro area, 
# join the dataset with average income and density (you'll have 5 columns including state),
# then check the correlation between all of them.

# Solution:
abc <- counties %>% 
  group_by(state, metro) %>% 
  summarize(mean_pov = sum(poverty*population) / sum(population)) %>% 
  left_join(dens_inc, by='state') 


cor(abc[, c('mean_pov', 'pop_dens', 'mean_inc')])
###









rm(list = ls())
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Exercise #1: Load ImdbTitleBasics and ImdbTitleRatings
# into imdb_1 and imdb_2. Check column names and first few rows. 
# Join imdb_1 and imdb_2 into imdb, and remove imdb_1 and imdb_2.

# Solution: 
imdb_1 <- read_csv('ImdbTitleBasics.csv')
imdb_2 <- read_csv('ImdbTitleRatings.csv')

colnames(imdb_1)
colnames(imdb_2)

head(imdb_1)
head(imdb_2)

imdb <- left_join(imdb_1, imdb_2, by='tconst')

nrow(imdb_1)
nrow(imdb_2)
###

# Exercise #2: Clean the imdb a little bit. First check the unique 
# genres in the dataset, discuss. Then remove all the movies 
# where the "Adult" in genres (use str_detect() for it) and 
# also remake startYear, endYear and runtimeMinutes as numeric.

# Solution: 
unique(imdb$genres)

imdb <- imdb[str_detect(imdb$genres, 'Adult', T),]

imdb <- imdb %>% 
  mutate(startYear = as.numeric(startYear, na.rm=T),
         endYear = as.numeric(endYear, na.rm=T),
         runtimeMinutes = as.numeric(runtimeMinutes, na.rm=T))
###

# Exercise #3: Show top-2 movies by the runtime in each genre, 
# checking only titles with 1 genre 
# Tip: If there are 2+ genres, what symbol will definitely be in genres?
# Tip2: You may need function slice_head() to take 2 titles from each group.

# Solution: 
imdb %>% 
  filter(str_detect(genres, ',', T)) %>% 
  group_by(genres) %>%
  arrange(runtimeMinutes) %>%
  slice_head(n=2)
###

# Exercise #4: Sort in descending order title average scores
# for each decade, taking only titles with 1k+ votes.

# Solution: 
colnames(imdb)

imdb %>% 
  filter(numVotes >= 1000) %>% 
  mutate(decade = startYear %/% 100 + 1) %>% 
  arrange(decade, desc(averageRating))
###

# Exercise #5: For movies and tvseries make  a table 1x2 with 
# amount of movies and TV series.

# Solution: 
unique(imdb$titleType)

imdb %>% 
  filter(titleType %in% c('movie', 'tvSeries')) %>% 
  count(titleType) %>% 
  pivot_wider(names_from = titleType, values_from = n)
  
  
###

# Exercise #6: Find the most voted title for each year

# Solution: 
imdb %>% 
  group_by(startYear) %>% 
  summarize(max_votes = max(numVotes))

###

# Exercise #7: Compare the rating distribution of movies and TV series.
# Mean, median and standard deviation. 

# Solution: 
imdb %>%  
  filter(titleType %in% c('movie', 'tvSeries')) %>% 
  group_by(titleType) %>%
  summarize(mean_rating = mean(averageRating, na.rm=T),
            median_rating =median(averageRating, na.rm=T),
            sd_rating = sd(averageRating, na.rm=T))
###

# Exercise #9: Use separate_rows() on genres with sep="," to 
# divide titles with multiple genres into multiple rows where each
# observation may appear multiple times, but with different genres. 
# Check the amount of observations in this case. Compare 
# with the amount of observations of just imdb and the case
# where you leave only titles with a single genre. 
# Now check unique genres for imdb with separated rows and compare 
# with imdb with only single genre. 

# Solution:
imdb_sep <- imdb %>% 
  separate_rows(genres, sep=',') %>% 
  nrow()

imdb_1_genre <- imdb %>% 
  filter(str_detect(genres, ',', T)) %>% 
  nrow()

imdb_sep - imdb_1_genre

#-------
imdb %>% 
  separate_rows(genres, sep=',') %>%
  distinct(genres) %>% 
  nrow()

imdb %>% 
  filter(str_detect(genres, ',', T)) %>% 
  distinct(genres) %>% 
  nrow()

###

# Exercise #10: Identify the genre with the highest-rated movies on average.
# If title have 2+ genres, you've got to divide it into 2+ observations 
# for each genre.

# Solution: 
imdb %>% 
  separate_rows(genres, sep=',') %>% 
  summarize(.by = genres,
            mean_rating = mean(averageRating, na.rm=T)) %>% 
  head(1)
  
###





rm(list = ls())
library(tidyverse)
library(gapminder)
library(ggplot)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

gpmind <- gapminder
colnames(gpmind)
# Exercise #1: Scatter plot of logaritmized GDP per capita vs 
# Life Expectancy and continent-based color

# Solution:
gpmind %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_point()
###

# Exercise #2: Scatter plot of GDP per capita vs Life Expectancy with 
# size based on population and transparency 0.6

# Solution:
gpmind %>%  
  ggplot(aes(x = log(gdpPercap), y = lifeExp, color = continent, size=pop)) +
  geom_point(alpha=.6)
###

# Exercise #3: Scatter plot with GDP per capita and Life Expectancy 
# colored by continent for year 2007 only. Add a theme_minimal() in the end

# Solution:
gpmind %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_point() +
  theme_minimal()
###

# Exercise #4: Scatter plot with smooth trend lines per continent.
# Tip: Use both geom_point and geom_smooth(se=F)

# Solution:
gpmind %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(se=F)
###


# Exercise #5: Scatter plot with country labels instead of dots

# Solution:
gpmind %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp, color = continent)) +
  geom_text(aes(label = country))
###

# Exercise #6-7: Annotated scatter plot with years for continents.
# Make an average life expectancy and average gdpPercap.
# For this use a combination of geom_point and geom_text, also make 
# in geom_text parameters vjust=-0.5.

# Solution:
gpmind %>% 
  group_by(year, continent) %>% 
  summarize(mean_le = mean(lifeExp), mean_gdp = mean(gdpPercap)) %>% 
  
  ggplot(aes(x=mean_gdp, y=mean_le, color=continent)) +
  geom_point() +
  geom_text(aes(label=year, vjust=-0.5))

###

# Geom Histogram Exercises

# Exercise #7: Histogram of Life Expectancy in 2007 with bins
# colored to lightblue3 with black border

# Solution:
gpmind %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x=lifeExp)) +
  geom_histogram(fill='lightblue', color='black')
###

# Exercise #8: Histogram of Life Expectancy by continent

# Solution:
gpmind %>% 
  ggplot(aes(x=lifeExp, fill=continent)) +
  geom_histogram(color='grey')
###

# Exercise #9: Density plot of Life Expectancy in 2007 by continent

# Solution:
gpmind %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x=lifeExp, fill=continent)) +
  geom_density(alpha=.5)
  
###

# Exercise #10: Bar chart of number of countries per continent in 2007.
# Try to do it using geom_col() and then with geom_bar().
# In which case you succeded?

# Solution:
gpmind %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(num = n()) %>% 
  ggplot(aes(x=continent, y=num)) +
  geom_col()

gpmind %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x=continent)) +
  geom_bar()
###

# Exercise #11-12: Bar chart of average life expectancy by continent in 2007.
# Make 2 cases, one calculated correctly, another with just a mean. 
# Plot both on the same graph using position "dodge".

# Solution:
gpmind %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(mean_le = mean(lifeExp)) %>% 
  ggplot(aes(x=continent, y=mean_le)) +
  geom_col()

gpmind %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(mean_le = sum(lifeExp*pop) / sum(pop)) %>% 
  ggplot(aes(x=continent, y=mean_le)) +
  geom_col()

###

# Exercise #13: Bar chart of median GDP per capita by continent in 2007

# Solution:
gpmind %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(median_gdp = median(gdpPercap)) %>% 
  ggplot(aes(x=continent, y=median_gdp)) +
  geom_col()
###

# Exercise #14: Bar chart of maximum life expectancy by continent in 1997-2007.

# Solution:
unique(gpmind$year)

gpmind %>% 
  filter(year >= 1997 & year <= 2007) %>% 
  group_by(continent, year) %>% 
  summarize(max_le = max(lifeExp)) %>% 
  ggplot(aes(x=year, y=max_le, fill=continent)) + 
  geom_col(position='dodge') +
  scale_x_continuous(breaks=seq(1997, 2007, 5))
  
###



# ---------------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(gapminder)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

gpmind <- gapminder

# Exercise #1: Line chart of life expectancy over time for a single country (e.g., Germany)

# Solution:
gpmind %>% 
  filter(country == 'Germany') %>% 
  ggplot(aes(x=year, y=lifeExp)) + 
  geom_line() +
  labs(title='Germany Life Expectancy over time')
###

# Exercise #2: Line chart of weighted average life expectancy over time 
# for all continents, line width is 2, add theme_bw.

# Solution:
gpmind %>% 
  group_by(continent, year) %>% 
  summarise(avg_lifeExp = sum(lifeExp*pop) / sum(pop)) %>% 
  ggplot(aes(x=year, y=avg_lifeExp, color=continent)) +
  geom_line(linewidth=2) +
  theme_bw()

###

# Exercise #3: Line chart with points of GDP per capita over time for Japan,
# line width is 1, point size is 3, color for both is "darkblue"

# Solution:
gpmind %>% 
  filter(country == 'Japan') %>% 
  ggplot(aes(x=year, y=gdpPercap)) +
  geom_line(color='darkblue', linewidth=1) +
  geom_point(size=3, color='darkblue')
###

# Exercise #4: Area chart of total population over time for Africa,
# color lightblue3, transparency 0.5, theme bw

# Solution:
gpmind %>% 
  filter(continent == 'Africa') %>% 
  group_by(year) %>% 
  summarize(total_pop = sum(pop)) %>% 
  ggplot(aes(x=year, y=total_pop)) +
  geom_area(color='lightblue3', alpha=0.5) +
  theme_bw()
###

# Exercise #5: Line chart of GDP per capita with a smooth trend line 
# for Rwanda, line has width 1.5 and color darkblue, smoothed line 
# without standard errors, color lightblue3 and line type is dashed, theme bw

# Solution:
gpmind %>% 
  filter(country == 'Rwanda') %>% 
  ggplot(aes(x=year, y=gdpPercap)) +
  geom_line(linewidth=1.5, color='darkblue') +
  geom_smooth(color='lightblue3', se=F, linetype='dashed')
###

# Exercise #6: Area chart of population by continent over time 
# (stacked by continent) with transparency 0.9 and theme bw
# and palette of colors changed with scale_fill_manual()
# to the following palette using values = ... inside of the function: 
# "#003964","#00bbce","#A7C539","#D33E2C","#00665a"

# Solution:
gpmind %>% 
  group_by(continent, year) %>% 
  summarize(total_pop = sum(pop)) %>% 
  ggplot(aes(x=year, y=total_pop, color=continent)) + 
  geom_area(alpha=.9) +
  theme_bw() +
  scale_fill_manual(values=c("#003964","#00bbce","#A7C539","#D33E2C","#00665a"))
###

# Exercise #7: Line chart of GDP per capita for USA, China, France, Germany
# using theme bw, line width 2 and palette 
# "#003964","#00bbce","#A7C539","#D33E2C"

# Solution:
gpmind %>% 
  filter(country %in% c('United States', 'China', 'France', 'Germany')) %>% 
  group_by(country, year) %>% 
  summarize(avg_gdp = sum(gdpPercap*pop) / sum(pop)) %>% 
  ggplot(aes(x=year, y=avg_gdp, color=country)) +
  geom_line(linewidth=2) +
  theme_bw() +
  scale_fill_manual(values=c("#003964","#00bbce","#A7C539","#D33E2C"))
###

# Exercise #8: Change the order to Germany, France, China, United States.
# For this make the variable to be factor and change levels.

# Solution:
gpmind %>% 
  filter(country %in% c('United States', 'China', 'France', 'Germany')) %>% 
  group_by(country, year) %>% 
  summarize(avg_gdp = sum(gdpPercap*pop) / sum(pop)) %>% 
  mutate(country = factor(country, levels=c('Germany', 'France', 'China', 'United States'))) %>% 
  ggplot(aes(x=year, y=avg_gdp, color=country)) +
  geom_line(linewidth=2) +
  theme_bw() +
  scale_fill_manual(values=c("#003964","#00bbce","#A7C539","#D33E2C"))
  
###

# Exercise #9: Faceted line chart of life expectancy over time by continent
# with theme bw and line width 1.5.
# Use + facet_wrap(~continent) after the ggplot

# Solution:
gpmind %>% 
  group_by(continent, year) %>% 
  summarize(avg_le = sum(lifeExp*pop) / sum(pop)) %>% 
  ggplot(aes(x=year, y=avg_le, color=continent)) +
  geom_line(linewidth=1.5) +
  theme_bw() +
  facet_wrap(~continent)
###

# Exercise #10: Line chart of median and weighted average GDP per capita 
# by continent over time with theme bw and line width 1.5.
# Make them differintiate by the type of the line.

# Solution:
gpmind %>% 
  group_by(continent, year) %>% 
  summarize(median_gdp = median(gdpPercap), avg_gdp = sum(gdpPercap*pop) / sum(pop)) %>% 
  pivot_longer(cols=c(median_gdp, avg_gdp), names_to='type', values_to='gdp') %>%
  ggplot(aes(x=year, y=gdp, color=continent, linetype=type)) +
  geom_line(linewidth=1.5) +
  theme_bw()
###

# Exercise #11: Line chart of the ratio of GDP per capita 
# between Germany and China with theme bw and line width 1.5

# Solution:
gpmind %>% 
  filter(country %in% c('Germany', 'China')) %>%
  group_by(country, year) %>% 
  summarize(avg_gdp = sum(gdpPercap*pop) / sum(pop)) %>%
  group_by(year) %>% 
  summarize(ratio = avg_gdp[country == 'Germany'] / avg_gdp[country == 'China']) %>%
  ggplot(aes(x=year, y=ratio)) +
  geom_line(linewidth=1.5) +
  theme_bw()

gpmind %>% filter(country %in% c("Germany", "China")) %>%
  select(country, year, gdpPercap) %>%
  pivot_wider(names_from=country, values_from=gdpPercap)

###

# Exercise #12: Line chart of the average annual GDP growth rates 
# in % over time for Australia. To access previous value use function
# lag(), line width 1.5 and theme bw.

# Solution:
gpmind %>% filter(country == "Australia") %>%
  arrange(year) %>% mutate(gdp = gdpPercap*pop) %>%
  mutate(growth = (gdp - lag(gdp)) / lag(gdp)) %>%
  ggplot(aes(x=year, y=growth)) +
  geom_line(linewidth = 1.5) + 
  theme_bw()
###


# ---------------------------------------------------------------------------


rm(list = ls())
library(tidyverse)
library(gapminder)
library(showtext)

# Load a custom font
font_add_google("Pacifico", "cute_font")
showtext_auto()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

gpmind <- gapminder

# Exercise #1: Build a line plot of average GDP per capita in continents.
# Save into ggp.

# Solution:
ggp <- gpmind %>% 
  group_by(continent, year) %>% 
  summarize(avg_gdp = sum(pop*gdpPercap) / sum(pop)) %>% 
  ggplot(aes(x=year, y=avg_gdp, color=continent)) +
  geom_line()
###

# Exercise #2: Add title, x and y axes, caption to be meaningful

# Solution:
ggp <- ggp +
  labs(title='Average GDP per capita over time by Cintinent',
       x='Year',
       y='Avg GDP',
       caption='Source: Gapminder')
###

# Exercise #3: Add a theme_minimal to ggp. Then try to write 
# a parameter base_family equal to the "cute_font".
# Resave ggp with this new theme_minimal.

# Solution:
ggp <- ggp + theme_minimal(base_family='cute_font')
###

# Exercise #4: Let's make a palette_cute. Add to the vector following colors:
# "#FF69B4","#C71585","#8B008B","#FFA07A","#DC143C". 
# Then add this palette to the ggp. 

# Solution:
palette_cute <- c("#FF69B4","#C71585","#8B008B","#FFA07A","#DC143C")

ggp <- ggp + scale_color_manual(values=palette_cute)
###

# Exercise #5: Change the plot background color to light pink "#FFF0F5"
# Solution:
ggp <- ggp +
  theme(plot.background = element_rect(fill = "#FFF0F5"))
###

# Exercise #6: Change the panel background color to soft peach "#FFE4E1"
# Solution:
ggp <- ggp + 
  theme(panel.background=element_rect(fill="#FFE4E1"))
###

# Exercise #7: Modify the legend background and key colors to baby pink "#FFD1DC"

# Solution:
ggp <- ggp + 
  theme(legend.backgroung=element_rect(fill='#FFD1DC'),
        legend.key=element_rect(fill='#FFD1DC'))
###

# Exercise #8: Customize legend text size to 14 and color "#8B008B"
# Solution:
ggp <- ggp +
  theme(legend.text=element_text(size=14, color='#8B008B'))
###

# Exercise #9: Style the plot title with bold crimson text "#DC143C"
# Solution:
ggp <- ggp +
  theme(plot.title=element_text(face='bold', color='#DC143C'))
###

# Exercise #10: Customize the axis titles with medium violet-red color "#C71585"

# Solution:
ggp <- ggp +
  theme(axis.title.x = element_text(color = "#C71585", size = 14),
        axis.title.y = element_text(color = "#C71585", size = 14))
###

# Exercise #11: Change the axis text color to deep pink "#FF1493" and 
# adjust size to 14

# Solution:
ggp <- ggp + theme(
  axis.text = element_text(color = "#FF1493", size = 14)
)
###

# Exercise #12: Apply the final custom theme function

# Solution:
custom_theme <- function() {
  theme_minimal(base_family = "cute_font") +
    theme(
      plot.background = element_rect(fill = "#FFF0F5", color = NA),
      panel.background = element_rect(fill = "#FFE4E1", color = NA),
      legend.background = element_rect(fill = "#FFD1DC", color = NA),
      legend.key = element_rect(fill = "#FFD1DC"),
      legend.text = element_text(size = 14, color = "#8B008B"),
      plot.title = element_text(size = 20, face = "bold", color = "#DC143C"),
      axis.title = element_text(size = 16, color = "#C71585"),
      axis.text = element_text(size = 14, color = "#FF1493")  # Deep pink
    )
}

ggp + custom_theme()
###

# Exercise #13: Build a KSE palette from scratch with 4 colors :)

# Solution: 
palette_kse <- c('#E4FF33', '#33edff', '#3334ff', '#ffffff', '#000000')
###


# ---------------------------------------------------------------------------

sptf <- read_csv('spotify.csv')

colnames(sptf)

min(sptf$popularity)
max(sptf$popularity)

summary(sptf)
# Exercise #1: Create a scatter plot of danceability vs energy
sptf %>% 
  ggplot(aes(x=danceability, y=energy)) +
  geom_point() 
  

  
# duration
# --------
sptf %>% 
  transmute(duration_min = duration_ms/60000) %>% 
  summary()

# 50% of songs are within 3-4 min, but the duration can be up to 87 min
  
# --------
high_pop_sptf <- sptf %>% 
  filter(popularity >= 95) %>%
  transmute(duration_min = round(duration_ms / 60000, 2), track_genre) 

high_pop_sptf %>% 
  summary()

# 
high_pop_sptf %>% 
  ggplot(aes(x=duration_min)) +
  geom_density() +
  geom_vline(xintercept=mean(high_pop_sptf$duration_min), color='red') +
  theme_minimal() +
  labs(title='Duration of popular songs (95+)',
       x='Duration, min',
       y='Density') 
# songs with high popularity last for 2.5 - 4.5 min
# on average they are 3min long, but the actual mode is near 2-40min


high_pop_sptf %>% 
  ggplot(aes(x=duration_min, fill=track_genre)) +
  geom_density(alpha=.5) +
  theme_minimal() +
  labs(title='Duration of popular songs (95+)',
       x='Duration, min',
       y='Density') 
  

# songs with high popularity ratings are from ... genres, but most of them are
# dance, pop, latin, reggaton ones (other are <2 occurences)

sptf %>% 
  filter(track_genre %in% c('dance', 'pop', 'latin', 'reggae'), duration_ms/60000 <= 8) %>%
  ggplot(aes(x=duration_ms/60000, fill=track_genre)) +
  geom_density(alpha=.5) +
  theme_minimal() +
  labs(title='Duration of popular genres',
       x='Duration, min',
       y='Density')



# liveness + duration


%/%        #  ціле число
  %%          #  остача
  isTRUE      # character
is.na()      # is data NA
%in%         !a %in% c()      #   is in / not in
all.equal(a+b, c)     #  is a+b=c T
f(x) |>  g(f) / f(x) %>%  g(f)  # use func1 res in func2 | Ctrl Shift M
typeof()
class()
as.numeric()
ceiling()       floor()     round(x, digits=0)     #   round to top/bottom

# basic
paste('txt', 4, 'f')  paste0()    #   => 'txt 4 f' / with no space
strsplit(v, split='')
gsub(pattern, replacement, x, ignore.case=F)
str_detect(str, pattern, negate=F)
# ----------------------------
lapply(txt, tolower)      #   all lowercase
lapply(v, min)   lapply(v_s, function(v) {..v..})   #   min(v)  | if many v_s => get separ mins     
sapply(v, min)     sapply(v, func_name)    # sapply(var1, func, var2=…, var3=…)    #  min(v)  | if many v_s => get vec of mins    
vapply(v, func, numeric(3) expect 3 numb)       #  specify expected output | if F => get Error
tapply(vec, group_col_name / list(group1, gr2), func)            #    apply func to vec + group by col
outer(x, y, FUN='*')      # apply FUN to all pairs of xy
# ------------------------------------------------------------------------------------
names(v) <- v2    rownames(a) <- c()     colnames()  *colnames(df)[colnames(df) == ''] <- new_name
nrow()      ncol()
rowSums()        colSums()
rowMeans()     colMeans()
# ----------------------------
min/max      range()       length()
sin()   cos()
sqrt()    abs()    prod()
# ----------------------------
mean(data, na.rm=TRUE, trim=n)  #  remove na | n% of data = outlier  
median()     var()    IQR()    quantile(vec, 0.25)     sd(val)   # standard deviation
cor(col1, col2)    #  count correlation


# tidyverse
tibble(col1 = data1, col2=data2,..) # ~df  | show 1st 10 rows 
# dplyr ----------------------------
%>%    # Ctrl Shift m
  df  %>%  select(col1, col2,..)      select(-col, -col)
pull(col)    # take vec, no col_name
slice(row2:row6)
slice_min/max()      #  if diff type of data
slice_sample(data, n=, replace=T)    #   random select n rows
slice_head()/tail()   
separate(df, column, c(col1, col2,..), sep='')   # separate col_vals into new cols by ''
# ----------------------------
filter(cond1, cond2)
distinct()
n_distinct()      # num of distinct vals
rename(new_col = old_col)
cat('abc', x, …)   #  sum string
# ----------------------------
mutate(new_col/old_col = transform_of_old_col)
transmute(col1, new_col=…col2, col3,..)   #  select_mutate cols in 1 func
# ----------------------------
n()      #  count num of rows
na.omit()
arrange(desc(col))     arrange(col1, desc(col2))
# ----------------------------
count(col / col1, col2 / col1, wt=col2, sort=F)  # =group_by+summarize  |  count each val/paired val occurrence in col / sum of col2 
group_by(col1, col2,..)
summarize(.by=col, new_col=func(col), new_col2=…)   #  with mean() / sum() / sd()/.. | summary() but we choose what to count
summarise(across(cols, func))   #   ~sapply in summarise()
# ----------------------------
df1 %>% left_join(df2, suffix=c())   , by=c('col1' = 'col2')     right_join / inner_join / full_join
# ----------------------------
pivot_wider(names_from=col1, values_from=col2)   #   make cols with names=col1, vals = col2
pivot_longer(cols = c(col1, col2) / everything() , names_to= 'col_for_names', values_to= 'col_for_vals') # pivot cols -> 2 cols name+val
# ----------------------------
map_dbl(vec, ~built_in_func/ ~{own_func})   #  apply for every val func + return vec of doubles
_lgl

# plot
plot(as.factor(df$col), main='title')
hist(data, breaks=)
par(mfrow = c(x, y))      #  do plots with x;y grid


# ggplot ------------------------------------------------------------------------------------
ggplot(data=, rbind(x, y), aes(x=, y=, color=dataset, fill=col, size=) +  #aes used for all following plots
         geom_point()        scatter plot       geom_jitter(width=, alpha=)       #розкиданий scatter
   geom_line()         geom_area()
   geom_boxplot()
   geom_histogram(data, bins=30, binwidth=, position='', fill='color', color='margin_color')
   geom_bar() # need only x | y=count(x)
   geom.col() # need x, y
   geom_density()
   geom_segment()
   geom_smooth(se=T)      # +areas of SE
   geom_text(label=col)
       
ggplot(aes(sample=data)) +
 geom_qq() +       # QQ - plot | default sample=norm_distrib
 stat_qq_line()     # line

stat_function(fun=dnorm) (args=list(a=, b=,..)   # plot function fun +- vars a, b
     # ----------------------------
     theme_minimal()    # +wight background
     theme(legend.position='none')
     facet_wrap(~col)     # divide in subplots by col
     scale_fill_manual(values=c('grey', 'red'), limits=c(), breaks=c(), name='title_leg/ax', labels=c())
     scale_color_manual(values=c())
     # ----------------------------
     geom_text(data=, vjust=, size=)
     labs(title=, subtitle=, caption='',  x=, y=, fill=)
     # --------------------
     coord_flip()     #   x;y -> y;x
     # 
     library(ggthemes)
     theme_bw()       #  margins for plot
     theme_classic()  # no margins, just axes
     theme_void()      #  nothing, just lines_legend
     theme_minimal()
     theme_wsg   _economist      _excel_new   _tufte
     theme(panel.grid = element_blank() # -prev grid,
           panel.grid.major.y = element_line(color='', linewidth=, linetype=, size=),
           panel.background/border= element_rect(fill=''),
           axis.ticks=element_line(),
           axis.title = element_text(),
           axis.text.x = element_text(),
           plot.title = element_text()
           plot_caption=element_text(),
           plot.background = element_rect(fill='color'),
           plot.margin
           legend.position = 'bottom/none',
     )
     
     custom_theme <- theme_minimal(...) + theme(...)
     plot + custom_theme



