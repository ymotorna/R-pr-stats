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


# Exercise #1: Load "suicide.csv" into df. Check the summary.
# Remake df in a single pipe by making sex, country, age, 
# and generation to be factors, and renaming suicides/100k pop into 
# suic_100k, gdp_for_year ($) into gdp, gdp_per_capita ($) into gdp_cap.
# Write a comment about the dataset and it's features.
# Tip: answer what is a single observation in this dataset.

# Solution:
df <- read_csv('suicide.csv')

summary(df)

df <- df %>% 
  mutate(sex = as.factor(sex),
         country = as.factor(country),
         age = as.factor(age),
         generation = as.factor(generation)) %>% 
  rename(suic_100k = 'suicides/100k pop',
         gdp = 'gdp_for_year ($)',
         gdp_cap = 'gdp_per_capita ($)')

# the dataset provides information about number of suicides commited in different countries
# alongside the information about age group, sex, year of incident and statistics over country
# (gdp, suicides per 100k people, population)

###

# Exercise #2: For df build a scatterplot:
# 1) GDP per capita on x, suicides per 100k on Y, but only for Ukraine
# 2) GDP per capita on x and suicides per 100k on Y for the following countries:
# "Ukraine","Poland","Bulgaria","Czech Republic","Estonia","Lithuania","Slovenia".
# Also take age only 25-34 and year more or equal to 2000.
# Colour points by country. Add theme_kse (that you can find in the lecture) 
# and color using the kse palette

# Solution:

kse_palette <- c("#003964","#00bbce","#A7C539","#F15B43",
             "#D33E2C","#E4E541","#00665a")

theme_kse <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white",color="white"),
    axis.line = element_blank(),
    text = element_text(color = kse_palette[1], size = 18, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(angle = 0,vjust = 0.5,color=kse_palette[1],
                               margin = margin(t=10)),
    axis.text.y = element_text(color=kse_palette[1],
                               margin = margin(r=10)),
    panel.grid.major.y = element_line(color=kse_palette[2],linewidth=0.25),
    legend.title=element_blank(),
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.title = element_text(margin = margin(t=5, r=5, b=10, l=5)),
    plot.caption = element_text(hjust=0,face='italic',
                                margin = margin(t=5, r=5, b=3, l=5))
  )

df %>% 
  filter(country == 'Ukraine',
         age == '25-34 years',
         year >= 2000) %>%
  ggplot(aes(x=suic_100k, y=gdp_cap)) +
  geom_point() +
  labs(title = 'Suicides per 100k vs GDP per Capita in Ukraine',
       x = 'Suicides per 100k',
       y = 'GDP per Capita') +
  scale_fill_manual(values=kse_palette) +
  theme_kse


df %>% 
  filter(country %in% c('Ukraine', 'Poland', 'Bulgaria', 'Czech Republic', 'Estonia', 'Lithuania', 'Slovenia'),
         age == '25-34 years',
         year >= 2000) %>%
  ggplot(aes(x=suic_100k, y=gdp_cap, color=country)) +
  geom_point(size=2.5) +
  labs(title = 'Suicides per 100k vs GDP per Capita',
       x = 'Suicides per 100k',
       y = 'GDP per Capita') +
  scale_fill_manual(values = kse_palette) +
  theme_kse
###

# Exercise #3-4: 
# 1) Build a histogram for suicides per 100k
# 2) Build a histogram, but filter out those observations with suicides 
# per 100k less than 60
# 3) Additionally, make colours by gender, also make the position to be "fill".
# Add a theme_minimal and palette from the theme_cute from class. 
# 4) Write a comment about your findings.

# Solution:
palette_cute <- c("#FF69B4","#C71585","#8B008B","#FFA07A","#DC143C")

df %>% 
  ggplot(aes(x=suic_100k, fill=sex)) +
  geom_histogram(position='fill', color='grey') +
  theme_minimal() +
  scale_fill_manual(values=palette_cute)
  
df %>% 
  filter(suic_100k >= 60) %>%
  ggplot(aes(x=suic_100k, fill=sex)) +
  geom_histogram(position='fill', color='grey') +
  theme_minimal() +
  scale_fill_manual(values=palette_cute)  

# the plots compare the ratio of suicides per 100k of population between males and females.
# in most cases men have higher rates except for the cases where suic_100k is 
# less then 16. And after filtering  it to be higher than 60, the percents of
# males' suisides are 90% and more.
# there is a tendency thet the higher the rate of suicides per 100k, the higher the
# ratio of males to females
  
###

# Exercise #5-6: Create a variable suic_rate which is factor that means 
# low (less than 2), medium (2 to 7) or large (more than 7) 
# suicide rate per 100k.
# 1) Make a barplot for gender
# 2) Build a barplot for gender and colour it by 
# the factor of suicide rate and make position to be "dodge"
# Make colors and theme to be cute.
# 3) Write a comment about your findings.

# Solution:
library(showtext)
font_add_google("Pacifico", "cute_font")
showtext_auto()

theme_cute <- theme_minimal(base_family = "cute_font") +
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


df <- df %>% 
  mutate(suic_rate = factor(case_when(
    suic_100k < 2 ~ 'low',
    suic_100k > 7 ~'large',
    T ~ 'medium'
  )))

df %>% 
  ggplot(aes(x=sex)) +
  geom_bar(fill=palette_cute[1:2]) +
  theme_cute


df %>% 
  ggplot(aes(x=sex, colour = suic_rate)) +
  geom_bar(fill=palette_cute[1:6], position='dodge') +
  theme_cute

# from the first bar chart we can see that the number of males and females are 
# equal or almost equal.
# the division of suic_rate into 3 categories shows that within males the majority 
# of statistic is large, while within females - low

###

# Exercise #7-8: 
# Create a line chart for Ukraine and Republic of Korea (on the same graph)
# about amount of suicides per 100k. Filter only for those who are 
# age 25-34 and Generation X. Make two different graphs by gender in the same 
# figure. For this you can add to the pipe "facet_grid(sex~.)" in the end.
# Make linewidth to be 2. Then use a theme_minimal, but adapt it to have 
# all the texts be of the size 18 and color from kse_palette, first color.
# Add meaningful title and x,y titles if necessary, also add caption.

# Solution:
df %>% 
  filter(country %in% c('Ukraine', 'Republic of Korea'),
         age == '25-34 years',
         generation == 'Generation X') %>%
  group_by(country, sex, year) %>% 
  summarise(avg_suic = sum(suicides_no)/sum(population)*100000) %>%
  ggplot(aes(x=year, y=avg_suic, color=country)) +
  geom_line(linewidth=2) +
  theme_minimal() +
  theme(
    text = element_text(color=kse_palette[1], size=18)
  ) +
  labs(title = 'Suicides per 100k',
       x = 'Year',
       y = 'Suicides per 100k',
       caption = 'Source: suicide.csv') + 
  facet_grid(~sex)
###

# Exercise #9-10: Copy the graph from 1.png.
# Colours are lightblue and violet.
# For the second scale (right one) you may need the library "scales"
# and the following piece of code:
# scale_y_continuous(sec.axis = sec_axis(~ . /5000, name = "Amount of Suicides, mln")) +
# Write a short comment why population is 2B instead of 6-7B. 
# Tip: to change right scale, you have to work with suffix .right. 
# For example: axis.text.y.right

# Solution:
library(scales)

pop_suic <- df %>% 
  group_by(year) %>% 
  summarise(pop = sum(population)/1000000, 
            suic = sum(suicides_no)/1000000) 

pop_suic %>% 
  ggplot() +
  geom_line(aes(x=year, y=pop), color='lightblue', linewidth=2) +
  
  scale_y_continuous(limits = c(500, 2000),
                     expand = c(0, 0),
                     sec.axis = sec_axis(~ . /5000, name = "Amount of Suicides, mln")) +
  geom_line(aes(x=year, y=suic*5000), color='violet', linewidth=2) +
  
  labs(title = "Population (blue) and amount of suicides (violet) \n
       in the world, millions of people",
       y = 'Population, min',
       x = 'Year',
       caption = 'Source: World Bank') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
      color = "black",
      linewidth = 0.5,
      linetype = "dotted"),
    text = element_text(size = 10, face='bold'),
    plot.title = element_text(size=17, face='italic'),
    
    axis.text.y.right = element_text(color = "violet"),
    axis.title.y.right = element_text(color = "violet", angle=90),
    axis.text.y = element_text(color = "lightblue"),
    axis.title.y = element_text(color = "lightblue"),
    
    axis.line.x = element_line(color = "darkgrey"),
  )

# the population is 2B instead of 6-7B because the dataset contains less countries
length(unique(df$country))   # => 101, in reality 195

###

# Exercise #11-12: For the following countries:
# "Ukraine","Poland","Bulgaria","Czech Republic","Estonia","Lithuania","Slovenia"
# Make a line chart for the whole country of suicides per 100k.
# Make a theme to be kse, so as the palette and linewidth 2.

# Solution:
df %>% 
  filter(country %in% c("Ukraine","Poland","Bulgaria","Czech Republic","Estonia","Lithuania","Slovenia")) %>%
  group_by(country, year) %>%
  summarize(suic_100k = sum(suicides_no)/sum(population)*100000) %>%
  ggplot(aes(x=year, y=suic_100k, color=country)) +
  geom_line(linewidth=2) +
  theme_kse +
  scale_color_manual(values=kse_palette)
###


savehistory("HW7_8_Yelyzaveta_Motorna.Rhistory")
