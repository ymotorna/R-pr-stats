library(tidyverse)

install.packages("remotes")
remotes::install_github("rstudio-education/dsbox")
library(dsbox)

gss16
colnames(gss16)
unique(gss16$harass5)
unique(gss16$polviews)

alpha <- 0.05

# check for statistically significant differences in answers
gss16 %>% 
  group_by(harass5) %>%
  summarise(frequency = n()) %>% 
  pull(frequency) %>%
  chisq.test() 


# Results:
# x^2 test has showed that the probabilities of getting one of the answer differ 
# significantly (p-value < 0.05, extremely low). From the frequency table we can see that
# the most common answer is 'No' if there is some at all. We can check for differences
# after removing NA values:

gss16 %>% 
  filter(!is.na(harass5)) %>%
  group_by(harass5) %>%
  summarise(frequency = n()) %>% 
  pull(frequency) %>%
  chisq.test()


# Results:
# Although statistic value has become less, p-value is still extremely low, 
# meaning the differences are statistically significant. The 'No' answer occurs
# too often in comparison to other. We can compare two other answers with visibly
# less tangible difference:

gss16 %>% 
  filter(harass5 %in% c('Does not apply (i do not have a job/superior/co-worker)', 'Yes')) %>%
  group_by(harass5) %>%
  summarise(frequency = n()) %>% 
  pull(frequency) %>%
  chisq.test() 

# Results:
# Again, we can  reject the hypothesis of same probabilities of getting one of the answers.
# Although the test has become less reliable due to the small number of observations,
# p-value is too low to have strong uncertainty about the results. 

# All in all, there is a mode/most common answer in the dataset that represents 
# tendencies of harassment experience at work. But people can understand harassment 
# differently, or not say truth.
# We can make a contingency table to check for independence of harassment and
# political views, as there may be various interpretations of behavior based on political ideas


gss16 %>% 
  filter(!is.na(harass5) & !is.na(polviews)) %>%
  # combine values to have 10+ observations in each cell
  mutate(polviews = case_when(
    polviews %in% c('Extremely liberal', 'Liberal') ~ 'liberal',
    polviews %in% c('Slightly liberal', 'Moderate', 'Slghtly conservative') ~ 'moderate',
    polviews %in% c('Conservative', 'Extrmly conservative') ~ 'conservative'
  )) %>% 
  group_by(harass5, polviews) %>%
  summarise(frequency = n()) %>%
  pivot_wider(names_from=polviews, values_from=frequency) %>%
  tibble() %>%
  select(liberal, moderate, conservative) %>%
  chisq.test()

# Results:
# the statistic is low, and a p-value is ~81%, meaning there is no dependency between 
# variables, so there is no evidence to say that political views affect harassment
# interpretation




