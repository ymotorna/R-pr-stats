library(tidyverse)

crime_10_19 <- read_csv('cleaned_data_1.csv')
crime_24 <- read_csv('cleaned_data_2.csv')
crime_20_23 <- read_csv('cleaned_data_4.csv')

colnames(crime_10_19)
alpha = 0.05

# for 2010-2019 we want to see if on average Females committed more crimes per year
# than Males

# h0: mu_m < mu_f   <=>  mu_f - mu_m > 0
# h1: mu_m >= mu_f  <=>  mu_f - mu_m <= 0

# group data by year, gender, and count number of arrests
sex_10_19 <- df1 %>% 
  transmute(arrest_year = substr(as.character(Arrest.Date), 1, 4), Sex.Code) %>% 
  group_by(arrest_year, Sex.Code) %>% 
  summarize(num = n())


# two-sampled t-test
avg_sex <- t.test(num ~ Sex.Code, sex_10_19, alternative='less')

# p-value
avg_sex$p.value <= alpha   # True => probability to get mu_f - mu_m <= 0 
# when ho is True is less than significance lvl => reject h0
# critical value
crit_val <- qt(1-alpha, df=avg_sex$parameter)

abs(avg_sex$statistic) > crit_val   # True => unlikely to get ho => reject h0

# confidence interval 
avg_sex$conf.int[2] > 0   # False => in 95% of cases mu_f - mu_m <= 0 => reject h0



# visualization

mu_f = avg_sex$estimate[1]
mu_m = avg_sex$estimate[2]


sex_10_19 %>% 
  ggplot(aes(x=arrest_year, y=num, fill=Sex.Code)) +
  geom_col(alpha=0.5, position='dodge') +  
  
  geom_hline(aes(yintercept=mu_m, color='Male mean')) +
  geom_hline(aes(yintercept=mu_f, color='Female mean')) +
  geom_hline(aes(yintercept=mu_f+abs(avg_sex$conf.int[2]), color='CI', ), linetype='dashed', linewidth=.5) +
  scale_color_manual(values=c('Female mean' = 'red',    
                              'Male mean' = 'blue',
                              'CI' = 'black')) +    
  
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +  # GPT prompt: how to adjust legend in ggplot
  labs(title='Arrest 2010-2019 Distribution by Sex', fill='sex') +    # GPT prompt: how to adjust legend name in ggplot
  theme_minimal()









# --------------------------------------------------------------------

# for 2020-2023 we want to see whether the situation has changed

# h0: mu_m < mu_f   <=>  mu_f - mu_m > 0
# h1: mu_m >= mu_f  <=>  mu_f - mu_m <= 0

alpha = 0.05

# group data by year, gender, and count number of arrests
sex_20_23 <- crime_20_23 %>% 
  filter(YEAR >= 2020 & YEAR <= 2023) %>%
  group_by(YEAR, SEX) %>% 
  summarize(num = n())


# two-sampled t-test
avg_sex_2 <- t.test(num ~ SEX, sex_20_23, alternative='less')

# p-value
avg_sex_2$p.value <= alpha   # True => reject h0

# critical value
crit_val_2 <- qt(1-alpha, df=avg_sex_2$parameter)

abs(avg_sex_2$statistic) > crit_val_2   # True => reject h0

# confidence interval 
avg_sex_2$conf.int[2] > 0   # False => reject h0



# vizualization

mu_f_2 = avg_sex_2$estimate[1]
mu_m_2 = avg_sex_2$estimate[2]


sex_20_23 %>% 
  ggplot(aes(x=YEAR, y=num, fill=SEX)) +
  geom_col(alpha=0.5, position='dodge') +
  
  geom_hline(aes(yintercept=mu_m_2, color='Male mean')) +
  geom_hline(aes(yintercept=mu_f_2, color='Female mean')) +
  geom_hline(aes(yintercept=mu_f_2+abs(avg_sex_2$conf.int[2]), color='CI', ), linetype='dashed', linewidth=.5) +
  scale_color_manual(values=c('Female mean' = 'red',    
                              'Male mean' = 'blue',
                              'CI' = 'black')) +     
  
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +  
  labs(title='Arrest 2020-2023 Distribution by Sex', fill='sex') +
  theme_minimal()




# --------------------------------------------------------------------
df2 <-  read.csv("NYPD_Arrest_Data__Year_to_Date__20250221.csv")
dff2 <- read.csv("NYPD_Arrest_Data__Year_to_Date__20250221.csv")

str(df2)

summary(df2)

# First we remove duplicates
df2 <- df2 %>%
  distinct()

# Here we convert the date-to-date format so that we can work with data more c
# Delete missing data in important columns for us

df2 <- df2 %>%
  filter(!is.na(ARREST_DATE) & !is.na(PERP_SEX))
         
# Convert columns to factors

df2$PERP_SEX <- factor(df2$PERP_SEX)
df2$PERP_RACE <- factor(df2$PERP_RACE)
                    
# Deal with anomalities for age
 
df2$AGE_GROUP <- factor(df2$AGE_GROUP, levels = c("18-24", "25-44", "45-64", "65+"))
                          
summary(df2)
str(df2)

# for 2024 we want to check if there is 50%+ probability to have a criminal of age
# 18-24 y.o.

# h0: p >= 0.5
# h1: p < 0.5

successes <-df2 %>% 
  filter(AGE_GROUP == '18-24') %>% 
  summarize(num = n()) %>% 
  pull()

n = nrow(df2)
prob = 0.5

# binomial test
binom_test <- binom.test(successes, n, prob, alternative='less')

# p-value
binom_test$p.value <= alpha   # True => reject h0

# critical value
crit_val_3 <- qbinom(alpha, size=n, prob=prob)

binom_test$statistic < crit_val_3   # True => reject h0

# confidence interval
binom_test$conf.int[2] > prob                  # False => reject h0
binom_test$conf.int[2] > binom_test$estimate   # True => reject h0


# visualization
tibble(
  x = 0:n,
  probability = dbinom(x, n, prob),
  is_crit_val = x <= qbinom(alpha, n, prob=prob)) %>%
  
  ggplot(aes(x=x, y=probability, fill=is_crit_val)) +
  geom_col() +
  scale_fill_manual(values=c('lightblue', 'red')) +
  geom_vline(xintercept=successes, color='red') +
  xlim(128500, 131500) +
  theme(legend.position='none') +
  labs(title='Probability of 18-24 y.o. being criminals in half of events',
       x='Number of successes') 
    

# ---------------------------------------------------------------------

