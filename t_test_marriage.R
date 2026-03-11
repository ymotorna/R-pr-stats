library(tidyverse)

marriage <- read_csv("https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/first_marriage/first_marriage.csv")
marriage_age <- marriage$age
summary(marriage)

# formulate hypothesis
# h0: mean = 23
# h1: mean > 23

n <- 5534
mu <- 23
mu_1 <- mean(marriage_age)    # => 23.44
alpha <- 0.05


# calculate statistics
t_test <- t.test(marriage_age, mu=mu, alternative='greater')

# critical values
crit_val <- qt(1-alpha, df=n-1)  
t_test$statistic > crit_val        # => T => reject h0

# p-vaue
t_test$p.value < alpha        # => T =>  reject h0

# confidence interval
mu > t_test$conf.int        # => F F => mu out of CI => reject h0


# power of the test
power_t_test <- function(n, mu_0, mu_1, sd, crit_val) {
  ncp <- (mu_1 - mu_0) / (sd / sqrt(n))
  power <- 1 - pt(crit_val, df=n-1, ncp=ncp)
  
  return(power)
}

power_t_test(n, mu, mu_1, sd(marriage_age), crit_val)   # => 99.9%



# visualization
plot_data <- tibble(
  sample = marriage_age,
  hypothesis = rnorm(n, mu, sd(marriage_age))
) %>% 
  pivot_longer(cols=everything(), names_to='Distribution', values_to='Age')


plot_data %>% 
  ggplot(aes(x=Age, fill=Distribution)) +
  geom_density(alpha=0.5) +
  
  geom_vline(aes(xintercept=mu, color='hypothesis mean')) +
  geom_vline(aes(xintercept=mu_1, color='sample mean')) +
  geom_vline(aes(xintercept=t_test$conf.int[1], color='CI start', ), linetype='dashed', linewidth=.5) +
  scale_color_manual(values=c('hypothesis mean' = 'red',    # GPT prompt: how to add lines' labels to the legend in ggplot
                              'sample mean' = 'blue',   
                              'CI start' = 'black')) +    
  
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +  # GPT prompt: how to adjust legend in ggplot
  labs(title='Age of first marriage distribution') +
  theme_minimal()



# Results:
# The question was whether the mean age of first marriage for US women is
# greater then 23 years. The H0 hypothesis stated that the mean = 23, and the
# H1 hypothesis stated that the mean > 23. 
# On the graph a distribution and mean of the hypothesis are represented in red 
# colors, and of the sample data in blue ones.
# The sample distribution is slightly skewed to the left, where age is equal to 21-22,
# so those are the most repetitive values in the data. However, since there are 
# marriages occurred during 30-40s, the actual mean is higher then the hypothetical. 
# A t-test has shown, that H0 could be rejected:
    # t_statistics > critical value, meaning that it is unlikely to get mean=23
    # p-value < alpha, so the probability to get sample mean > 23 when it is 23
                    # for the population is really low
    # 23 is not within CI, so in 95% of cases the mean is not 23. A dashed line 
                        # on the graph is a start of CI. We can see that hypothetical
                        # mean (red line) is before it, and the sample mean 
                        # (blue line) is after it.
# The power of the test is 99.9%, meaning that the probability to mistakenly 
# reject H0 is less than 1%, which gives us more confidence in the results.
# Concluding all, we have evidence that the mean age of first marriage for all US women
# from 2006 to 2010 is greater than 23 years.




?geom_vline
?linetype
