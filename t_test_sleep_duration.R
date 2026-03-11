library(tidyverse)

# formulate hypothesis 

# medical student are aware of necessity of sleeping enough hours per day, so
# the actual mean of sleep duration is greater than 6.25h
# h0 : mean = 375 min of sleep
# h1 : mean > 375 min of sleep

mu <-  375  
n <-  226
alpha <-  0.05
sd <- 70   

# for recommended sleep duration
mu_ideal <- 480 
sd_ideal <- 60   


# building visualization

# plot sleep duration distribution in ideal and real situations
set.seed(123)
sleep_duration <- tibble(
  ideal = rnorm(n, mu_ideal, sd_ideal),
  real = rnorm(n, mu, sd)
) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "value")

sleep_duration %>%
  ggplot(aes(x= value, fill=type)) +
  geom_density(aes(alpha=.5)) +
  labs(title='Sleep duration of 226 students') +
  geom_vline(xintercept=mu, color='blue', show.legend=T) +
  geom_vline(xintercept=mu_ideal, color='red', show.legend=T)

# Results:
# the study has shown that in comparison to recommended (ideal) distribution, 
# a chosen sample of students has less average time of sleep, and it varies more


          
          
# use t-test as:
  # we work with means
  # population's sd is unknown
set.seed(123)
sleep <- rnorm(n, mu, sd)

t_test <- t.test(sleep, mu=mu, alternative='greater')
names(t_test)

# critical regions
crit_t_val <- qt(1-alpha, df=n-1)

abs(t_test$statistic) > crit_t_val  # => F => accept h0


# p-value
t_test$p.value <= alpha   # => F => accept h0


# CI
mu >= t_test$conf.int  # => T F => mu within CI => accept h0


# power
mu_actual <- mu * 1.05

power_t_test <- function(n, alpha, mu_h0, mu_h1, sd) {
  crit <- qt(1-alpha, df=n-1)
  ncp = (mu_h1 - mu_h0) / (sd / sqrt(n))
  power <- 1 - pt(crit, df=n-1, ncp=ncp)
  return(power)
}


power_t_test(n, alpha, mu, mu_actual, sd) # => 99.1% 

# plot for power-size relation
set.seed(123)
power_size <- tibble(
  size = 1:1000,
  power = power_t_test(size, alpha, mu, 
                       mean(rnorm(size, mu_actual, sd)), 
                       sd=sd(rnorm(size, mu_actual, sd))))

power_size %>% 
  ggplot() +
  geom_line(aes(x=size, y=power)) +
  labs(title='Power-Size relation')


power_size[power_size$power >= .8, ]    # => n >= 80
power_size[power_size$power >= .9, ]    # => n >= 110

# Results:
# The power of the test is extremely high (99.1%), so there are low chances
# to make Type II error, or to mistakenly reject h1.
# The power >= 80% starting from n = 80, and >= 90% for n >= 110.
# Such significant rates for relatively small sample size is probably a result
# of choosing a one-tailed test (h1 : mu > 375).
# Power-size relation plot represents an increase in power value with the rise
# of sample size.



?t.test
?qt
?pt
