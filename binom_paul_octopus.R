library(tidyverse)
library(pwr)

n = 14
q = 12
alpha = .05

check_paul_criterion <- function(n, k, alpha) {
  
  # generate binom_distrib
  outcomes <- dbinom(0:n, n, 0.5)          
  
  # critical vals = quantiles
  c_right <- qbinom(1 - alpha/2, n, 0.5)  
  c_left <- qbinom(alpha/2, n, 0.5) 
  
  # check H0
  reject <- (k > c_right) | (k < c_left)        
  
  # p-value              
  right_tail <- sum(outcomes[c_right:n])
  left_tail <- sum(outcomes[0:c_left])
  
  p_val <- 2 * min(right_tail, left_tail)
  
  # results
  plot_data <- tibble(
    x = 0:n,
    prob = outcomes,
    is_critic_val = x > c_right | x < c_left
    )
  
  PaulCheckResults <- tibble(
    is_rejected = reject, 
    p_value = p_val)
  
  return(list(plot_data, PaulCheckResults))
}

results <- check_paul_criterion(n, q, alpha)
results[[2]]


# Results
# As Paul guessed 12 out of 14 matches, the hypothesis H0 that it has probability 
# to win = 0.5 is rejected giving common alpha = 0.05: 
  # 1) the number of successes is within critical values
  # 2) p-value is less than alpha

# For alpha = 0.01 the hypothesis is not rejected. Although thus the chance to 
# make a Type I Error is reduced, it is more likely to mistakenly approve H0.

# For the Paul's prediction test I'd rather use alpha = 0.05 as the costs of
# FPR and FNR are equal.


# plot
results[[1]] %>% 
  ggplot(aes(x=x, y=prob, fill=is_critic_val)) +
  geom_col() +
  scale_fill_manual(values = c('grey', 'red')) +
  geom_text(data = filter(results[[1]], is_critic_val),
            aes(label = round(prob, 3)), vjust = -0.5, size = 2) +
  theme_minimal() +
  labs(title = 'Octopus Paul Prediction test',
       subtitle = 'n = 14, k = 12, alpha = 0.05',
       x = 'Successes',
       y = 'Probability',
       fill = 'Critical Values')
            

# CI
p_hat <- q/n
z <- qnorm(1-alpha/2)
se <- sqrt(p_hat*(1-p_hat)/n)
ci <- p_hat + c(-1, 1)*z*se      # => [0.67, 1]


# Results
# in 95% of cases octopus's probability to guess a winner will lie 
# between 0.67 and 1. The H0 hypothesis is out of the range, so it can be rejected.



# check with binom.test
binom.test(12, 14, .5, alternative = 'two.sided')
