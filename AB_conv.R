library(tidyverse)
set.seed(123321)

split_user <- read_csv('https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/aa_test/aa_test.csv')

users <- split_user %>% tibble()

# split csv data into columns
users <- users %>% 
  separate(1, c('col', 'id', 'expVariant', 'version', 'purchase'), sep = ';') %>% 
  select(2:5) %>% 
  mutate(expVariant = as.numeric(expVariant),
         purchase = as.numeric(purchase))


# hypotheses ---------------------------------
# h0: conv_A = conv_B
# h1: conv_A != conv_B

alpha <- 0.05

# calculate fpr for 1000 samples of 1000 rows for group_A and 1000 rows of group_B
fpr_check <- function(data) {
  # vector of p-values
  p_vals <- numeric(1000)
  
  # find p-value for each sample
  for (i in 1:1000) {
    sample_A <- slice_sample(.data=data[data$expVariant == 0,], n=1000, replace=F)
    sample_B <- slice_sample(.data=data[data$expVariant == 1,], n=1000, replace=F)
    sample <-  rbind(sample_A, sample_B)
    
    t_test <- t.test(purchase ~ expVariant, sample, alternative='two.sided')

    p_vals[i] <- t_test$p.value <= alpha
  }
  
  # calculate fpr
  return(mean(p_vals))
}

fpr_check(users)       # => 0.943 > 0.05

# Results:
# fpr rate is much higher than the alpha, meaning that there are multiple
# Type I Errors, or rejected experiments that should not be rejected. So, our
# splitting algorithm is not working properly



# show conversion for each version and experiment group ---------------------
convs <- users %>%
  group_by(version, expVariant) %>%
  summarize(convertion = sum(purchase) / n())

# Results:
# in version v2.8.0 the conversion rate is extremely different between 
# experiment groups, so it might execute wrongly


# exclude v2.8.0 from users
users <- users[users$version != 'v2.8.0', ]

fpr_check(users)      # => 0.046 < 0.05

# Results:
# now fpr is lower than alpha, so we can say that our splitting algorithm properly
# divide customers into two similar groups











