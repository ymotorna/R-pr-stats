library(tidyverse)
library(pwr)

n <- 30      # total num
p <- 0.5     # p in H0
q <- 20      # want to get q+ successes | підбир to get 5% of crit_vals

x_grid <- 0:n                  # all possib outcomes
probs <- dbinom(x_grid, n, p)  # p of success for each outcome

# critical region ---------------------------------------
crit_reg <- x_grid >= q    # if res >= 20 => strange that p=0.5


# plot ---------------------------------------
data_tbl <- tibble(
  x = x_grid,
  prob = probs,
  crit = crit_reg
)

data_tbl %>% 
  ggplot(aes(x = x, y = prob, fill = crit)) +
  geom_col() +
  scale_fill_manual(values = c("grey", "red")) +
  geom_text(data = filter(data_tbl, crit_reg),
            aes(label = round(prob, 2)), vjust = -0.5, size = 2) +
  theme_minimal() +
  labs(
    title = "Binomial Distribution",
    subtitle = "n = 30, p = 0.5, q = 19",
    x = "x",
    y = "Probability",
    fill = "x >= q"
  )

# p_val = sum of % in crit_region -------------------------
probs[crit_reg] %>% sum() %>% round(3)

# mathematical p_val ----------------------------------------
1 - pbinom(q - 1, n, p)    # 1 - (% to get < q) = % to get q+

# crit_val (a=0.05)  ---------------------------------------
qbinom(1 - 0.05, n, p) + 1  # 1 + (x_min after wich cum_prob >= 95%)



# find crit_val for bin_distib  ---------------------------------
make_binom_test <- function(n, mu = 0.5, alpha = 0.05) {
  return(qbinom(1 - alpha, n, mu) + 1)
}

make_binom_test(30, 0.5, 0.05)
make_binom_test(10, 0.2, 0.02)



# p-val  -----------------------------------------------------
pval_binom <- function(n, mu, q) {
  return(1 - pbinom(q-1, n, mu))
}

pval_binom(30, 0.5, 20)

 
# 2-sided p-val  ------------------------------------------------------
pval_2_side_binom <- function(n, q, p) {
  binom_h0 <- dbinom(0:n, n, p)              #розподіл
  diff <- abs(q - n*p)        # diff (success_val - mean)
  left_tail <- sum(binom_h0[0:(n*p - diff)])
  right_tail <- 1 - sum(binom_h0[0:n*p + diff - 1])
  
  return(min(right_tail, left_tail)*2)
}

pval_2_side_binom(30, 1, .2)


# 2-sided crit_val
crit_vals_2_side <- function(n, mu, alpha=0.05) {
  c2 <- qbinom(1-alpha / 2, n, mu) + 1     # 1 + x (=.95/2 quantile) => next after x val
  c1 <- qbinom(alpha / 2, n, mu) - 1          
  
  return(c(c1, c2))
}

crit_vals_2_side(30, 0.5, .05)


# power ------------------------------------------
get_start_power <- function(N, mu_h0, mu_h1, alpha=0.05) {
  crit_val = qbinom(1-alpha, N, mu_h0) + 1
  power <- 1 - pbinom(crit_val-1, N, mu_h1)
  
  return(power)
}

get_start_power(70, .5, .7)


# ---------------
test <- prop.test(x=20, n=30, p=.5, conf.level=.95, correct=F, alternative='greater')
test$p.value


# -------------------------
n <- 200         # sample num
q <- 70          # success from sample 
alpha <- 0.05
power <- .8
mu_h0 <- 0.3     # p in H0 = 0.3

tibble(
  x = 0:n,
  prob = dbinom(x, n, mu_h0),
  crit_x = x >= q
) %>% 
  ggplot(aes(x=x, y=prob, fill=crit_x)) +
  geom_col() +
  scale_fill_manual(values=c('grey', 'red')) +
  theme(legend.position='none')
  
pbinom(q-1, n, mu_h0, lower.tail = F)    # use p-val to reject H0 (p-val < alpha)
1 - pbinom(q-1, n, mu_h0)                # p_val

qbinom(1-alpha, n, mu_h0) + 1             # crit_val

# auto func
food_test <- binom.test(q, n, mu_h0, alternative='two.sided')


# CI
p_hat <- q/n
z <- qnorm(1-alpha/2)
se <- sqrt(p_hat*(1-p_hat)/n)
ci <- p_hat + c(-1, 1)*z*se

# standard norm distr ---------------------------------------------------------------------------
pnorm(2, mean=0, sd=1)

1 - pnorm(2)
pnorm(2, lower.tail=F)

pnorm(-1)*pnorm(1, lower.tail=F)    # prob to get < -1 or > 1
pnorm(-1) == pnorm(1, lower.tail=F)  # => T <= symmetric distrib
qnorm(0.975)

# +data table ---------------
set.seed(123)
norm_tbl <- tibble(
  x = seq(-4, 4, length.out=1000),
  norm_density = dnorm(x),
  norm_dist = pnorm(x),
  random_dist = rnorm(1000)
)

# plot ------------
norm_tbl %>% 
  ggplot() +
  geom_line(aes(x=x, y=norm_density)) +
  geom_histogram(aes(x=random_dist, y =..density..),    #..density.. - turn y 
                 bins = 30,                             #    from count -> density
                 fill='lightblue',
                 color='black',
                 alpha=.5) +
  geom_density(aes(x=random_dist), color='red') 

# plot cum_distrib ------
norm_tbl %>% 
  ggplot(aes(x=x, y=norm_dist))+
  geom_line()




# check sum of norm distr_s -------------------------------------------------------------------
x <- seq(-8, 8, length.out=10000)
n <- 10000

first_mean <- 3
first_var <- 4

second_mean <- 5
second_var <- 9

norm_check_tbl <- tibble(x=x,
       first=dnorm(x, mean=first_mean, sd=sqrt(first_var)),
       first_random = rnorm(n, mean=first_mean, sd=sqrt(first_var)),
       
       second=dnorm(x, mean=second_mean, sd=sqrt(second_var)),
       second_random = rnorm(n, mean=second_mean, sd=sqrt(second_var)),
       
       check_sum = dnorm(x, first_mean + second_mean, sqrt(first_var + second_var)),
       check_random = first_random + second_random
        )

norm_check_tbl %>%
  ggplot()+
  geom_line(aes(x=x, y=check_sum)) +
  geom_density(aes(x=check_random), color='red')


norm_check_tbl %>% 
  summarise(
    mean=mean(check_random), 
    var=var(check_random))
  


# z-standardization ------------------------------------------
tibble(
  x = rnorm(1000, mean=5, sd=2),
  z_x = (x-mean(x)) / sd(x)     
  ) %>% 
  summarise(
    mean = mean(z_x),     # => ~0
    var = var(z_x),       # => 1
    sd = sd(z_x)          # => 1
  )                       # => N(0, 1)


# central limit T ------------------------------------------
binom_generator <- function(p, n, size) {
  rbinom(size, n, p)
}


clt_viz <- function(sample_generator, expect_val, variance) {
  set.seed(123)
  n <- 5000
  
  clt_sample <- map_dbl(1:n, ~{
    sample <- sample_generator()
    sample_size <- length(sample)
    statistic <- sqrt(sample_size) * mean(sample) - expect_val / sqrt(variance)
  })

  x <- seq(-4, 4, length.out=1000)
  plot_data <- tibble(
    x=x_,
    ppf=dnorm(x)
  )
  sample_data <- tibble(
    clt_sample=clt_sample
  )
  
  ggplot(
    sample_data,
    aes(x=clt_sample)) +
  geom_density(aes(y=..density..)) +
  geom_line(data=plot_data, aes(x=x, y=pdf), color='red')
}

p <- 0.01
n <- 20
size <- 5000

clt_viz(
  sample_generator = function() binom_generator(p, n, size),
  expect_val = n*p,
  variance = n*p*(1-p)
)


expected_value <- 24
size <- 400

expon_generator <- function(expected_value, size) {
  rexp(size, rate=1 / expected_value)
}

clt_viz(
  sample_generator = function() expon_generator(expected_value, size),
  expect_val = n*p,
  variance =  n*p*(1-p)
)

#       ????????????????????????????????????


# z-test ---------------------------------------------------------------------------------
mu <-  .5
n <- 30
q <- 19
 
1 - pbinom(q-1, n, mu)   # discrete p_val
1 - pnorm(q, mean=n * mu, sd=sqrt(n*mu*(1-mu)))  # continuous p_val

tibble(
  x = 0:n,
  binom = dbinom(x, n, mu),
  norm = dnorm(x, mean = n * mu, sd = sqrt(n * mu * (1 - mu)))
) %>% 
  ggplot() +
  geom_col(aes(x = x, y = binom)) +
  geom_line(aes(x = x, y = norm), color = "red")


# func
z_p_value <- function(mean, size, mu, var) {
  z <- sqrt(size) * (mean - mu) / sqrt(var)
  return(pnorm(z, lower.tail=F))
}

# example
set.seed(123)
n <- 235
mean_iq <- 100
sd_iq <- 15

iq_sample <- rnorm(n, mean_iq, sd_iq)
mean(iq_sample)
sd(iq_sample)

# H_0: mean_iq = 100
# H_1: mean_iq > 100

z_p_val(mean(iq_sample), n, mean_iq, sd_iq^2)

prop.test(19, 30, 0.5, alternative = "greater", correct = TRUE)



# t-test ---------------------------------------------------------------------------------
m <- 10000
sample_size <- 7

plot_data <- tibble(
  
  # t-test distrib
  T_X <- map_dbl(1:m,   
                 ~{sample <- rnorm(sample_size, mean=5, sd=3)
                 sqrt(sample_size) * mean(sample-5) / sd(sample)}
                 ),
  
  # z-test distrib
  Z_X <- map_dbl(1:m,
                  ~{sample <- rnorm(sample_size, mean=5, sd=3)
                 sqrt(sample_size) * mean(sample-5) / 3}
                 )
) %>% 
  # transform to long format
  pivot_longer(cols = everything(), names_to = "statistics", values_to = "value")

# plot
plot_data %>% 
  ggplot(aes(x = value, fill = statistics)) +
  geom_density(aes(alpha=.5))       # t-test isnt sharp on the ends => not norm, 
                                    # z-test = normal distrib
# vec of sds for 10000 samples
sigma_hat <- map_dbl(1:m,    
                     ~{sample <- rnorm(sample_size, 5, 3)
                     sqrt(var(sample)) 
    }) %>% 
  tibble() %>% 
  ggplot(aes(x= .)) +
  geom_density() +       #distrib
  geom_vline(xintercept = 3, color='red') +  # real sd
  geom_vline(xintercept = mean(sigma_hat), color='blue') # mean(generated_sds)

# =>  real sd != generated sd


# example 1 ---------------
# h0 : mu >= 7
# h1 : mu < 7

X <- c(6.9, 6.45, 6.32, 6.88, 6.09, 7.13, 6.76)

t_stat <- sqrt(length(X)) * (mean(X) - 7) / sd(X)  # t_statistics = -2.52

p_value <- pt(t_stat, df = length(X) - 1)   # => 0.02 < apha => reject H0
qt(.05, df=length(X)-1)          # crit_t_val = -1.94 < t_stat => reject H0


# example 2 ---------------
# h0 : mu = 500
# h1 : mu >= 500

set.seed(123)
coffee <- rnorm(12, 505, 50)  # use rnorm as mean of means of samples -> norm_distrib
mean(coffee)

t_stat <- sqrt(length(coffee)) * (mean(coffee) - 500) / sd(coffee)

p_value <- pt(t_stat, df = length(coffee) - 1, lower.tail=F)# => 0.21 > apha => accept H0

ifelse(p_value < .05, 'reject H0', 'accept H0')

tibble(
  coffee = coffee
) %>% 
  ggplot() +
  geom_density(aes(x = coffee)) +
  geom_line(aes(x = coffee, y = dnorm(coffee, mean = 500, sd = sd(coffee))), 
            color = "red")


# ready func
t.test(coffee, mu=500, alternative='greater')





# two-sample t-test -----------------------------------------
library(tidyverse)
library(palmerpenguins)

penguins   
colnames(penguins)
mean(penguins$body_mass_g, na.rm=T)

t.test(penguins$body_mass_g, mu=4116)

# h0: mean_mass_f = mean_mass_m
# h1: mean_mass_f != mean_mass_m
t.test(body_mass_g ~ sex, data=penguins)     # group body_mass by sex from data

t.test(flipper_length_mm ~ sex, data=penguins %>% 
         filter(species == 'Adelie'))

t.test(                  # how to substitute ~ species
  x = penguins %>% 
    filter(species == 'Adelie') %>%
    pull(flipper_length_mm),                   # take vec
  y = penguins %>%
    filter(species == 'Gentoo') %>%
    pull(flipper_length_mm),
  alternative = 'greater',
  var.equal=T
)

penguins %>% 
  filter(species == 'Adelie') %>%
  pull(sex)




# how to apply test for diff n+norm cond ------------------------------------------------

# t1: mu = 2, sigma = 3
set.seed(9)

alpha <- 0.05
N <- 10000
n <- 5
mu_0 <- 2
sigma <- 3

perform_ttest <- function() {
  test <- rnorm(n, mean=mu_0, sd=sigma)
  control <- rnorm(n, mean=mu_0, sd=sigma)
  
  ttest_res <- t.test(test, control, alternative='two.sided')
  
  return(ttest_res$p.value <= alpha)
} 

results <- map_lgl(1:N, ~ perform_ttest())    # T/F for test = control for N times
cat('FPR:', sum(results)/N *100, '%\n')       # 5.01% > alpha but випадкова

binom.test(sum(results), N, p=alpha, alternative='two.sided')$conf.int  # test in CI

    # Results:
    # t-test works with norm_distrib + big/small sample_n 


# t2: exponential distrib with same mean = 10 but diff distrib_S

x <- seq(0, 100, length.out=1000)
test_dist <- dexp(x, rate = 1/10)
control_dist <- dexp(x-5, rate = 1/5)      # do smth => сер.чек >= 5 but mean(сер.чек) still =

ggplot() +
  geom_line(aes(x=x, y=test_dist), color='red') +
  geom_line(aes(x=x, y=control_dist), color='blue') +
  theme_minimal()

mean(test_dist)
mean(control_dist)


test <- rexp(n, rate=1/10)            
control <- rexp(n, rate=1/5) + 5

mean(test)
mean(control)

sd(test)
sd(control)

t.test(test, control, alternative='two.sided', var.equal=F)


# t3: custom func ---------------------------

check_test <- function(test_dist_func, control_dist_func, sample_size, alpha =0.05, N_exps=10000, to_print = T) {
  library(tidyverse)
  set.seed(123)
  
  results <- map_lgl(1:N_exps, ~{
    test <- test_dist_func(sample_size)
    control <- control_dist_func(sample_size)
    
    ttest_res <- t.test(test, control, var.equal=F, alternative='two.sided')
    
    return(ttest_res$p.value <= alpha)
  })
  
  fpr <- round(sum(results) / N_exps, 4) 
  
  conf_int <- binom.test(sum(results), 
                         N_exps, 
                         p=alpha, 
                         alternative='two.sided', 
                         conf.level=1-alpha)$conf.int
  
  if(to_print){
    cat("FPR:", fpr * 100, "%\n")
    cat("CI: [", conf_int[1], "", conf_int[2], "]\n")
  } else
    return(conf_int)
}

# \() - lambda or function()
check_test(\(x) rexp(x, rate = 1/10), \(x) rexp(x, rate = 1/5) + 5, 40,
           to_print = FALSE)



# t4  Sample size search

scale <- seq(20, 100, by = 1)

for (N in scale){
  conf_int <- check_test(\(x) rexp(x, rate = 1/10),
                         \(x) rexp(x, rate = 1/5) + 5,
                         sample_size = N,
                         N_exps = 10000,
                         to_print = FALSE)
  
  
  left <- conf_int[1]
  right <- conf_int[2]
  
  alpha <- 0.05
  
  if(left < alpha && alpha < right){
    cat("Sample size:", N, "\n")
    break
  }
}




# ----------------------------------------------------------------------------
pizza <- read_csv('https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/ab_experiment/ab_experiment.csv') %>% 
  mutate(experiment_group = factor(experiment_group, 
                                      levels=c('test', 'control'))) # => get this order, not by alphabet

 pizza %>% 
  summarize(.by = experiment_group, 
            mean_time = mean(delivery_time),
            median_time = median(delivery_time),
            sd_time = sd(delivery_time), 
            iqr = IQR(delivery_time),
            num = n())            # mean = median => probably norm_distrib

pizza %>%
  ggplot(aes(x=delivery_time, fill=experiment_group)) +
  # geom_histogram(aes(y=after_stat(density))) +
  geom_density(alpha=.5) +
  geom_vline(data=pizza %>% filter(experiment_group == 'control'), 
             aes(xintercept=mean(delivery_time)), color='red') +
  geom_vline(data=pizza %>% filter(experiment_group == 'control'), 
             aes(xintercept=mean(delivery_time)), color='blue') +
  theme(legend.position='bottom')


pizza %>% 
  ggplot(aes(x=experiment_group, y=delivery_time, fill=experiment_group)) +
  geom_boxplot() +
  geom_jitter(width=.2, alpha=.025) +
  theme(legend.position='none')


# h0: mu_test = mu_control  |  mu_test - mu_control = 0
# h1: mu_test < mu_control  |  mu_test = mu_control < 0

# 2-sample t-test
t.test(delivery_time ~ experiment_group, data=pizza, alternative='less', var.equal=F)





# Kolmogorov test -----------------------------------------------------------
library(tidyverse)

# Convergence of Fn to F ------------

calc_Fn <- function(sample, x){
  mean(sample <= x)
}

set.seed(123321)

N <- 5000
n_array <- seq(10, N, 10)

X <- rexp(N)
x <- 3
F_x <- pexp(x)

delta <- map_dbl(n_array, ~ calc_Fn(sample = X[1:.x], x=x) - F_x)

data <- tibble(n = n_array,
                delta = delta)

ggplot(data, aes(x=n, y=delta)) +
  geom_line(color='red') +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_vline(xintercept=0) +
  labs(title='Convergence of Fn to F',
       x='n',
       y='Delta') +
  theme_minimal()


# n+sup influence on Dn ------------

set.seed(4)
N <- 1000
n_array <- c(10, 100)

X <- rexp(N)
x <- seq(0, 5, length.out=1000)
F_x <- pexp(x)

plot_list <- map(n_array, function(n){
  F_n_x <- map_dbl(x, ~ calc_Fn(X[1:n], x=.x))
  delta <- abs(F_n_x - F_x)
  
  x_ind <- which.max(delta)   # find sup delta + its y to show on plot
  dn_x_dot <- x[x_ind]
  
  down_bound <- min(F_n_x[x_ind], F_x[x_ind])
  up_bound <- max(F_n_x[x_ind], F_x[x_ind])
  
  ggplot() +
    geom_line(aes(x=x, y=F_x), color='blue') +
    geom_line(aes(x=x, y=F_n_x), color='red') +
    geom_segment(aes(x=dn_x_dot, xend=dn_x_dot, 
                     y=down_bound, yend=up_bound), 
                 linetype='dashed', linewidth=0.8) +
    labs(title=paste('n =', n),
         x='x',
         y='F(x)') +
    theme_minimal()
})

library(gridExtra)
grid.arrange(grobs=plot_list, ncol=2)


# Kolmogorov-Smirnov tests ------------------------

set.seed(123321)
sample <- rnorm(10, mean=.1, sd=.04)

ks.test(sample, 'pnorm', mean=.1, sd=.04)  # D = 0.25 | ideal D = 0



# QQ-plot ----------------------------------------------------------------

# own
my_qq_plot <- function(sample){
  sample <- sort(sample)
  N <- length(sample)
 
  
  sample_quantiles <- sample[2:(N-1)]
  
  x <- seq(1, N-2) / N
  theoretical_qs <- qexp(x)
  
  data <- tibble(
    sample_quantiles = sample_quantiles,
    theoretical_qs = theoretical_qs
  )
  
  
  ggplot(data, aes(x=theoretical_qs, y=sample_quantiles)) +
    geom_point(color='darkblue') +
    labs(title='QQ-plot',
         x='Theoretical quantiles',
         y='Sample quantiles') +
    theme_minimal()
}

set.seed(123321)
sample <- rexp(100, rate=1/10)
my_qq_plot(sample)


# ready in ggplot
library(palmerpenguins)

penguins %>% 
  drop_na() %>% 
  ggplot(aes(sample=body_mass_g)) +
  geom_qq()+
  stat_qq_line()


penguins %>%         # filter sample | female+Adelie  => >~ to line
  drop_na() %>% 
  filter(sex == 'female',
         species == 'Adelie') %>% 
  ggplot(aes(sample=body_mass_g)) +
  geom_qq(alpha = .5) +
  stat_qq_line()




penguins_test <- penguins %>% 
  drop_na() %>% 
  filter(sex == 'female',
         species == "Adelie") %>% 
  distinct(body_mass_g) %>%
  pull(body_mass_g)

ks.test(penguins_test, "pnorm")





# ---------------------------------------------------------------------------
library(cryptoQuotes)

profits_tbl <- cryptoQuotes::get_quote(
  ticker = "AVAXUSDT",
  source   = 'binance',
  futures  = TRUE,
  interval = '1d',
  from = Sys.Date() - 200
) %>% 
  as_tibble() %>% 
  transmute(
    profit_avax = (close  - lag(close)) / lag(close) * 100,
    profit_btc = cryptoQuotes::get_quote(
      ticker = "BTCUSDT",
      source   = 'binance',
      futures  = TRUE,
      interval = '1d',
      from = Sys.Date() - 200
    ) %>% 
      as_tibble() %>% 
      transmute(
        profit_btc = (close  - lag(close)) / lag(close) * 100
      ) %>%
      pull(profit_btc)
  ) %>% 
  drop_na() %>% 
  pivot_longer(everything(), names_to = "coin", values_to = "profit")







# Mann-Whitney U-test ----------------------------------------------------------
library(tidyverse)

# plot of distribs
plot_1 <- tibble(
  x=seq(-4, 6, length.out=1000),
  A = dnorm(x, mean=1, sd=1),
  B = dnorm(x, mean=3, sd=1),
) %>% 
  ggplot(aes(x=x)) +
  geom_line(aes(y=A), color='red') +
  geom_line(aes(y=B), color='blue')

# plot of u-test
plot_2 <- tibble(
  value = c(rnorm(50, mean=1, sd=1),   # A where A = B
            rnorm(50, mean=1, sd=1),   # B where A = B
            rnorm(50, mean=1, sd=1),   # A where A != B
            rnorm(50, mean=3, sd=1)),  # B where A != B
  group = rep(c('A', 'B', 'A', 'B'), each=50),
  plot = rep(c('No shift', 'ShiftShift'), each=100)
) %>% 
  ggplot(aes(x=value, y=0, color=group)) +
  geom_jitter(alpha=.7) +
  facet_wrap(~plot) +
  theme_minimal() +
  theme(legend.position = 'none')


# library(gridExtra)
library(patchwork)

plot_1 / plot_2



# -------
set.seed(123)

sample_a <- rnorm(50, mean=1, sd=1)
sample_no_shift <- rnorm(50, mean=1, sd=1)
sample_b <- rnorm(50, mean=3, sd=1)

calculate_U <- function(A, B){
  sum(outer(A, B, '<'))     # sum of A < B
}

calculate_U(sample_a, sample_b)
length(sample_a) * length(sample_b) / 2

# -----
set.seed(123)

alpha <- .05
sample_size <- 50
N_exp <- 10000

# generate statistics
statistics <- map_dbl(1:N_exp, ~{       # make 10000 samples of 50 => calc U
  A <- rnorm(sample_size, mean=1, sd=1)
  B <- rnorm(sample_size, mean=1, sd=1)
  calculate_U(A, B)
})

max(statistics)

# expected U in ideal situation
EU <- sample_size*sample_size / 2   

tibble(statistics = statistics) %>% 
  ggplot(aes(x=statistics)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept=EU, color='red') +   # ideal num of pairs
  geom_vline(xintercept=quantile(statistics, 1-alpha), color='blue') +  # max val we can get
  labs(title='Mann-Whitney U-test',
       x='U',
       y='Frequency') +
  theme_minimal()


# monte carlo -------
A <- rnorm(sample_size, mean=1, sd=1)
B <- rnorm(sample_size, mean=1, sd=1)

man_whitney_distrib <- function(A, B) {
  N <- length(A)
  M <- length(B)
  DU <- N*M * (N +M + 1) / 12
  
  return(list(
    mean=N*M/2,
    sd = sqrt(DU)
  ))
}

x <- seq(min(statistics), max(statistics), length.out=1000)

# theoretical distrib
distribution <- man_whitney_distrib(A, B)

# expected U
EU <- length(A) - length(A) * length(B) / 2

tibble(statistics) %>% 
  ggplot(aes(x = statistics)) +
  geom_histogram(aes(y=after_stat(density)), bins = 30, 
                 fill='lightblue', color='black') +
  stat_function(fun = dnorm,
                args = list(mean = distribution$mean,
                            sd = distribution$sd),
                color = 'red') +             # ideal distrib
  geom_density(color = 'blue') +            # real distrib
  theme_minimal()


# test with ranks ---
sample_a <- c(1, 5, 8, 20)
sample_b <- c(2, 3, 6)

calculate_U(sample_a, sample_b)

ranks_b <- c(sample_a, sample_b) %>% 
  sort() %>% 
  rank() %>% 
  .[(length(sample_a) + 1):length(c(sample_a, sample_b))]


# ready func -----------
wilcox.test(sample_a, sample_b, alternative = 'greater')

# example 2
distr_a <- rnorm(50, mean=1, sd=1)
distr_b <- rnorm(50, mean=1, sd=1)

wilcox.test(distr_a, distr_b, alternative = 'less')



# example with bad similarity (great shift) / sd+mean infl on test ---------------
set.seed(123)

sz <- 1000
sample_size=50
alpha <- .5

# red, green, blue distribs
data_tbl <- tibble(
  distr = rnorm(n=1000, mean=1, sd=1),
  distr_shift = rnorm(n=1000, mean=2, sd=1),
  distr_scale = rnorm(n=1000, mean=1, sd=2)
  ) %>% 
  pivot_longer(cols=everything(), names_to='group', values_to = 'value')
  
data_tbl %>% 
  ggplot(aes(x=value, fill=group)) +
  geom_density(alpha=.5) +
  theme_minimal()

# compare red + blue distribs
mann_bad_cnt <- map_lgl(
  1:sz,
  ~ {
    A <- rnorm(sample_size, mean=1, sd=1)
    B <- rnorm(sample_size, mean=3, sd=1)
    wilcox.test(A, B, alternative='less')$p.value <= alpha
  }
)

CI_mann <- binom.test(sum(mann_bad_cnt), sz, conf.level = 1-alpha)$conf.int
cat('Mann_Whitney power:', round(sum(mann_bad_cnt) / sz, 4), '\n',
    'CI:', round(CI_mann, 4), '\n')

# Res: great similarity (small shift) 

# compare red + green distribs
mann_bad_cnt <- map_dbl(
  1:sz,
  ~ {
    A <- rnorm(sample_size, mean=1, sd=1)
    B <- rnorm(sample_size, mean=1, sd=2)
    wilcox.test(A, B, alternative='less')$p.value <= alpha
  }
)

CI_mann <- binom.test(sum(mann_bad_cnt), sz, conf.level = 1-alpha)$conf.int
cat('Mann_Whitney power:', round(sum(mann_bad_cnt) / sz, 4), '\n',
    'CI:', round(CI_mann, 4), '\n')

# Res: not as good similarity (big dispertion)

A <- rnorm(sample_size, mean=1, sd=1)
B <- rnorm(sample_size, mean=1, sd=2)
t.test(A, B, alternative='two.sided')
wilcox.test(A, B, alternative='two.sided')


# mann-whitney test is NOT about medians | t-test/mann-whitney comparison
set.seed(123)

tibble(distr = runif(1000, -1, 1),
       distr_shift = runif(1000, -100, 100)
) %>% 
  pivot_longer(cols = everything(), names_to='group', values_to='value') %>% 
  ggplot(aes(x=value, fill=group)) +
  geom_density(alpha=.5) +
  facet_wrap(~group, scale='free') +
  theme_minimal()

sz <- 1000

results <- map(
  1:sz,
  ~ {
    # +2 samlpes
    test <- runif(1000, -1, 1)        
    control <- runif(1000, -100, 100)
    
    # diff tests
    mann_pval <- wilcox.test(test, control, alternative='two.sided')$p.value
    t_pval <- t.test(test, control, alternative='two.sided')$p.value
    
    # p-vals | know that should be <= alpha
    mann_reject <- if_else(mann_pval <= alpha, 1, 0)
    t_reject <- if_else(t_pval <= alpha, 1, 0)
    
    tibble(mann_reject, t_reject)
  }
)

# count rejections
mann_bad_cnt <- map_dbl(results, 'mann_reject') %>% sum()
t_bad_cnt <- map_dbl(results, 't_reject') %>% sum()

mann_CI <- binom.test(mann_bad_cnt, sz, conf.level=0.95)$conf.int
t_CI <- binom.test(t_bad_cnt, sz, conf.level=0.95)$conf.int

cat('Mann-Whitney signif.rate:', round(mann_bad_cnt / sz, 4), '\n',
    'CI:', round(mann_CI, 4), '\n',
    't-test signif.rate:', round(t_bad_cnt / sz, 4), '\n',
    'CI:', round(t_CI, 4), '\n')

# Results:
# t-test ~5% found diff, but mean=median=0 in our distribs
# mann-whit ~10% found diff => bad for median, t-test better 


# case ----------------------------------

# +discounts for 1 group => >num_customers + <avg_check | want to know if it's good
# h0: avg_check same in 2 groups
# h1: avg_check diff in 2 groups

set.seed(8)

data <- rexp(1000, rate=1/6)   # avg_check usually = exp_distrib

# hist
ggplot(data.frame(x=data), aes(x=x)) +
  geom_histogram(binwidth = diff(range(data)) / 30,
                 fill = 'blue',
                 color='black',
                 alpha=.7) +
  labs(title = 'Possible ARPPU', x='value', y='density') +
  theme_minimal(base_size=12)

# 1. Знижки призвели до зростання к-ті клієнтів, які платять на 5%.
# 2. Середній до знижок - 7 грн, після - 6 грн.

# МС до знижок: 7 * 50% = 3.5
# МС після знижок: 6 * 55% = 3.3

library(furrr)

set.seed(8)

sz <- 10000
n <- 5000

# Функція для виконання тестів для кожної пари тестових і контрольних даних
run_test <- function(i) {
  test_zero_array <- rbinom(n, 1, 0.55)    # sample where 55% bought smth (1)
  control_zero_array <- rbinom(n, 1, 0.5)   # sample where 50% bought smth (1)
  
  test <- rexp(n, rate = 1/6) * test_zero_array    # => exp_distrib with checks + 0 if dont buy smth
  control <- rexp(n, rate = 1/7) * control_zero_array
  
  # Перевірка гіпотез
  mann_pvalue <- wilcox.test(test, control, alternative = "greater")$p.value
  ttest_pvalue <- t.test(test, control, alternative = "greater")$p.value
  
  c(mann_pvalue < 0.05, ttest_pvalue < 0.05)
}

# Паралельне виконання тестів через furrr
availableCores <- parallel::detectCores()
plan(multisession, workers = 8) # 20 ядер

results <- future_map(1:sz, ~ run_test(.x), .progress = TRUE)

# Підсумовуємо відхилені гіпотези для кожного тесту
mann_bad_cnt <- sum(map_int(results, ~ .x[1]))
ttest_bad_cnt <- sum(map_int(results, ~ .x[2]))

# Створюємо довірчі інтервали для кожного тесту
mann_fpr_ci <- binom.test(mann_bad_cnt, sz)$conf.int
ttest_fpr_ci <- binom.test(ttest_bad_cnt, sz)$conf.int

# Виводимо результати
cat("Mann-whitneyu GREATER FPR:", round(mann_bad_cnt / sz, 4), "CI:", round(mann_fpr_ci, 4), "\n")
cat("T-test GREATER FPR:", round(ttest_bad_cnt / sz, 4), "CI:", round(ttest_fpr_ci, 4), "\n")

# Res: mann-w find many diff => say it is good
# t-test find few diff => no good => better use t-test 
# mann-wit use to detect shifts in distrib



# ----------------------------------------------------------------------------
                 
# xi_square --------------------------------------------------------------------

library(tidyverse)

# df infl on xi^2
x_axis <- seq(1, 10, by=.001)

df_vals <- c(1, 2, 3, 5, 10)

df_delta <- map_dfr(df_vals, ~tibble(x=x_axis,
                                     density=dchisq(x_axis, df=.x),
                                     df=factor(.x)))


df_delta %>%
  ggplot(aes(x=x, y=density, color=df)) +
  geom_line() +
  theme_minimal() +
  labs(title='Chi-square distribution',
       x='x',
       y='Density') +
  theme(legend.position='bottom')

# exmp
freq <- c(5, 8, 9, 8, 10, 20)

tau <- sum(freq) * sum((freq) / sum(freq) - 1/6)^2 / (1/6))    # xi^2

1 - pchisq(tau, df=5)

chisq.test(freq)

# ---

gen_monte_carlo_exp_for_norm_kstest <- function(n, loc, scale, alpha, N_runs) {
  # Функція для перевірки критерію Колмогорова для нормального розподілу методом Монте-Карло
  # Повертає частку відкидань гіпотези та довірчий інтервал 
  # для цієї частки
  # 
  # Параметри:
  #   - n: розмір вибірки
  # - loc: істинна середня розподілу
  # - scale: істинне середньоквадратичне відхилення розподілу
  # - alpha: рівень значущості
  # - N_runs: число експериментів у методі Монте-Карло
  
  positive <- map_lgl(1:N_runs, ~ {
    sample <- rnorm(n, mean = loc, sd = scale)
    pvalue <- ks.test(sample, "pnorm")$p.value
    pvalue <= alpha
  })
  
  positive_rate <- sum(positive) / N_runs
  CI <- binom.test(x = sum(positive), n = N_runs, conf.level = 0.95, alternative = "two.sided")
  
  return(
    list(
      positive_rate = positive_rate,
      confint_left_bound = CI$conf.int[1],
      confint_right_bound = CI$conf.int[2]
    ))
}


gen_monte_carlo_exp_for_discrete_norm_chi_sq <- function(n, loc, scale, n_bins, alpha, N_runs) {
  # Функція для перевірки критерію хі-квадрат для дискретизованого нормального розподілу методом Монте-Карло
  
  positive <- map_lgl(1:N_runs, ~ {
    sample <- rnorm(n, mean = loc, sd = scale)
    
    # Визначаємо границі бінів за квантилями нормального розподілу
    bins <- qnorm(seq(0, 1, length.out = n_bins + 1))
    
    # Дискретизуємо вибірку
    observed <- hist(sample, breaks = bins, plot = FALSE)$counts
    expected <- rep(n / n_bins, n_bins)
    
    # Обчислюємо p-value критерію хі-квадрат
    pvalue <- chisq.test(x = observed, p = rep(1 / n_bins, n_bins), rescale.p = TRUE)$p.value
    
    pvalue <= alpha
  })
  
  positive_rate <- sum(positive) / N_runs
  CI <- binom.test(x = sum(positive), n = N_runs, conf.level = 0.95, alternative = "two.sided")
  
  return(
    list(
      positive_rate = positive_rate,
      confint_left_bound = CI$conf.int[1],
      confint_right_bound = CI$conf.int[2]
    ))
}



# Визначаємо значення n
n_values <- seq(100, 900, by = 200)

KS_results <- map_dfr(n_values, ~ {
  gen_monte_carlo_exp_for_norm_kstest(n = .x, loc = 0, scale = 1, alpha = 0.05, N_runs = 10000)
})

Chi_Sq_results_4 <- map_dfr(n_values, ~ {
  gen_monte_carlo_exp_for_discrete_norm_chi_sq(n = .x, loc = 0, scale = 1, n_bins = 4, alpha = 0.05, N_runs = 10000)
})

Chi_Sq_results_8 <- map_dfr(n_values, ~ {
  gen_monte_carlo_exp_for_discrete_norm_chi_sq(n = .x, loc = 0, scale = 1, n_bins = 8, alpha = 0.05, N_runs = 10000)
})

KS_results %>% 
  mutate(method = "Kolmogorov-Smirnov") %>% 
  bind_rows(
    Chi_Sq_results_4 %>% mutate(method = "Chi-Sq (4 bins)"),
    Chi_Sq_results_8 %>% mutate(method = "Chi-Sq (8 bins)")
  ) %>% 
  mutate(n = rep(n_values, 3)) %>%
  ggplot(aes(x = n, y = positive_rate, color = method)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 1.2) +
  geom_ribbon(aes(ymin = confint_left_bound, ymax = confint_right_bound, fill = method), alpha = 0.3) +
  facet_wrap(~ method, scales = "free_y", ncol = 1) +
  labs(title = "FPR dependence on sample size n", x = "Sample size n", y = "FPR") +
  theme_minimal(base_size = 14)




# xi^2 for independence -----

obs <- matrix(c(
  932299, 680684, 794884, 585978, 470660,	397554,
  490658,	519094,	648085,	416680,	347016,	315555,
  223166,	347325,	306182,	219092,	254512,	237701,
  204310,	426824,	277656,	127961,	237965,	220446
), nrow=4, byrow=T)

chisq.test(obs)        # p_val < alpha => have dependence


# Функція для перевірки критерію хі-квадрат для біноміального розподілу методом Монте-Карло
gen_monte_carlo_exp_for_binom <- function(n, p1, p2, correction, alpha, N_runs) {
 
  positive <- map_lgl(1:N_runs, ~ {
    n_1_1 <- rbinom(1, size = n, prob = p1)
    n_1_2 <- n - n_1_1
    n_2_1 <- rbinom(1, size = n, prob = p2)
    n_2_2 <- n - n_2_1
    
    # Визначаємо спостережувану таблицю
    if ((n_1_1 + n_2_1 != 0) && (n_1_2 + n_2_2 != 0)) { # Якщо всі комірки не нульові
      observed <- matrix(c(n_1_1, n_1_2, n_2_1, n_2_2), nrow = 2, byrow = TRUE)
      p_value <- chisq.test(observed, correct = correction)$p.value
    } else {
      p_value <- 1
    }
    
    # Перевіряємо, чи відкидаємо нульову гіпотезу
    p_value <= alpha
  })
  
  positive_rate <- sum(positive) / N_runs
  CI <- binom.test(x = sum(positive), n = N_runs, conf.level = 0.95, alternative = "two.sided")
  
  return(
    list(
      positive_rate = positive_rate,
      confint_left_bound = CI$conf.int[1],
      confint_right_bound = CI$conf.int[2]
    ))
}


n_values <- seq(2, 30, by = 2)

FPR_w_yates <- map_dfr(n_values, ~ {
  gen_monte_carlo_exp_for_binom(n = .x, p1 = 0.3, p2 = 0.3, correction = TRUE, alpha = 0.05, N_runs = 10000)
})

FPR_wo_yates <- map_dfr(n_values, ~ {
  gen_
  _carlo_exp_for_binom(n = .x, p1 = 0.3, p2 = 0.3, correction = FALSE, alpha = 0.05, N_runs = 10000)
})

FPR_w_yates %>% 
  mutate(method = "Yates") %>% 
  bind_rows(
    FPR_wo_yates %>% mutate(method = "Without Yates")) %>% 
  mutate(n = rep(n_values, 2)) %>% 
  ggplot(aes(x = n, y = positive_rate, color = method)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = confint_left_bound, ymax = confint_right_bound, fill = method), alpha = 0.3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 1.2) +
  labs(title = "FPR dependence on sample size n", x = "Sample size n", y = "FPR") +
  theme_minimal(base_size = 14)

# Висновок: включена поправка Єйтса зменшує FPR. 
# Це не дивно, оскільки p-value у критерії з увімкненою поправкою Єйтса буде строго вищим. 
# У цьому випадку поправка Єйтса може бути потрібна тільки для n<10 , щоб забезпечити коректність критерію.


# --- Давайте тепер перевіримо вплив на потужність критерію.

n_values <- seq(10, 2000, by = 200)

FPR_w_yates <- map_dfr(n_values, ~ {
  gen_monte_carlo_exp_for_binom(n = .x, p1 = 0.3, p2 = 0.35, correction = TRUE, alpha = 0.05, N_runs = 10000)
})

FPR_wo_yates <- map_dfr(n_values, ~ {
  gen_monte_carlo_exp_for_binom(n = .x, p1 = 0.3, p2 = 0.35, correction = FALSE, alpha = 0.05, N_runs = 10000)
})

FPR_w_yates %>% 
  mutate(method = "Yates") %>% 
  bind_rows(
    FPR_wo_yates %>% mutate(method = "Without Yates")) %>% 
  mutate(n = rep(n_values, 2)) %>% 
  ggplot(aes(x = n, y = positive_rate, color = method)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = confint_left_bound, ymax = confint_right_bound, fill = method), alpha = 0.3) +
  labs(title = "TPR dependence on sample size n", x = "Sample size n", y = "TPR") +
  theme_minimal(base_size = 14)



# tbl спряженості ----------------------------
library(tidyverse)

penguins <- palmerpenguins::penguins

table(penguins$sex, penguins$species)

chisq.test(table(penguins$sex, penguins$species))



# ----------------------------------------------------------------------------

# bootstrap ---------------------------------------------------------------
library(tidyverse)

n <- 20000

# +population (not sample)
gamma_sample <- rgamma(n, shape = 2, rate = 1/3)
hist(gamma_sample)
mean(gamma_sample)
sd(gamma_sample)

theta <- qgamma(0.75, shape=2, rate=1/3)   # true/expected val
theta_obs <- quantile(gamma_sample, 0.75)  # observed val

# +subsamples func
theta_calc_func <- function(n, sample_func, theta_func, b = 10000) {
  map_dbl(1:b, ~{
    sample <- sample_func(n)
    theta_true <- theta_func(sample)
  }) }

theta_vec <- theta_calc_func(20000, 
                             \(x) rgamma(x, shape=2, rate=1/3),
                             \(x) quantile(x, .75))

tibble(theta_vec) %>% 
  ggplot(aes(x=theta_vec)) +
  geom_histogram() +
  theme_minimal()
ks.test(theta_vec, 'pnorm', mean=mean(theta_vec), sd=sd(theta_vec))
tibble(theta_vec) %>% 
  ggplot(aes(sample=theta_vec)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()


delta <- theta_vec - theta

tibble(delta) %>% 
  ggplot(aes(x=theta_vec)) +
  geom_histogram() +
  theme_minimal()
ks.test(delta, 'pnorm', mean=mean(delta), sd=sd(delta))
tibble(theta_vec) %>% 
  ggplot(aes(sample=delta)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal()

alpha <- 0.05

CI <- quantile(theta_vec, c(alpha/2, 1-alpha/2))

cat('CI:', round(CI, 4), '\n')
cat('True theta:', theta)


# bootstrap for samples, not population -------

x <- c(5, 8, 6, 10, 15)

sample(x, length(x), replace=T)

simple_bootstrap <- function(sample, theta_func, alpha=0.05, b=10000) {
  n <- length(sample)
  
  theta_ci <- map_dbl(1:b, ~{
    sample_b <- sample(sample, n, replace=T)
    theta_func(sample_b)
  }) %>% 
    quantile(c(alpha/2, 1-alpha/2))
  
  c(2*theta_func(sample) - theta_ci[2],
    2*theta_func(sample) - theta_ci[1])
}

ci_gamma <- simple_bootstrap(gamma_sample, \(x) quantile(x, .75))



# bootstrap diff CI ------------

library(boot)

n <- 20000
# +sample of gamma distrib
gamma_sample <- rgamma(n, shape=2, rate = 1/3)

# theoretical quantile
theta <- qgamma(0.75, shape=2, rate=1/3)
# empirical quantile
theta_estimate <- quantile(gamma_sample, .75)

# bootstrap | resample 10k times
boot_data <- boot(data=gamma_sample, statistic= \(x, i) quantile(x[i], .75), R=10000)

# thetas in subsamples
boot_data$t
# theta in sample
boot_data$t0

# 2-sampled bootstrap
boot_data %>%
  boot.ci(type='basic', conf=.95)

boot_ci$basic[4:5]
theta

# ------
library(tidyverse)
library(microbenchmark)

microbenchmark(
  simple_bootstrap(gamma_sample, \(x) quantile(x, 0.75)
  ),
  boot.ci(
    boot(data = gamma_sample,
         statistic = \(x, i) quantile(x[i], 0.75),
         R = 10000,
         parallel = "multicore",  # for faster execute
         ncpus = 4),
    type = "basic", conf = 0.95)$basic[4:5],
  times = 5
)

boot_ci_all <- boot_data %>% 
  boot.ci(type='all', conf=.95)

boot_ci_basic <- boot_data %>% 
  boot.ci(type='basic', conf=.95)

boot_ci_norm <- boot_data %>% 
  boot.ci(type='norm', conf=.95)

boot_ci_perc <- boot_data %>% 
  boot.ci(type='perc', conf=.95)

cat('Basic:', boot_ci_basic$basic[4:5], '\n',
    'Norm:', boot_ci_norm$norm[4:5], '\n',
    'Perc:', boot_ci_perc$perc[4:5], '\n')

# IC_pr_plot --------------------------------------------------------------
library(tidyverse)
datasaurus <- read_csv('https://raw.githubusercontent.com/Aranaur/aranaur.rbind.io/main/datasets/datasaurus/datasaurus.csv')

summary(datasaurus)

datasaurus %>%
  distinct(dataset)

datasaurus %>%
  filter(dataset=='dino') %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    sd_y = sd(y)
  )



