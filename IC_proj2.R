library(tidyverse)

cleaned_data <- read_csv("2020_cleaned.csv")

colnames(cleaned_data)
unique(cleaned_data$CrashSeverity)
unique(cleaned_data$PersonInjurySeverity)
unique(cleaned_data$P_PSN)


# Чи ж залежність від місця сидіння і травми -----------------------------------

# we want to check for dependency of injury severity and seat location in car crashes
# we will create contingency table and use chi-squared test
# h0: there is no dependency between injury severity and seat location
# h1: there is dependency between injury severity and seat location

# divide data + create contingency table
seat_severity_tbl <- cleaned_data %>% 
  transmute(PersonInjurySeverity, seat = case_when(
    P_PSN == '11' ~'driver',
    P_PSN %in% c('12', '13') ~'front',
    P_PSN %in% c('23', '33') ~'right',
    P_PSN %in% c('32', '22') ~'center',
    P_PSN %in% c('31', '21') ~'left')) %>%
  filter(!is.na(seat)) %>% 
  mutate(seat = factor(seat, levels=c('driver', 'front', 'right', 'center', 'left'))) %>%
  group_by(PersonInjurySeverity, seat) %>% 
  summarise(frequency = n()) %>% 
  pivot_wider(names_from=PersonInjurySeverity, values_from=frequency, values_fill=0)
  
# chi-squared test
chisq.test(seat_severity_tbl[,2:4])


# monte carlo
set.seed(123)
matrix <- as.matrix(seat_severity_tbl[,2:4])

chisq_mc <- function(tbl, df, exp_size=50000, alpha=0.05){
  # number of observations
  obs_num <- sum(tbl)
  
  # matrix with probabilities of each cell
  prob_matrix <- tbl / obs_num
  
  # generate new matrices with same probabilities
  samples <- map_lgl(1:exp_size, ~{
                  # GPT prompt: how to generate matrix for chisq.test
    chisq_sample <- matrix(rmultinom(1, obs_num, prob_matrix),
                           nrow=nrow(tbl))
    
    p_value <- chisq.test(chisq_sample)$p.value
    p_value <= alpha
  })
    
  positive_rate <- sum(samples) / exp_size
  CI <- binom.test(sum(samples), exp_size, conf.level=1-alpha)$conf.int
  
  invisible(cat('Monte Carlo chisq_test:\n',
                'positives:', positive_rate, '\n',
                'CI:', CI[1], '; ', CI[2], '\n'))
}

chisq_mc(matrix, df=8)

# power = 1, meaning after running a test for 50k similar samples chisq.test 
# has rejected h0 hypothesis in all the cases




# plot
seat_severity_tbl %>% 
  mutate(total = rowSums(.[,2:4]),
         across(2:4, ~.x/total*100)) %>%
  pivot_longer(cols=c('Fatality', 'Injury', 'No Injury'),
               names_to='severity',
               values_to='freq_perc') %>% 
  
  ggplot(aes(x=seat, y=freq_perc, fill=severity)) +
  geom_col(position='stack') +
  
  scale_fill_manual(values=pallete) +
  labs(title='Injury severity by seat location',
       x='Seat location',
       y='Percentage of injuries',
       fill='Severity') +
  theme_custom




# Чи впливає пасок безпеки на аварію -------------------------------------------

# we assume that in most fatality cases a victim was not wearing a seat belt
# h0: a victim was wearing a seat belt in 50% cases | p = 0.5
# h1: a victim in most cases was not wearing a seat belt | p < 0.5

# create vector of seat belt usage
belt_usage <- cleaned_data %>% 
  filter(PersonInjurySeverity == 'Fatality') %>% 
  pull(P_SAFE)

belt_usage <- ifelse(belt_usage == 1, 0, ifelse(belt_usage == 2, 1, NA))
belt_usage <- belt_usage[!is.na(belt_usage)]

# binomial test
binom.test(sum(belt_usage), length(belt_usage), p=0.5, alternative='less')

# plot
tibble(
  x = 0:length(belt_usage),
  probability = dbinom(x, length(belt_usage), 0.5),
  is_crit_val = x <= qbinom(0.05, length(belt_usage), prob=0.5)) %>%
  
  ggplot(aes(x=x, y=probability, fill=is_crit_val)) +
  geom_col() +
  
  scale_fill_manual(values=pallete) +
  xlim(300, 500) +
  geom_vline(xintercept=sum(belt_usage), color='#E07A5F', linewidth=1.5) +
  labs(title='Probability of wearing a seatbelt in fatal car crashes',
       x='Number of cases',
       y='Probability',
       fill = 'Critical region') +
  theme_custom



theme_custom <- theme_minimal(base_size = 14, base_family = "Arial") +
    theme(
      plot.background = element_rect(fill = "#675a49", color = NA),
      panel.background = element_rect(fill = "#675a49", color = NA),
      panel.grid.major = element_line(color = "#F1FAEE"),
      panel.grid.minor = element_line(color = "#F1FAEE", linetype = "dotted"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white", face = "bold"),
      plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = element_text(color = "white", hjust = 0.5),
      legend.background = element_rect(fill = "#675a49"),
      legend.key = element_rect(fill = "#675a49", color = NA),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white", face = "bold")
    )

pallete = c("#A8DADC", "#457B9D", "#F1FAEE", "#E63946", "#1D3557")









