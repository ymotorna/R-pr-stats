### Instructions

# 1) Write the solution after "# Solution:" comment and before "###"
# It should be the code that will do the exercise and a comment if specified
# 2) Save the file as HWX_name_surname.R
# For example: HW1_Dmytro_Krukovets.R
# 3) Send through Moodle

# Exercise #1: Fill the gaps and make the code to produce TRUE outputs.
# Uncomment the code and fill in the missing parts to make the code work


# Solution:
num_var <- "42"
char_var <- "Hello"
log_var <- "FALSE"

is.numeric(as.numeric(num_var))
is.character(char_var)
is.logical(as.logical(log_var))

###

# Exercise #2: Calculate the sum of squares for integers from 1 to 100.
# Uncomment the code and fill in the missing parts to make the code work


# Solution:
vals <- 1:100
squared <- vals ^ 2
sum_squares <- sum(squared)
print(sum_squares)

###

# Exercise #3: Fix the syntax to create a vector and change the 3rd element to NA.
# Uncomment the code and fill in the missing parts to make the code work


# Solution:
my_vector <- c(1, 3, 5, 7, 9)
my_vector[3] <- NA
print(my_vector)

###

# Exercise #4: Create a vector vec with 100 random integers between 1 and 100.
# Calculate the sum of all elements in vec and save it to sum1. 
# Then calculate the sum of all elements that are divisible by both 3 and 5
# and save it to sum2. Then replace these elements divisible by 3 or 5 
# with NA. Calculate the sum of the remaining values and save into sum3. 
# Check whether sum1 is equal to sum2 + sum3

# Solution:
vec <- sample(1:100, 100)
sum1 <- sum(vec)

vec_3_5 <- vec[vec%%3 == 0 & vec%%5 == 0]
sum2 <- sum(vec_3_5)

vec[vec %in% vec_3_5] <- NA
sum3 <- sum(vec, na.rm=TRUE)

sum1 == sum2+sum3

######vec

# Exercise #5: Simulate a vector with ages of 50 people between 10 and 70.
# Identify people who are "minors" (under 18), "adults" (18–60), 
# and "seniors" (over 60) by creating a character vector age_group to classify 
# each age into one of these categories using ifelse(). 
# Calculate and print the percentage of each age group.

# Solution:
ages <- sample(10:70, 50)
age_group <- ifelse(ages<18, 'minors', ifelse(ages>60, 'seniors', 'adults'))

paste('minors:', length(age_group[age_group=='minors'])/50*100, '%')
paste('adults:', length(age_group[age_group=='adults'])/50*100, '%')
paste('seniors:', length(age_group[age_group=='seniors'])/50*100, '%')

###

# Exercise #6: Create a sequence seq1 of numbers from 1 to 500 with a step 5.
# Extract every 5th number from this sequence into a new vector seq2 (try to do 
# it in a smart way). Calculate the sum of these numbers, when each
# is multiplied by its index number in the new vector. Print the final sum.

# Solution:
seq1 <- seq(1, 500, 5)
seq2 <- seq1[seq(1, length(seq1), 5)]

sum(seq2 * seq(1, length(seq2)))
###

# Exercise #7: You have the following two vectors:
# names <- c("Anna", "John", "Maria", "Peter", "Sophia")
# scores <- c(56, 73, 89, 45, 90)
# Find the name of the person with the highest score.
# Remove the person with the lowest score from both vectors.
# Reorder the remaining people and scores in descending order of their scores.

# Solution:
names <- c("Anna", "John", "Maria", "Peter", "Sophia")
scores <- c(56, 73, 89, 45, 90)

names[order(scores, decreasing = TRUE)[1]]

names <- names[order(scores)[2:length(names)]]
scores <- scores[order(scores)[2:length(scores)]]

names <- names[order(scores, decreasing=T)]
scores <- scores[order(scores, decreasing=T)]

###

# Exercise #8: Generate a vector vec with 20 random integers between -50 and 50.
# Create a new vector signs that contains "Positive" for positive numbers, 
# "Negative" for negative numbers, and "Zero" for zero values in vec.
# Replace all negative values (use signs to see which ones) in vec with their 
# absolute values. Calculate the product (using prod()) of all 
# non-zero elements (use signs to see which ones) in vec.

# Solution:
vec <- sample(-50:50, 20)
signs <- ifelse(vec<0, 'negative', ifelse(vec>0, 'positive', 'zero'))

vec[signs=='negative'] <- abs(vec[signs=='negative'])
prod(vec[signs != 'zero'])

###

# Exercise #9: Create a vector vec with 30 random numbers between 1 and 100.
# Sort the numbers in descending order. Print the sum of all the elements 
# in the top half and the bottom half of the sorted vector, 
# then compare with the total sum divided by two.

# Solution:
vec <- runif(30, 1, 100)
vec_sorted <- sort(vec, decreasing = T)

top_half_sum <- sum(vec_sorted[1:15])       # => 1109.674
bottom_half_sum <- sum(vec_sorted[16:30])   # => 445.3452
total_half_sum <- sum(vec)/2                # => 777.5096

top_half_sum - total_half_sum               # => 332.1643
total_half_sum - bottom_half_sum            # => 332.1643

# bottom_half_sum < total_half_sum < top_half_sum with same difference = 332.1643

###