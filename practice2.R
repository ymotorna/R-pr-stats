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


# Exercise #1: Create a vector grades with values: 
# "A", "B", "C", "A", "B", "C", "B", "A", "C","A", "B", "C", "A", "B", "C", "B", "A", "C", "A", "A", "C", "C", "A"
# Convert it into a factor with levels "A", "B", "C". 
# Then reorder the levels to be "C","A","B".
# Count how many times each grade appears, using *some* function

# Solution:
grades <- c("A", "B", "C", "A", "B", "C", "B", "A", "C","A", "B", "C", "A", "B", "C", "B", "A", "C", "A", "A", "C", "C", "A")

grades <- factor(grades, levels=c("A", "B", "C"))
grades <- relevel(grades, ref="C")

table(grades)
###

# Exercise #2: Load the built-in "airquality" dataset in R 
# into "airq". Display the first 6 rows, column names, and summary.
# Show rows where Ozone is greater than 50 and Temp is above 80. 
# Create a new column Heat_Index in the airquality dataset using 
# the formula: Heat_Index=Temp+(Humidity/100)×10
# Assume Humidity is derived from (100 - Wind) for simplicity.

# Solution:
airq <- airquality

head(airq)
colnames(airq)
summary(airq)

airq[airq$Ozone > 50 & airq$Temp > 80, ]

airq$Heat_Idx <- airq$Temp + ((100 - airq$Wind)/100) * 10
###

# Exercise #3: Write a function ozone_category() that takes 
# an Ozone value as input and returns:
# "Low" if Ozone < 30
# "Moderate" if 30 <= Ozone <= 60
# "High" if Ozone > 60
# Note that you need to check first whether there is an NA
# value with is.na() and return NA in this case.
# Apply this function to the Ozone column in the airquality 
# dataset to create a new column Ozone_Category.

# Solution:
ozone_category <- function(oz) {
  if (is.na(oz)) {
    return(NA)
  } else if (oz < 30) {
    return('Low')
  } else if(oz <= 60) {
    return('Moderate')
  } else {
    return('High')
  }
}

airq$Ozone_Category <- sapply(airq$Ozone, ozone_category)
###

# Exercise #4: Write a function extreme_weather() that takes Temp 
# and Wind as inputs, returns T if Temp > 90 and Wind < 5, F otherwise 
# (including if some variable is NA). Use it to identify all days with extreme 
# weather conditions in the airq dataset. Count how many such days exist.

# Solution:
extreme_weather <- function(temp, wind) {
  ifelse(is.na(temp) | is.na(wind), F, ifelse(temp>90 & wind<5, T, F))}

count_days <- length(airq[extreme_weather(airq$Temp, airq$Wind), ])
###

# Exercise #5: Write a function col_stats() that calculates 
# the mean, median, and standard deviation of a numeric column 
# in a data frame. Use this function on the Temp, Wind, and Ozone 
# columns of airquality. (Make sure to handle NA values appropriately.)

# Solution:
col_stats <- function(col) {
  mean_val <- mean(col, na.rm = T)
  median_val <- median(col, na.rm = T)
  sd_val <- sd(col, na.rm = T)
  return(data.frame(mean=mean_val, median=median_val, standard_deviation=sd_val))
}

col_stats(airq$Temp)
col_stats(airq$Wind)
col_stats(airq$Ozone)
###

# Exercise #6: Replace all missing values in the Ozone column 
# with the column mean. (Hint: Use is.na() and mean() with na.rm = T)

# Solution:
airq$Ozone[is.na(airq$Ozone)] <- mean(airq$Ozone, na.rm=T)
###

# Exercise #7: Use a while loop to iterate through the Temp 
# column in the airquality dataset. 
# Count how many days have temperatures above 85 with a "count" assigned 0 before the while loop.

# Solution:
count <- 0     # => 34
rows <- 1

while (rows <= nrow(airq)) {
 if (airq[rows, ]$Temp > 85) {
   count <- count+1
 }
  rows <- rows+1
}
###

# Exercise #8: Rewrite the previous task using a for loop 
# instead of a while loop. Compare the results to ensure 
# they are the same.

# Solution:
count <- 0      # => 34

for (row in 1:nrow(airq)) {
  if (airq[row, ]$Temp > 85) {count <- count+1}
}

###

# Exercise #9: Custom Sorting
# Write a function sort_desc() that takes a numeric vector 
# as input and returns the vector sorted in descending order 
# without using the sort() function.
# (Hint: Use a WHILE loop and the max() function)

# Solution:
sort_desc <- function(vec) {
  sorted <- c()
  l <- length(vec)
  count <- 1
  
  while (count <= l) {
    max_val <- max(vec)
    sorted <- append(sorted, max_val)
    vec <- vec[vec != max_val]
    count <- count+1
  }
  
  return(sorted)
}

v <- c(1, 5, 3, 2, 4)
sort_desc(v)
###

savehistory("HW2_Yelyzaveta_Motorna.Rhistory")
