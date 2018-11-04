library(nycflights13)
library(dplyr)

#5.5.2
flights_time <- select(flights, dep_time, sched_dep_time)
flights_since_mid <- mutate(flights_time, 
          dep_hours = (dep_time %/% 100),
          sched_hours = (sched_dep_time %/% 100),
          dep_minutes = (dep_time %% 100),
          schedmins = (sched_dep_time %% 100),
          dep_since_mid = ((dep_hours * 60) + dep_minutes),
          schedmid = ((sched_hours * 60) + schedmins))

#Compare air_time with arr_time - dep_time. What do you expect to see? 
#What do you see? What do you need to do to fix it?
flights_time2 <- select(flights, air_time, arr_time, dep_time)
flights_time3 <- mutate(flights_time2,
                        air_time2 =arr_time - dep_time,
                        air_time_diff = air_time - air_time2)
#they're not the same because the variables are times not numbers. Also the times are local. 
#To fix time issue, convert times to minutes since midnight using same process as step 1.
#Also add a variable for time zone and create new variable to convert all to GMT.


#Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers
#to be related?
#Again, need to convert times to minutes and hours since midnight to calculate the difference.

#10 most delayed flights using ranking function
filter(flights, min_rank(desc(dep_delay))<=10)


1:3 + 1:10
# returns error because you cannot perform function with objects of different sizes
#
#5.6.7
nycflights13::flights
not_cancelled <- nycflights13::flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  count((dest))
#or
not_cancelled %>%
  group_by(dest) %>%
  tally()
#arr_delay is not necessary because if dep_delay is missing, arr_delay will also be missing

h <- nycflights13::flights %>%
  group_by(year, month, day) %>%
  summarize(cancelled = sum(is.na(dep_delay)),
            mean_dep_delay = mean(dep_delay, na.rm = TRUE))
ggplot(data = h) +
        geom_point(
          mapping = aes(x=cancelled, y=mean_dep_delay)
          )
#when dep delays are extreme, cancellations are more likely Shorter dep delays have less impact 

carrier_delays <- nycflights13::flights %>%
  group_by(carrier, dest) %>%
  summarize(cdelay = mean(dep_delay, na.rm = TRUE))
carrier_delays %>%
  summarise(n_car = n_distinct(carrier),
            n_air = n_distinct(dest)
  )

#If you want to sort cases based on count
nycflights13::flights %>%
  count(flight, sort = T)



#5.7.1 Exercises
#Refer back to the lists of useful mutate and filtering functions. 
#Describe how each operation changes when you combine it with grouping.
#You can define characteristics of subgroups within data set rather for entire data set.

flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarize_all(prop_time = sum(arr_delay <= 30)/n(),
            mean_arr = mean(arr_delay, na.rm = TRUE),
            fl = n()) %>%
  arrange(desc(prop_time))

flights %>%
  group_by(hour) %>%
  filter(!is.na(dep_delay)) %>%
  summarize( delay = mean( dep_delay > 0 , na.rm = T)) %>%
  ggplot(aes(hour, delay, fill = delay)) + geom_col() 

#early flights are less likely to be delayed

flights %>%
  group_by(dest) %>%
  filter(!is.na(dep_delay)) %>%
  summarize(mindelay = sum(dep_delay[dep_delay > 0]))

flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(tailnum, dest) %>%
  summarise(hour_perc = mean(dep_delay > 0))
  arrange(desc(hour_perc))
  
?lag
  #???
not_cancelled %>%
  unite (date, month, day) %>%
  group_by(origin, date) %>%
  filter(n() >1) %>%
  arrange(sched_dep_time) %>%
  mutate(delay_lag = lag(dep_delay))

    
flights %>%
  group_by(dest) %>%
  arrange(desc(air_time))

flights %>%
  group_by(dest) %>%
  filter(n_distinct(carrier) > 2) %>%
  group_by(carrier) %>%
  summarise(n <- n_distinct(dest))
  
#For each plane, count the number of flights before the first delay of greater than 1 hour.
not_cancelled%>%
  group_by(tailnum)%>%
  arrange(time_hour) %>%
  mutate(beforebigdelay <- sum(dep_delay < 60))

x <- c(1:10)
range(x)
rng <- range(x)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = FALSE)
  (x - rng[1]) / (rng[2] - rng[1])

  rescale01(c(1, 2, 3, NA, 5))
}
#na.rm=true ignores NA values

# Section 19 ---------------------------------------------------------------

#Read the source code for each of the following three functions, puzzle out what they do, 
#and then brainstorm better names.
#The function is determing if the beginning characters of string are the same as prefix. 
#A better name would be starts_with

#The function is first checking if the length of the vector is greater than 1.
#If that is false, then the function creates a new vector that drops the last case.
#a better name would be drop_last

#This function is replicating the y vector such that the desired length of the output vector
#is the same as the length of the x vector. A better name would be same_length.


#Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
#The arguments are in the same order in both: sample size, mean, variance, so that seems
#consistent. Because the statistics have different names for multivariate, the arguments
#cannot be exactly consistent.


#Take a function that you've written recently and spend 5 minutes brainstorming a better 
#name for it and its arguments.
#I have not written many (any) but for example, the rep function in this exercise would
#be more clear if it were called replicate.

#Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
#Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case
#for the opposite.
#rnorm and dnorm seems to emphasize the distribution whereas norm_r and norm_d seems to
#emphasize the action.


#What's the difference between if and ifelse()? 
#if can be followed by many else clauses.  ifelse can have only one - it tests each object
#in a vector as either true or false.


#Carefully read the help and construct \
#three examples that illustrate the key differences.
x <- c(6:-4)
sqrt(x)  #- gives warning
sqrt(ifelse(x >= 0, x, NA))  # no warning

if(x>0) {
  sqrt(x)
} else {
  NA
}  #gives warning only first element was used.

x <- runif(10)
y1 <- ifelse(x < 0.5, 0, 1)

for (i in 1:length(x)) {
  if (x[i] < 0.5) {
    y2[i] <- 0
  } else {
    y2[i] <- 1
  }} 

x <- c(1:10)
y <- ifelse((x %% 2) == 0, "even", "odd")
print(y)

for(i in 1:10) {
  if((i %% 2) == 0) {
    y[i] <- "even"
  } else {
    y[i] <- "odd"
  }
}
print(y)



#This operation is not possible with an if statement because once the first condition is
#met (true), the function continues to square root the rest of the objects and returns NaN.


#Write a greeting function that says "good morning", "good afternoon", or "good evening", 
#depending on the time of day. (Hint: use a time argument that defaults to lubridate::now(). That will make it easier to test your function.)

library(lubridate)

now_time <- now()
hour(time_of_day)
greeting <- function(time_of_day){
  if (hour(time_of_day) <= 12){
    "Good Morning"
  }
  else if (hour(time_of_day) < 18){
    "Good Afternoon"
  }
  else {
    "Good Evening"
  }
}
 greeting(now_time)

 
#Implement a fizzbuzz function. It takes a single number as input. If the number is 
#divisible by three, it returns "fizz". If it's divisible by five it returns "buzz". 
#If it's divisible by three and five, it returns "fizzbuzz". Otherwise, it returns the 
#number. Make sure you first write working code before you create the function.

fizzbuzz <- function(x){
  if ((x %% 3 == 0) & (x %% 5 == 0)) {
    "Fizzbuzz"
  }
  else if (x %% 3 == 0){
    "Fizz"
  }
  else if (x %% 5 == 0){
    "Buzz"
  }
  else {
    x
  }
}
  
fizzbuzz(15)
fizzbuzz(10)
fizzbuzz(3)
fizzbuzz(4)

#How could you use cut() to simplify this set of nested if-else statements?
x <- -2:40
my_breaks <- c(-Inf, 0, 10, 20, 30, Inf)
my_labels <- c("freezing", "cold", "cool", "warm", "hot")
cut(x, my_breaks, my_labels, include.lowest = TRUE, right = FALSE)
#to include the specified values, change the breaks to -inf, 1, 11, 21, 31, inf. 
#using cut instead of if is fewer steps.

#What happens if you use switch() with numeric values?
switch(1, "apple", "banana", "cantaloupe")
switch(2, "apple", "banana", "cantaloupe")
switch(2, 1,2,3,4)

#What does this switch() call do? What happens if x is "e"?
my_switch <- function(x) {
  switch(x,
         a = ,
         b = "ab",
         c = ,
         d = "cd"
         )
}
(my_switch("a")) #returns the next argument with the non-missing value
#when x is e, it returns NULL
