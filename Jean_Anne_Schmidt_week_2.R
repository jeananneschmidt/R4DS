r_rocks <- 2^3
seq()
seq(1, 10)
(y <- seq(1, 10))
(y <- seq(1, 10, length.out = 5))
(my_variable <- 10)
library (tidyverse)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
(filter(mpg, cyl == 8))
(filter(diamonds, carat > 3))
#install.packages(c("nycflights13", "gapminder", "Lahman"))
library(nycflights13)
library(dplyr)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
nycflights13::flights
(top <- head(nycflights13::flights))
arrival.delay <- filter(nycflights13::flights, arr_delay >= 120)
arrival.delay <- filter(flights, arr_delay >= 120)
Houston <- filter(flights, dest=="IAH" | dest=="HOU")
select.carrier <- filter(flights, carrier=="UA" | carrier=="AA" | carrier=="DL")
summer.depart <- filter(flights, month==7 | month==8 | month==9)
arrlate <- filter(flights, arr_delay > 120 & dep_delay <= 0)
deplate <- filter(flights, dep_delay >= 60 & arr_delay < 30)
flight.time <- filter(flights, dep_time >= 0 & dep_time <= 600)
testvar.between <- filter(flights, between(dep_time, 0, 600))
filter(flights, is.na(dep_time))


#How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(flights, desc(is.na(dep_delay)))
arrange(flights, desc((dep_delay)))
arrange(flights, dep_time)       
arrange(flights, desc(dep_time))
arrange(flights, air_time)
longest <- arrange(flights, desc(distance))
shortest <- arrange(flights, distance)

select(nycflights13::flights, year, year)
# it only uses the variable once

#one_of matches variable names in a character vector
select(flights, one_of(c("carrier", "month")))
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))
#^returns all variables that contain "time" You can change the default to ignore case
