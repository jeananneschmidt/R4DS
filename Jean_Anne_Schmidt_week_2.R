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
install.packages(c("nycflights13", "gapminder", "Lahman"))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
nycflights13::flights
(top <- head(nycflights13::flights))
arrival.delay <- filter(nycflights13::flights, arr_delay >= 120)
Houston <- filter(nycflights13::flights, dest=="IAH" | dest=="HOU")
select.carrier <- filter(nycflights13::flights, carrier=="UA" | carrier=="AA" | carrier=="DL")
summer.depart <- filter(nycflights13::flights, month==7 | month==8 | month==9)
arrlate <- filter(nycflights13::flights, arr_delay > 120 & dep_delay <= 0)
deplate <- filter(nycflights13::flights, dep_delay >= 60 & arr_delay <=30)
flight.time <- filter(nycflights13::flights, dep_time >= 0 & dep_time <= 600)
testvar.between <- filter(nycflights13::flights, between(dep_time, 0, 600))
filter(df, is.na(dep_time))
#How many flights have a missing departure time
#why is NA ^ 0 not missing
#How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(nycflights13::flights, desc(dep_delay))
arrange(nycflights13::flights, dep_time)       
arrange(nycflights13::flights, desc(dep_time))
arrange(nycflights13::flights, air_time)
arrange(nycflights13::flights, desc(air_time))
select(nycflights13::flights, year, year)
#one_of matches variable names in a character vector
select(nycflights13::flights, one_of(c("carrier", "month")))
select(nycflights13::flights, contains("TIME"))
#^returns all variables that contaim "time"