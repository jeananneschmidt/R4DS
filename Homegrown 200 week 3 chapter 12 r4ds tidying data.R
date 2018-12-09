library(tidyverse)
#install.packages('tidyverse')
library(dplyr)
table1
table2
table4a
table4b
countries <- filter(table2, type == 'cases')$country
countries
years <- filter(table2, type == 'cases')$year
years
cases <- filter(table2, type == 'cases')$count
populations <- filter(table2, type == 'population')$count
table2rate <- tibble(country = countries,
                     year = years,
                     rate = cases/populations * 10000)  
table2rate

(countries <- table4a$country)
(cases_1999 <- table4a$`1999`)
(cases_2000 <- table4a$`2000`)
(populations_1999 <- table4b$`1999`)
(populations_2000 <- table4b$`2000`)

table_1999_rate <- tibble(country = countries,
                          year = 1999,
                          rate = cases_1999 / populations_1999 * 10000)

table_2000_rate <- tibble(country = countries,
                          year = 2000,
                          rate = cases_2000 / populations_2000 * 10000)

table4_rate <- rbind(table_1999_rate, table_2000_rate) %>% arrange(country)

table4_rate

# First get the rows where the type is "cases"
new_table <- filter(table2, type == "cases")

# Then plot
ggplot(new_table, aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))


stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>% 
  spread(year, return) 
  gather("year", "return", `2015`:`2016`)
stocks
# The question is why, after doing a spread() followed by a gather() the result
# isn't exactly the same as the starting point.  This is because the gather()
# maintain the data type.  "Variable names" (e.g. "2015" and "2016") are converted
# to character type whereas in the original they were doubles

# Both  spread()  and  gather()  have a  convert  argument. What does it do?
# When convert = TRUE, it attempts to determine the data type, but you can't
# depend on it
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`, convert = TRUE)

# 2. Why does this code fail?
library(tidyverse)
table4a %>%
  gather(1999, 2000, key = "year", value = "cases")
#need backticks around 1999 and 2000 since the variable names are numeric
#need to use : to indicate we want to select all columns from 1999 to 2000

#3 Spreading this tibble will fail because there are duplicated rows.
#need to add a third column for id so the rows are not duplicates.

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people$id <- c(1, 1, 2, 1, 1)
people %>%
  spread(key = "key", value = "value")

#4
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg %>%
  gather(key = 'gender', value = 'value', 2:3, na.rm = TRUE)

#12.5
#In spread(), all NAs will be replaced one, specified fill argument.
#In complete(), the fill argument takes in a list of values.
#you can specify up or down to indicate what to fill. Default is down.
