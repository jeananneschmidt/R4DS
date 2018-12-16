library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
install.packages("maps")
library(maps)
#13.4.6
#1
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
avg_dest_delays <-
  flights %>%
  group_by(dest) %>%
  # arrival delay NA's are cancelled flights
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))
avg_dest_delays %>%
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
#2
airport_locations <- airports %>%
  select(faa, lat, lon)
flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(
    airport_locations,
    by = c("origin" = "faa")
  ) %>%
  left_join(
    airport_locations,
    by = c("dest" = "faa"))

#3
plane_ages <- 
  planes %>%
  mutate(age = 2018 - year) %>%
  select(tailnum, age)
flights %>%
  inner_join(plane_ages, by = "tailnum") %>%
  group_by(age) %>%
  filter(!is.na(dep_delay)) %>%
  summarise(delay = mean(dep_delay)) %>%
  ggplot(aes(x = age, y = delay)) +
  geom_point() +
  geom_line()

#4
flight_weather <-
  flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour"))
flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()

#13.5.1
#1
flights %>%
  filter(is.na(tailnum))
#flights with a missing tailnum also have missing dep_time, 
#so they were cancelled flights

#2
flights_100 <-
filter(flights) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n > 100)
flights_100
flights %>%
  semi_join(flights_100, by = 'tailnum')

#3
install.packages("fueleconomy")
fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))

#4
flights48 <-
  filter(flights) %>%
  group_by(year, month, day) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  unite(date, year, month, day, sep = '-') %>%
  mutate(date = parse_date(date, "%Y-%m-%d")) %>%
  gather(key = 'mode', value = 'delay', 2:3) %>%
  mutate(mode = factor(mode, labels = c('Average arrival delay',
                                        'Average departure delay')))
weather48 <-
  filter(weather) %>%
  group_by(year, month, day) %>%
  summarize(avg_wind_speed = mean(wind_speed, na.rm = TRUE),
          avg_wind_gust = mean(wind_gust, na.rm = TRUE),
          avg_precip = mean(precip, na.rm = TRUE),
          avg_visib = mean(visib, na.rm = TRUE)) %>%
  unite(date, year, month, day, sep = '-') %>%
  mutate(date = parse_date(date, "%Y-%m-%d"))

#5
#anti_join(flights, airports, by = c("dest" = "faa"))
#Returns flights with no matching airport
#i_join(airports, flights, by = c("faa" = "dest"))
#Returns airports with no matching flights (from NYC)

#6
multi_carrier_planes <-
  flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier) %>%
  count(tailnum) %>%
  filter(n > 1)
multi_carrier_planes
multi_carrier_planes <-
  flights %>%
  semi_join(multi_carrier_planes, by = "tailnum") %>%
  select(tailnum, carrier) %>%
  distinct() %>%
  arrange(tailnum)
multi_carrier_planes


#Chapter 15
library(forcats)
gss_cat
gss_cat %>%
  count(race)
ggplot(gss_cat, aes(race)) +
  geom_bar()
gss_cat %>%
  count(rincome)
ggplot(gss_cat, aes(rincome)) +
  geom_bar()

#2
gss_cat %>%
  count(relig) %>%
  arrange(desc(n))
gss_cat %>%
  count(partyid) %>%
  arrange(desc(n))
#3
gss_cat %>%
  group_by(denom) %>%
  count (relig)

gss_cat %>%
 count(relig, denom) %>%
 ggplot(aes(x = relig, y = denom, size = n)) +
 geom_point() +
 theme(axis.text.x = element_text(angle = 90))

#15.4
relig_summary <- gss_cat %>%
  group_by(relig)%>%
  summarise(age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE))
ggplot(relig_summary, aes(tvhours, relig)) + geom_point()
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE))

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

summary(gss_cat[["tvhours"]])
gss_cat %>%
  filter(!is.na(tvhours)) %>%
  ggplot(aes(x = tvhours)) +
  geom_histogram(binwidth = 1)

#2
gss_cat %>%
  ggplot(aes(x = marital)) +
  geom_bar()
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

gss_cat %>%
  ggplot(aes(x = race)) +
  geom_bar()
#categories are already in order by frequency

gss_cat %>%
  ggplot(aes(x = rincome)) +
  geom_bar()
rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE))
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()

gss_cat %>%
  ggplot(aes(x = relig)) +
  geom_bar()
gss_cat %>%
  mutate(relig = relig %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(relig)) +
  geom_bar() +
  coord_flip()
gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_flip()

#3
#Why did moving "Not applicable" to the front of the levels move it to the bottom of the plot?
#Because that gives the level "Not applicable" an integer value of 1.