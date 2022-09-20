# dplyr/tidyverse homework
library(tidyverse)

# Outline
# 1. filter
# 2. mutate
# 3. summarize
# 4. group_by
# 5. pipes
# 6. arrange/select/count
# 7. joins


# 1. filter
# filter iris data by each species 
setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

# 2. mutate
# create 2 new columns in iris data that is petal/sepal length x width
iris_area <- mutate(
  iris, 
  petal.area = Petal.Length * Petal.Width,
  sepal.area = Sepal.Length * Sepal.Width
)

# plot petal area ~ length - is the relationship linear? Why?
ggplot(data = iris_area, aes(x = Petal.Length, y = petal.area)) + geom_point()

# 3. summarize
# compute the mean petal length of each species dataset from above
setosa_mean <- mean(setosa$Petal.Length)
versicolor_mean <- mean(versicolor$Petal.Length)
virginica_mean <- mean(virginica$Petal.Length)

# now do it using summarize
setosa_summarize_mean <- summarize(setosa, mean.petal.length = mean(Petal.Length))
versicolor_summarize_mean <- summarize(versicolor, mean.petal.length = mean(Petal.Length))
virginica_summarize_mean <- summarize(virginica, mean.petal.length = mean(Petal.Length))

# 4. group by
# we can do the above summarize so much easier when combined with group_by
iris_means <- summarize(group_by(iris, Species), mn.petal.length = mean(Petal.Length))

# 5. pipes
# the above can get unwieldy - rearrange iris_means from 4 using pipes
iris_means <- iris %>% 
  group_by(Species) %>% 
  summarize(mn.petal.length = mean(Petal.Length))


## On Your Own #1 
# now compute mean petal area for each species - how would you go about it using dplyr
iris_mean_area <- iris_area %>% 
  group_by(Species) %>% 
  summarize(mn.petal.area = mean(petal.area))

# Q: What is the mean petal area for each species
# Species    mn.petal.area
# setosa             0.366
# versicolor         5.72 
# virginica         11.3  

# 6. arrange/select/count
# determine which species has the longest mean petal length
iris_size <- 
  iris %>% 
  select(Species, Petal.Length) %>% # only selects these 2 columns
  group_by(Species) %>%
  summarize(mn.petal.length = mean(Petal.Length)) %>%
  arrange(desc(mn.petal.length))

# On Your Own #2
# do the same for the other measurements (i.e. petal.width, sepal.length, etc)
# Q: What is the mean petal and sepal lengths and widths for each species
iris_mean_measurements <- iris %>% 
  # Tidy-er data format by pivoting
  pivot_longer(cols = -Species, names_to = 'Measurement', values_to = 'Value') %>% 
  # Find means for each Species first
  group_by(Measurement, Species) %>% 
  summarize(mean_value = mean(Value))

# Measurement  Species    mean_value
# Petal.Length setosa          1.46 
# Petal.Length versicolor      4.26 
# Petal.Length virginica       5.55 
# Petal.Width  setosa          0.246
# Petal.Width  versicolor      1.33 
# Petal.Width  virginica       2.03 
# Sepal.Length setosa          5.01 
# Sepal.Length versicolor      5.94 
# Sepal.Length virginica       6.59 
# Sepal.Width  setosa          3.43 
# Sepal.Width  versicolor      2.77 
# Sepal.Width  virginica       2.97 

# Now determine which species has the biggest mean
iris_mean_measurements %>%
  group_by(Measurement) %>% 
  summarize(
    biggest_value = max(mean_value),
    biggest_value_row = which.max(mean_value), 
    biggest_value_species = Species[which.max(mean_value)]) %>% 
  select(Measurement, biggest_value_species, biggest_value)

# Measurement  biggest_value_species biggest_value
# Petal.Length virginica                      5.55
# Petal.Width  virginica                      2.03
# Sepal.Length virginica                      6.59
# Sepal.Width  setosa                         3.43

# count the number of records for each species
(iris_spp_n <- count(iris, Species))

# On Your Own #3
# count the number of samples that are >= mean.petal.length for each species
mean_petal_lengths <- iris %>% 
  group_by(Species) %>% 
  summarize(mean_petal_length = mean(Petal.Length))
iris %>% 
  select(Species, Petal.Length) %>% 
  left_join(mean_petal_lengths) %>% 
  filter(Petal.Length >= mean_petal_length) %>% 
  group_by(Species) %>% 
  tally()

# Q: How many samples where Petal.Length >= mean.petal.length does each species have
# Species        n
# setosa        26
# versicolor    27
# virginica     25

# 7. joins
set.seed(123)
ht <- data.frame(level = LETTERS[1:5], height = sample(40:80, 5, replace = TRUE))
wt <- data.frame(level = LETTERS[1:6], weight = sample(50:500, 6, replace = TRUE))

# join together height and weight by level
# what happens when you reverse ht and wt (i.e. put wt first, then ht)
ht_wt <- left_join(ht, wt, by = "level")

# On Your Own #4 - Extra Credit
# work with the nycflights13 data set to determine what airport had the 
#     most departing flights in 2013
# must use combination of dplyr verbs
# data.frames are airports, flights, planes and weather
# HINT: faa column in airports links to origin column in flights
library(nycflights13)

# Q: Which airport (name) had the greatest number of arriving flights in 2013?
# Atlanta (ATL) had the greatest number of arriving flights in 2013
flights %>% 
  filter(!is.na(arr_time)) %>% 
  left_join(select(airports, faa, name), by = c("dest" = "faa")) %>% 
  group_by(name, dest) %>%
  tally() %>% 
  arrange(desc(n))

# If we don't take into account flights with NA for arrival time, then
# Chicago O'Hare has the greatest number
flights %>%  
  left_join(select(airports, faa, name), by = c("dest" = "faa")) %>% 
  group_by(name, dest) %>%
  tally() %>% 
  arrange(desc(n))

# Q: Which airport (name) had the greatest number of delayed arriving flights?
# Also Atlanta (ATL) here - has the greatest number of delayed flights
flights %>% 
  filter(!is.na(arr_delay)) %>% 
  left_join(select(airports, faa, name), by = c("dest" = "faa")) %>% 
  group_by(name, dest) %>%
  tally() %>% 
  arrange(desc(n))

# Q: What is the manufacturer, model, year, and type of airplane that flew the 
#    most flights in 2013 (only include planes with all relevant information)?
#    How man flights was it?
most_flights <- flights %>%  
  filter(!is.na(tailnum)) %>% 
  group_by(tailnum) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  head(10)
planes %>% 
  semi_join(most_flights) %>% 
  left_join(most_flights) %>% 
  arrange(desc(n)) %>% 
  head(1) %>% 
  select(tailnum, year, type, manufacturer, model, n_flights = n)

# The most flights 
# tailnum  year type                    manufacturer         model  n_flights
#   1 N711MQ   1976 Fixed wing multi engine GULFSTREAM AEROSPACE G1159B       486
