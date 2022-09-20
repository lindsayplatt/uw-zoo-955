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

## On Your Own #1 
# now compute mean petal area for each species - how would you go about it using dplyr
# Q: What is the mean petal area for each species

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

# count the number of records for each species
(iris_spp_n <- count(iris, Species))

# On Your Own #3
# count the number of samples that are >= mean.petal.length for each species
# Q: How many samples where Petal.Length >= mean.petal.length does each species have

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
# Q: Which airport (name) had the greatest number of delayed arriving flights?
# Q: What is the manufacturer, model, year, and type of airplane that flew the 
#    most flights in 2013 (only include planes with all relevant information)?
#    How man flights was it?