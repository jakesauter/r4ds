# This script will cover the Exploratory Data Analysis (EDA) shown
# in R for Data Science and may go a bit further

# EDA is a cyclical process of 
# 1. Generating questions about data
# 2. Searching for answers in the data using summary stats
#    and visualizations along with models
# 3. Generating more educated questions based on these
#    insights to form even more refined insights 

library(tidyverse)

# Two questions that are often helpful to start with are 
# 1. What type of variation occurs within my variables
# 2. What type of covariation occurs between my variables

# Visualizing distributions 

# What is the distribution of the cut of 
# diamonds in my dataset

diamonds %>% 
  count(cut)

# hmm this is intriguing but maybe it would
# be better to see this information in a graphic
# form, such as a bargraph

ggplot(diamonds) + 
  geom_bar(aes(x = cut))

# Note that we used a bar graph and not a histogram here, 
# bar charts are used for categorical values while histograms
# involve the use of binning continuous variables

ggplot(diamonds) + 
  geom_histogram(aes(x = carat), binwidth = 0.5)

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# If we want to overlay muliple distributions of variables
# we can use frequency polygrams, a histogram though with a point
# at the middle of each bar

ggplot(smaller, aes(x = carat, color = cut)) + 
  geom_freqpoly(binwidth = 0.1)


# Which values are the most common? Why?

# Which values are rare? Why? Should we expect this?

# Are there any unusual patterns? What might explain them?


ggplot(smaller, aes(x = carat)) + 
  geom_histogram(binwidth = 0.01)

# Why are there more diamonds at whole carasts and common
# fractions of carats?

# Why are there more diamonds slightly to the right of each peak
# than there are slight to the left of each peak? 

# What are there no diamonds bigger than 3 carats? 


# Cluster of similar values suggest the existance of 
# subgroups in the data, to understand these subgroups
# ask the following questions: 

# How are the observations within each cluster 
# similar to eachother?

# How are the observations in seperate clusters different 
# from eachother?

# How can the clusters be described?

# Why might the appearance of the clusters be misleading?


ggplot(faithful, aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)


# Unusual values

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5)

# With this binwidth its hard for us to even see 
# the outlier values near 0. To help us see these values 
# we can zoom in using the coord_cartesian() ggplot2 
# coordinate function

ggplot(diamonds) + 
  geom_histogram(aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))

# Note that coord_cartesian() also has an xlim argument, 
# and ggplot2 has seperate xlim() and ylim() functions 
# that work slightly differently, by throwing away values outside
# of the limits 

# Zooming in on the y axis allows us to see three unusual values
# ~0, ~30, and ~60. Lets pluck them out with dplyr

diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>% 
  arrange(y)

# The y variable measures one of the three dimensions of these
# diamonds in mm. Notice that these values are 0 and could 
# not be a possible measurement for these diamonds, incorrect
# values. Also notice that these values are nearly 100 mm, very large
# and their cost is not nearly proportional to the cost they 
# should be for diamonds this size!

# It is good practice to repeat analyses with and without 
# the outliers. If they have minimal effect on the result and
# we cannot figure out why they are there, it is reasonable
# to replace them with missing values (probably removing them). 
# However if they have a substantial effect on the results, we
# should not just drop them without justification and need to 
# figure out what caused them. (data entry error / etc ...)

# If we do encounter these unusual values and don't think that 
# they are valid, we have two options

# 1. Completely remove them 
# 2. Replace the unusual values with missing values 


df <- diamonds %>% 
  filter(between(y, 3, 20))

# This is most likely not the best option as although 
# one measurement is not valid, it does not mean that 
# all of the measurements for the observation are 
# garbage 

# It is best to replace these values with missing values, 
# then to replace them with modified values (probably from 
# interpolation) 

df <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# We can also use dplyr::case_when(), which is particularly 
# useful when we want to create a new variable 

x <- 1:50

case_when(
  x%% 35 == 0 ~ "fizz buzz", 
  x %% 5 == 0 ~ "fizz", 
  x %% 7 == 0 ~ "buzz", 
  TRUE ~ as.character(x)
)

diamonds %>% 
  mutate(color = case_when(
    color == "E" ~ "Eerlily blue", 
    color == "I" ~ "Indefinately black", 
    color == "J" ~ "Joyasley teal", 
    TRUE ~ "All diamonds are clear"
  ))

# Sometimes missing values mean something though, like 
# in the case of missing dep_delay in nycflight13 
# means that the flights were cancelled

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time), 
    sched_hour = sched_dep_time %/% 100, 
    sched_min = sched_dep_time %% 100, 
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(aes(sched_dep_time)) + 
    geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

# This isn't a great way to summarise this data because 
# there are much more non-cancelled flights than 
# cancelled flights, we willcover this later. 


# Covariation of variables

# It is common to want to explore the distribution 
# of a continuous vriable broken down by categorical variable.

ggplot(diamonds, aes(x = price)) + 
  geom_freqpoly(aes(color = cut), binwidth = 500)

# Its hard to see the difference in distribution because 
# the overall counts vary so much

ggplot(diamonds) + 
  geom_bar(aes(x = cut))

# To help in this scenario, we can change the y-axis
# to be density instead of count. Density is count 
# but standardized so that the area under each frequency 
# polygram is one 

ggplot(diamonds, aes(x = price, y = ..density..)) + 
  geom_freqpoly(aes(color = cut), binwidth = 500)

# what are things like ..count.., ..proportion.., and ..density.. called
# and where can I see documentation for them with a list of 
# all possible items like it?

# these special variables are returned from stat functions
# in ggplot, such as stat_bin (called implicilty by 
# geom_histogram)

# these special variables can be found on the ggplot2 
# cheat sheet: 
# https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf

# This plot is a little hard to read so lets take a look 
# at displaying the same data in a box plot

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_boxplot()

# Here we see much less information about 



















































