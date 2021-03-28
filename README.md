# gapminder
Week 3 of learning

#install packages and libraries to import and tidy data
install.packages("gapminder")
install.packages("tidyverse")
install.packages("plyr")

#load libraries
library(gapminder)
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr) #for the mutate

#read in the different gapminder data sets
data(gapminder)
gapminder_csv <- read_csv("1_data/gapminder.csv")
gapminder_wide_csv <- read_csv("1_data/gapminder_wide.csv")

gapminder_url <- "https://raw.githubusercontent.com/jennybc/gapminder/master/inst/extdata/gapminder.tsv"
gapminder_tsv <- read_tsv(gapminder_url)

#show the read in for the different readins are
head(gapminder_csv) #the results show tidy form
head(gapminder_wide_csv) #doesn't seem like tidy form because it has two variables in one, it is not 1 observation per row either
head(gapminder) #one variable per column, each observation forms a row, ultimately forms a table
head(gapminder_tsv) #as above, like a table

#need to reshape gapminder_wide to get into tidy form
gap_long <- gapminder_wide_csv %>%
  gather(key, value, -continent, -country) %>%
  separate(key,c("variable_name", "year"), "_") %>%
  spread(variable_name,value) %>%
  mutate(year=as.numeric(year)) #year should be numeric, not character


