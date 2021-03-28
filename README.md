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


#install and load libraries for cleaning data
install.packages("janitor")
install.packages("psych")
library(janitor) #to use clean function
library(psych) 


#summarise the data
str(gapminder)
class(gapminder)

#ideal variable names should have no spaces, no capitals for the names

gapminder_adjnames <- gapminder
names(gapminder_adjnames) <- c("country", "continent", "year", "Life Exp", "population", "Gdp Per cap")
gapminder_adjnames #to preview the adjusted columns

make.names(names(gapminder_adjnames)) #the spaces in the column name had full stops
gapminder_clean <- clean_names(gapminder_adjnames) #replaces the dots with underscore
names(gapminder_clean) #lets you look at the column names

#can also specifically call out which columns to rename
gapminder_clean2 <- rename(gapminder_adjnames, life_exp = "Life Exp", gdp_per_cap = "Gdp Per cap")
names(gapminder_clean2)


table(gapminder$continent) #like a frequency distribution
prop.table(table(gapminder$continent)) #get the proportion of continent out of all continents
summary(gapminder) #provide summary stats of data set
describe(gapminder) #besides boxplot info, it also gives skewness and kurtosis. 
#the variable in question is life expectancy, so distribution is important.
#can start thinking about whether linear regression is worth solving our problem
#before moving onto GLMs
#linear regression has a normality assumption
#population and GDP is quite skewed in this case because their skew number is >1

#correlation matrix
cor(gapminder[,3:6]) #picked only columns 3:6 because they are numerical
#normally correlation does not work if there are blanks or #NAs in the values
cor(gapminder[,3:6], use="pairwise.complete.obs") #so this correlation tells it to ignore the NAs

#other functions to summarise
nrow(gapminder) #useful when trying to split dataset into train and test dataset
ncol(gapminder)
tail(gapminder) #gives last 6 values, opposite of head
head(gapminder)
unique(gapminder)

table(gapminder$continent)/12 #summary shows that the country appears 12 times each year so need to divide by 12 to find out how many times it actually appeared
 