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


### DATA CLEANING ###
gapminder_all <- read_csv("1_data/gapminderall.csv")
install.packages("naniar")
install.packages("simputation")
library(naniar)
library(simputation)

#these tell us missing variables
miss_var_summary(gapminder_all)
miss_case_summary(gapminder_all)

#we can drop these missing rows
gapminder_naomit <- na.omit(gapminder_all) #omit all rows that have NAs in it
gapminder_naomit #number of rows dropped from 10k to 7k

#or we can make these NAs zero
gapminder_zeroNA <- gapminder_all %>%
  mutate(infant_mortality = ifelse(is.na(infant_mortality),0, infant_mortality),
         gdp = ifelse(is.na(gdp), 0, gdp),
         fertility = ifelse(is.na(fertility), 0, fertility),
         population = ifelse(is.na(population),0, population))
gapminder_zeroNA #have a look at the modified data
#we end up with 10,545 rows, same as before because we did not drop any rows with missing values
miss_var_summary(gapminder_zeroNA) #to check if there are still missing variables
#we see 0s for all so no more missing values


#we can also use the mean and median for the missing values instead of 0
#we do the same as 0 but replace with mean() function. 
#but because there are NAs, when we do the mean, there'll be NAs
#to remove the NAs inside the mean, use mean(gdp,na.rm=TRUE) to make sure those with NAs are not used in the mean calculation
gapminder_meanNA <- gapminder_all %>%
    mutate(infant_mortality = ifelse(is.na(infant_mortality), mean(infant_mortality,na.rm=TRUE), infant_mortality),
         gdp = ifelse(is.na(gdp), mean(gdp, na.rm=TRUE), gdp),
         fertility = ifelse(is.na(fertility), mean(fertility, na.rm=TRUE), fertility),
         population = ifelse(is.na(population), mean(population, na.rm=TRUE), population))

gapminder_meanNA
miss_var_summary(gapminder_meanNA)

#the same logic works for median
gapminder_medianNA <- gapminder_all %>%
  mutate(infant_mortality = ifelse(is.na(infant_mortality), median(infant_mortality,na.rm=TRUE), infant_mortality),
         gdp = ifelse(is.na(gdp), median(gdp, na.rm=TRUE), gdp),
         fertility = ifelse(is.na(fertility), median(fertility, na.rm=TRUE), fertility),
         population = ifelse(is.na(population), median(population, na.rm=TRUE), population))

gapminder_medianNA
miss_var_summary(gapminder_medianNA)

#for a less chunky code, we could use naniar or simputation
gapminder_zeroreplace1 <- gapminder_all %>%
  bind_shadow(only_miss = TRUE) %>% #gives an extra column and tells us when the variables are NA or not 
  add_label_shadow() %>% #the new columns will have the same names but with additional label at end
  impute_const(fertility~0) %>% #replace anything that is NA with 0
  impute_const(population~0) %>%
  impute_const(infant_mortality~0) %>%
  impute_const(gdp~0)
gapminder_zeroreplace1
miss_var_summary(gapminder_zeroreplace1) #all values are zero

#now we select the ones with the _NA to see what it looks like
gapminder_zeroreplace1 %>% select(infant_mortality_NA, fertility_NA, population_NA, gdp_NA, any_missing)

#we can also try replacing all NAs with zero directly with base R
gapminder_zeroreplace2 <- gapminder_all
gapminder_zeroreplace2[is.na(gapminder_zeroreplace2)]<- 0
gapminder_zeroreplace2
miss_var_summary(gapminder_zeroreplace2) #problem with this is there is no tracking


#imputing all with mean using naniar
gapminder_meanreplace <- gapminder_all %>%
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_mean_all() 
miss_var_summary(gapminder_meanreplace)


#imputing with median using naniar
gapminder_medianreplace <- gapminder_all %>%
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_median_all()
miss_var_summary(gapminder_medianreplace)


#we can also use a mix, some with medians, some with means
gapminder_mixreplace <- gapminder_all %>%
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_const(fertility~5) %>%
  impute_proxy(population~mean(population,na.rm=TRUE)) %>% #there needs to be a calculation in proxy
  impute_proxy(infant_mortality~median(infant_mortality,na.rm=TRUE)) %>%
  impute_proxy(gdp~mean(gdp,na.rm=TRUE))
miss_var_summary(gapminder_mixreplace)

#we can deal with missing values via imputation e.g. using linear regression
gapminder_lm <- gapminder_all %>%
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_lm(fertility~life_expectancy) %>%
  impute_lm(population~life_expectancy + fertility + year) %>%
  impute_lm(infant_mortality~life_expectancy + fertility) %>%
  impute_lm(gdp~life_expectancy+year)
miss_var_summary(gapminder_lm)

#naniar is simple with impute_median_all and with simputation, there is always a relationship using ~

