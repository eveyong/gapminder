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

#creating a tibble is like creating a dataframe
my_tib <- tibble(x=1:3, y=c("blue","pink","yellow")) #shows you the variables when printing
my_df <- data.frame(x=1:3, y=c("blue","pink","yellow"))

my_tib #printing to show difference
my_df #printing to show difference

#converting a dataframe into tibble
as_tibble(my_df)

data("starwars")
starwars 
#this is a tibble. A tibble prints it in a nice way already
#a tibble also shows NAs nicely
as.data.frame(starwars) #converting starwars into dataframe
#the result is a messy chunky result


#difference between tibble and dataframe is also the subset
my_df[,1:2] #original data frame
my_df[,1] #this becomes a vector after subsetting

my_tib[,1:2] #original tibble
my_tib[,1] #this is still a tibble
my_tib[[1]] #extracts the vector x
my_tib[["x"]] #extracts the vector x
my_tib$x #extracts the vector x

#dataframe must be in sequential manner but tibbles does not need to
anotherDF <- data.frame(nItems =c(12,45,107),
                        cost = c(0.5,1.2,1.8),
                        totalworth = nItems*cost)

anothertib <- tibble(nItems =c(12,45,107),
                        cost = c(0.5,1.2,1.8),
                        totalworth = nItems*cost)
anothertib 

#load the gapminder
gapminder_all <- read_csv("1_data/gapminderall.csv")
str(gapminder_all) #have a look at the structure


#combine two columns to form a key (esp. useful for combining datasets using a key)
gapminder_all_gdpcap <- gapminder_all %>%
  mutate(gdp_per_capita = gdp/population) %>%
  select(gdp_per_capita) #that creates a new column and you can select it

#we create a new key with country and year combined
gapminder_all_withkey <- gapminder_all %>%
  mutate (key = paste0(country, "_", year)) #this does not show "key" column though

gapminder_all_withkey$key #just taking out the column we want to see as a vector


#use arrange to arrange data set according to asc or desc based on a column
#removes the need for index
gapminder_all_gdpcap %>% arrange(gdp_per_capita) #this is asc order
gapminder_all_gdpcap %>% arrange(desc(gdp_per_capita))

gapminder_all %>% arrange(fertility)


#summarise is handy, it summarises values of variables
gapminder_all %>% summarise(n_distinct((country))) #finds out how many different countries in the dataset
#this is same as uniqie
length(unique(gapminder_all$country)) #gives the unique number of countries

#mean and std dev of population

gapminder_all %>% filter(!is.na(population)) %>% #to get non NAs
  summarise(mean_pop = mean(population), sd_pop = sd(population))

#put the above into mean_pop
mean_pop <- gapminder_all %>% filter(!is.na(population)) %>% #to get non NAs
  summarise(mean_pop = mean(population), sd_pop = sd(population))

class(mean_pop)

gapminder_all2 <- gapminder_all %>% filter(!is.na(gdp)) %>%
  mutate(gdpovermeanpop = gdp/mean_pop) %>%
  select(gdp,gdpovermeanpop)
#problem is the division does not work because of the wrong class / variable type

#we need to pull or extract a value to convert it
#either .$ works or pull
mean_pop <- gapminder_all %>% filter(!is.na(population)) %>% #to get non NAs
  summarise(mean_pop = mean(population), sd_pop = sd(population)) %>%
  .$mean_pop  #call it a numeric with the dollar sign


mean_pop <- gapminder_all %>% filter(!is.na(population)) %>% #to get non NAs
  summarise(mean_pop = mean(population)) %>%
  pull(mean_pop)  #this makes it numeric

#so after changing mean_pop and making it numeric, we try again with the mutation
gapminder_all3 <- gapminder_all %>% filter(!is.na(gdp)) %>% #to get non NAs
  mutate(gdpovermeanpop = gdp/mean_pop) %>%
  select(gdp, gdpovermeanpop)
  
gapminder_all3 #this is correct dividing after the pull or .$


#group by

gapminder_all %>% group_by(continent) %>% filter(!is.na(gdp)) %>%
  summarise(average = mean(gdp))


#distinct shows all distinct values and take out duplicates

distinct(gapminder_all)
#we end up with the same number of rows so that means our dataset was already all distinct

#how to find duplicates? use duplicated function
duplicated(gapminder_all_withkey$key) #key with country and year from before
#all of them look false so far

#can also check with index
gapminder_all_withkey$key[duplicated(gapminder_all_withkey$key)]  #shows 0 means no duplicate within key

#country has duplicates though
duplicated(gapminder_all_withkey$country)
table(gapminder_all_withkey$country[duplicated(gapminder_all_withkey$country)]) #to see how many duplicate for each country there are


#================================================
### JOINS ###

library(dplyr) #has join functions
library(readr) #can import data
library(gapminder) #for the gapminder dataset
library(tibble) #to see results in new window
data(gapminder)

gapminder_all <- read_csv("1_data/gapminderall.csv")
gapminder <- read.csv("1_data/gapminder.csv")

#mutating joins add columns from one table to another with matching key

gapminder_withkey <- gapminder %>% 
  mutate (key = paste0(country, "_", year)) #this will be left table
gapminder_allwithkey <- gapminder_all %>% 
  mutate (key = paste0(country, "_", year)) #this will be right table

#let us do a left join
gapminderleft <- left_join(gapminder_withkey, gapminder_allwithkey, by="key")
gapminderleft
view(gapminderleft)

#right join
gapminderright <- right_join(gapminder_withkey, gapminder_allwithkey, by="key")
gapminderright
view(gapminderright) #you can see NAs in the left table because not all observations of the right table matches the left table
miss_var_summary(gapminderright) 

#inner join only matches those that are common for both
gapminderinner <- inner_join(gapminder_withkey, gapminder_allwithkey, by="key")
view(gapminderinner)

#full join joins everything
gapminderfull <- full_join(gapminder_withkey, gapminder_allwithkey, by="key")
gapminderfull
view(gapminderfull)

# filtering joins must have key and reference dataset

gapmindersemi <- semi_join(gapminder_withkey, gapminder_allwithkey, by="key")
view(gapmindersemi) #this only keeps the columns from X table

# anti join is the opposite, it takes the other observations that were not in X

gapminderanti <- anti_join(gapminder_withkey, gapminder_allwithkey, by="key")
view(gapminderanti)



#=========== quiz
#Q10
length(unique(gapminder_all$country)) 

#Q11 last year of data available in gapminder
gapminder_all %>% arrange(desc(year))
tail(gapminder_all)

#Q12 For the CAS dataset freMTPL2freq.csv, 
# calculate the proportion of policies for each vehicle brand.  Which vehicle brand has the highest proportion 
# and what is the proportion to 3 decimal places?     

CASdata <- read_csv("1_data/freMTPL2freq.csv")     
summary(CASdata)
head(CASdata)

length(unique(CASdata$VehBrand)) #there are 11 brands
prop <- prop.table(table(CASdata$VehBrand))  #to get proportions
view(prop) #and then arrange it by frequency in desc order

# Q13 what is the proportion of policies coming from the Area "F" to 4 decimal places?

prop.table(table(CASdata$Area)) 

# Q14 For the dataset creditcardcut.csv, 
# what is the proportion of fraudulent transactions? 

credit <- read_csv("1_data/creditcardcut.csv")
head(credit)
summary(credit)

prop.table(table(credit$Class)) 
# Class = response variable i.e. value 1 denotes fraud and 0 denotes no fraud


# Q15 What is the correlation of Pulse Rate 1 and 
# Height to 3 decimal places (ignoring missing values)

pulserate_url <- "http://www.statsci.org/data/oz/ms212.txt"
pulserate <- read_tsv(pulserate_url)
head(pulserate)

cor(pulserate[,1:11], use="pairwise.complete.obs") #so this correlation tells it to ignore the NAs

# Q17 What is the mean change in pulse rate 
# (pulse rate 2 minus pulse rate 1) for participants who ran?

diffcalc<- pulserate %>% filter(pulserate$Ran == "1") %>%
  mutate(diff = Pulse2-Pulse1) %>% #create new column to calculate the difference
  .$diff  #make numeric variable

pulserate_diff <- pulserate %>% filter(pulserate$Ran == "1") %>%
  summarise(mean(diffcalc, na.rm = TRUE)) #summarise the mean of diffcalc variable

pulserate_diff #to see the value of the mean

# Q18 As above but for those who sat
diffcalc2<- pulserate %>% filter(pulserate$Ran == "2") %>%
  mutate(diff = Pulse2-Pulse1) %>% #create new column to calculate the difference
  .$diff  #make numeric variable

pulserate_diff2 <- pulserate %>% filter(pulserate$Ran == "2") %>%
  summarise(mean(diffcalc2, na.rm = TRUE)) #summarise the mean of diffcalc variable

pulserate_diff2 #to see the value of the mean




#================================================
### MISSING VALUES WEEK 4 ===
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

#========================================================
### outliers ###

##capping the outlier

x <- c(1,2,3,100)
