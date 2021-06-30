# load libraries ####

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(magrittr)


# ?diamond

# save Cory participants data as participants_data in learning_r.Rproj directory
## keep your data in the same folder structure as .Pproj
## at or below the level of RProj

# getting data in R ####

# assign data

participants_data<-read.csv("participants_data.csv")

# learn about function by ?read.csv

?read.csv

# looking at the data ####

##  view data in the console using the view function

view(participants_data)

## look at top row using the head function

head(participants_data)

### default of function is to show 6 rows.Change via n argument

### change the number of rows displayer to 7
### head(participants_data,n=7)

head(participants_data,n=7)

## check the names of the variable in the data with names function

names(participants_data)

## look at the structure of the data with str function

str(participants_data)

## call a particular variable in your data with $
### change the variable to gender via participants_data$gender

participants_data$gender

# load dplyr, tidyr, magrittr library

library(dplyr)
library(tidyr)
library(magrittr)

# wrangling dplyr arguments ####

## select
###create a subset of the data with the select function
### change the selection to batch and age
select(participants_data,batch,age)

### select without batch and age

select(participants_data,-batch,-age)

##filter
### change selection to those who work more than 5 hours a day

filter(participants_data,working_hours_per_day>5)

### change filter to those who work more than 5 hours a day and names are longer
### than 3 letters

filter(participants_data,working_hours_per_day>5 & letters_in_first_name>3)

##rename
### change the names of the variable in the data with the rename function
### rename the variable km_home_to_office as commute

rename(participants_data,commute = km_home_to_office)

##mutate
###mutate a new column named age_mean that is a function of the age multiplied
###by the mean of all ages in the group

mutate(participants_data,age_mean=age*mean(age))

##create a communte category with the mutate function
###mutate a new column named ifelse populated by 'commuters' if it takes more 
###than 10km to work, and 'local' for others

mutate(participants_data,commute=ifelse(km_home_to_office>10,
                                        "commuter","local"))


#summary
##get a summary of selected variables with summarize
###create a summary of the participants_mutate data with the mean number of 
###years of study and the median of letters in the first name

summarize(participants_data,mean(years_of_study),median(letters_on_first_name))


# wrangling. magrittr use ####


## pipeline %>%
###do all the previous with a magrittr pipeline %>%. Use the group_by function
###to get these results for comparison between groups
#### use the magrittr pipe to summarize the mean working_hour_per_day
#### the median letters in first name and maximum years of study by gender

participants_data%>%summarise(mean(working_hours_per_day),
                              median(letters_in_first_name),
                              max(years_of_study))
##group by function
###use the mutate function to subset the data and use the group_by function to 
###get these resulst for comparisons between groups
###use the magrtittr pipe to create a new column called commute, where those who
###travel more than 10km to get to the office are called "commuter" and others
###are "local". Summarize the mean age, median years of 
###study and maximum letter in first name

participants_data%>%mutate(mutate=ifelse
                           (km_home_to_office>10,"commuter","local")
)%>%group_by(commute)%>%
  summarize(mean(age),median(years_of_study),
            max(letters_in_first_name))


#purrr: Apply a function to each element of a vector ####
##we will use the purrr library to run a simple linear regression.Note that when 
##using base R functions with the magrtittr pipeline we use ',' to refer to data.
##The functions split and lm are from R and stats

# load library ####

library(purrr)

##use purr to solve: split a data frame into pieces, fit a model to each peace,
##compute the summary, then extract the R^2

###see last point data wrangling
