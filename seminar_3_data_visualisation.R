install.packages ("ggplot2")

library(ggplot2)
library(tidyverse)

#getting data in R - keep in same folder as RProj

participants_data<-read.csv("participants_data.csv")

?barplot
#height, width=1 - already assigned - chose variable as height - academic parents in this case

# creating a barplot in base R with table() and barplot() function

participants_barplot<-table(participants_data$academic_parents)

barplot(participants_barplot)

# ggplot email response  ####

ggplot(data=participants_data,
       aes(x=letters_in_first_name,y=days_to_email_response)) + 
  geom_point()

# add colour and size - have lots of information at the same time

ggplot(data=participants_data,
       aes(x=letters_in_first_name,y=days_to_email_response, 
           colour=academic_parents,size=working_hours_per_day)) + 
  geom_point()

# ggplot iris data ####

ggplot(data=iris,aes(x=Sepal.Length,
                     y=Petal.Length,
                     color=Species,
                     size=Petal.Width)) +
  geom_point()

# diamonds data ####

dsmall<-top_n(diamonds,n=100)

ggplot(data=dsmall,aes(x=carat,
                       y=price,
                       color=color)) +
  geom_point()