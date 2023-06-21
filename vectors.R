#name <- c("Mandi", "Amy", "Nicole", "Olivia")
#distance <- c(0.8, 3.1, 2.8, 4.0)
#time <- c(10, 30, 40, 50)

#time_hours <- time/60
#time_hours
#speed <- distance/time_hours
#speed
library(dplyr)
library(dslabs)
data(heights)
options(digits =3)
average<- mean(heights$height)
#ind<- heights %>% filter(height > average, na.rm=TRUE)
#First, determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height.
ind<- heights$height>average
sum(ind)
# how many of them are females
ind<-heights$height>average & heights$sex =="Female"

#What proportion of individuals in the dataset are female
ind1<- heights$sex =="Female"
mean(ind1, na.rm = TRUE)

#minimum heights in the height dataset
min_height<- min(heights$height)
min_height

#To determine the index of the first individual with the minimum height.
first_index<- match(min_height, heights$height)
first_index
#Subset the sex column of the dataset by the index in above to determine the individual's sex.
first_Sex<- heights$sex[first_index]
first_Sex
#max heights in the height dataset
max_height<- max(heights$height)
max_height

#to create a vector x that includes the integers between the minimum and maximum heights in this dataset (as numbers).
x<- min_height:max_height
x
#How many of the integers in x are NOT heights in the dataset
sum(!(x %in% heights$height))
#Using the heights dataset, create a new column of heights in centimeters named ht_cm. Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2

heights<- mutate(heights, ht_cm = heights$height*2.54)
heights$ht_cm[18]
#mean height in cn
mean(heights$ht_cm, na.rm=TRUE)
# new datframe contaimig only female observation
heights2 <-data.frame(heights %>% filter(heights$sex =="Female"))
 mean(heights2$ht_cm)
