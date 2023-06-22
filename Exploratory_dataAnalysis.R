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

#sumarry of height variable dataset
summary(heights$height)

#find the percentile of height$height
p<-seq(0.01, 0.99, 0.01)
percentiles<- quantile(heights$height, p)
percentiles

#to confirm the 25th and 75th percentile matches the 1st and 3rd quartile
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#plotting male height data from the dataset
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

#to create a smooth density plot the male height data
p + geom_density()
p + geom_density(fill = "blue")

#QQ  QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

