library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
head(gapminder)
##  a scatter plot of life expectancy versus fertility for the African continent in 2012.
gapminder %>% filter(year == 2012 & continent == "Africa" ) %>%
  ggplot(aes(fertility, life_expectancy )) +
  geom_point()

#color to distinguish the different regions of Africa
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
  ggplot(aes(fertility, life_expectancy, color =region)) +
  geom_point()

#a table showing the country and region for the African countries that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
df <- gapminder %>% filter(continent == "Africa" & year == 2012 & fertility <= 3 & life_expectancy >= 70 ) %>% 
  select(country, region)
df

#create a table with data for the years from 1960 to 2010 in Vietnam and the United States.
countries <- c("Vietnam", "United States")
years <- c(1960:2010)
tab <- gapminder %>% filter(year %in% years &  country %in% countries)
tab

#to plot life expectancy vs year for Vietnam and the United States and save the plot as p
p <- tab %>% ggplot(aes(year, life_expectancy, color = country))+
  geom_line()
p

#a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
gapminder %>% filter(year %in% c(1960:2010) & country == "Cambodia") %>%
  ggplot(aes(year, life_expectancy))+ geom_line()

#to calculate and plot dollars per day for African countries in 2010 using GDP data.
daydollars <- gapminder %>% filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365)

daydollars
#a smooth density plot of dollars per day from daydollars
daydollars %>% ggplot(aes(dollars_per_day))+
  scale_x_continuous(trans = "log2")+
  geom_density()
##the dollars_per_day variable but for African countries in the years 1970 and 2010 this time.
years_africa<- c(1970, 2010)
daydollar_1 <- gapminder %>% filter(year %in% years_africa & continent == "Africa" & !is.na(gdp)) %>%
  mutate(dollars_per_day = gdp/population/365)

daydollar_1

daydollar_1 %>% ggplot(aes(dollars_per_day))+
  scale_x_continuous(trans = "log2")+
  geom_density()+ facet_grid(year ~ .)

### edit the code to show a stacked density plot of each region in Africa.
daydollar_1 %>% ggplot(aes(dollars_per_day,  fill = region))+
  scale_x_continuous(trans = "log2")+
  geom_density(alpha = 0.2, bw = 0.5, position = "stack")+ facet_grid(year ~ .)


###To continue looking at patterns in the gapminder dataset by plotting infant mortality rates versus dollars per day for African countries.
gapminder_Africa_2010 <- gapminder %>% 
  filter(year == 2010 & continent =="Africa" & !is.na(gdp)) %>% 
  mutate(dollars_per_day = gdp/population/365)
gapminder_Africa_2010

#scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.

gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region))+
  geom_point()

##Transform the x axis to be in the log (base 2) scale.
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region))+
  scale_x_continuous(trans = "log2")+
  geom_point()

## the plot with country names instead of points so we can identify which countries are which.

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text(vjust = 0, nudge_y = 0.5) + 
  scale_x_continuous(trans = "log2")
##Infant mortality scatter plot - part 4 - comparison of scatter plots
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day) & !is.na(infant_mortality)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)
