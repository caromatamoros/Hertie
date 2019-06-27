source("../packages.r")
library(nycflights13)
nrow(flights)
View(flights)
summary(flights)
str(flights)
dim(flights)

flights_1<- filter(flights, day == 1)
flights_1JFK<- filter(flights_1, origin == "JFK")
flights_1LGA<- filter(flights_1, origin == "LGA")
flights_NY <-bind_rows(flights_1JFK,flights_1LGA)
View(flights_NY)
str(flights_NY)
dim(flights_NY)

flights_NY1 <- arrange(flights_NY, arr_time) 
flights_SORT <- arrange(flights, arr_time) # For the all the flights
View(flights_NY1)


flights_2013<- filter(flights, year == 2013)
flights_JFK13<- filter(flights_2013, origin == "JFK")
flights_LGA13<- filter(flights_2013, origin == "LGA")
flights_NY_2013<-bind_rows(flights_JFK13,flights_LGA13)
head(flights_NY_2013)
str(flights_NY_2013)
dim(flights_NY_2013)

flights_delay <- mutate(flights, delay = dep_delay - arr_delay)

flights_delay_bycarrier <- group_by(flights_delay, carrier)
class(flights_delay_bycarrier)

summarize(flights_delay_bycarrier, n_deps = n())
summarize(flights_delay_bycarrier, mean_delay = mean(delay, na.rm = TRUE))

?class

hist(flights_delay$delay)

# simple bar chart
plot(as.factor(flights_delay$delay))

# simple scatterplot
#plot(flights_delay$dep_time, flights_delay$delay)

# add smoothing spline
#smoothingSpline = smooth.spline(wage1$educ, wage1$wage, spar = 0.3)
#lines(smoothingSpline, col = "red")

# PART 2
library(babynames)
  str(babynames)
summary(babynames)


resumen <-babynames %>% group_by(name) %>% summarise(totalm = sum(n, na.rm = TRUE))

resumen <-arrange(resumen,desc(totalm))

head(resumen)



# kimgroup <- group_by(kim1880_2015, sex, year)
# class(kimgroup)
# 
# kimsum <- summarize(kimgroup, n)
# 
# resumen2 <-arrange(resumen,desc(n_deps))

#_------------

kim1880_2015<- filter(babynames, name == "Kim")

resumenKim <-kim1880_2015 %>% group_by(sex, year) %>% summarise(totalm2 = sum(n, na.rm = TRUE))

resumenKim <-arrange(resumenKim, year)

# FemaleKim <- filter(resumenKim, sex == "F")
# MaleKim <- filter(resumenKim, sex == "M")

rm(MaleKim)
rm(FemaleKim)

library(ggplot2)
kimdraw <- ggplot(data=resumenKim,aes(x=year,y=totalm2,linetype=sex)) +
  geom_line(mapping=aes(color=sex)) +
  labs(title="Kim babies by gender and year") +
  scale_x_continuous(breaks=seq(1880,2015,25)) 
kimdraw