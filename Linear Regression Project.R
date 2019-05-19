library(tidyverse)
library(ggthemes)
library(lubridate)
#You are provided hourly rental data spanning two years. For this competition, the training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month. You must predict the total count of bikes rented during each hour covered by the test set, using only information available prior to the rental period.

bike <-  read.csv("bikeshare.csv")
head(bike)
str(bike)
bike$season <- as.factor(bike$season)
str(bike)
bike$weather <- as.factor(bike$weather)

# Exploratory Data Analysis
# count vs temp
ggplot(bike, aes(temp, count)) +
    geom_point(aes(color = temp), alpha = .5) +
    labs(x = "Temperature", y = "Number of Bikes Rented")

#Need to turn datetime into POSIXct data field
bike$datetime <- as.POSIXct(bike$datetime)
#bike %>% mutate(datetime = ymd_hms(datetime))

# count vs datetime
ggplot(bike, aes(datetime, count)) +
    geom_point(aes(color = temp)) +
    scale_color_continuous(low = "turquoise2", high = "orange")

#coorelation between temp and count
cor(bike[,c("temp","count")])

# Explore seasonality with boxplot
ggplot(bike, aes(season, count)) +
    geom_boxplot(aes(color = season)) +
    theme_minimal()

# Create an hour column (figure our a better way using a package rather than base R)
bike$hour <- sapply(bike$datetime, function(x){format(x,"%H")})

head(bike)

# visualize count vs hour
workday <- bike %>% 
    filter(workingday == 1)
nonworkday <- bike %>% 
    filter(workingday == 0)

ggplot(workday, aes(hour, count)) +
    geom_point(aes(color = temp), position = position_jitter(width = 1, height = 0)) +
    scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red')) +
    labs(title = "Weekday; Rentals by Hour")

ggplot(nonworkday, aes(hour, count)) +
    geom_point(aes(color = temp), position = position_jitter(width = 1, height = 0)) +
    scale_color_gradient2_tableau(palette = "Red-Green Diverging") +
    labs(title = "Weekend; Rentals by Hour")

#building the model
temp.model <- lm(formula = count ~ temp, data = bike )
summary(temp.model)
#Interpreting the intercept (β0): It is the value of y when x=0.Thus, it is the estimated number of rentals when the temperature is 0 degrees Celsius. Note: It does not always make sense to interpret the intercept. 
#Interpreting the "temp" coefficient (β1): It is the change in y divided by change in x, or the "slope". Thus, a temperature increase of 1 degree Celsius is associated with a rental increase of 9.17 bikes. This is not a statement of causation. β1 would be negative if an increase in temperature was associated with a decrease in rentals.
# Based on the summary how many bike rentals would you have if the temp was 0 degrees Celsius (y = mx +b)

9.17*25 + 6.0462

bike$hour <- sapply(bike$hour, as.numeric)

# Finally build a model with all the variables

model_bike <- lm(count ~ . -casual - registered - datetime - atemp, bike)
summary(model_bike)



