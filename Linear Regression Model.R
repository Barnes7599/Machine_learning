# Linear Regression #Part 2
df <- read.csv("student-mat.csv", sep = ';')
library(tidyverse)
library(caTools)
# Set a seed, since the splits are going to be random, we need to set the seed so that we all can have the same results
set.seed(101)
#Split data into train and test sets
# Split the sample on the column you are trying to perdict
#70% training data 30% for testing
sample <- sample.split(df$G3, SplitRatio = 0.7)
# sample.split just assigns TRUE / FALSE to the ratio
# TRUE == 70% of data (Train)
train <- subset(df, sample == TRUE)
# FALSE == 30% of data (Test)
test <- subset(df, sample == FALSE)

# TRAIN AND BUILD MODEL
# General model of building a linear regression model in R
#           model <- lm(y ~ x1 + x2, data) (Select variables)
#           model <- lm(y ~ ., data) (Use all the variables)
model <- lm(G3 ~ ., train)

#Run Model
summary(model)

# store the models residuals in a data frame
res <- residuals(model)
class(res)
res <- as.data.frame(res)
head(res)

# for most residuals you want it to look like a normal distribution, because if they are normally distributed then this indicates that the mean of the difference between our predictions and the actual values are close to zero, that means if we miss, we are missing both the short and long of the actual value and the liklihood of a miss being far from the actual value gets smaller as the distance of the actual value gets larger. 

ggplot(res, aes(res)) +
    geom_histogram(fill = "blue", alpha = .5)

#regression validation
#https://en.wikipedia.org/wiki/Regression_validation

plot(model)

# Predictions
G3.predictions <- predict(model, test)
#column bind the resuts and the predictions 
results <- cbind(G3.predictions, test$G3)
#name the columns in results
colnames(results) <- c('predicted', 'actual')
#transform the matrix into a data frame
results <- as.data.frame(results)
head(results)

# in the histogram you saw some negative predictions but the lowest you can score on the test is zero so we need to rid the model of those negative predictions by making them zeros

to_zero <- function(x){
    if (x < 0) {
        return(0)
    }else {
        return(x)
        
    }
}

# Apply zero function

results$predicted <-  sapply(results$predicted, to_zero)

# MEAN SQUARED ERROR
mse <-  mean( (results$actual - results$predicted)^2)
#print mse
mse
#print square root of mse
mse^0.5

######
SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$G3) - results$actual)^2)
#R2 will tell you how much of the variance we explaining by the test data
R2 <- 1 - SSE/SST
#print R2
R2

differences <- results %>% 
    mutate(differences = actual - predicted) %>% 
    summarise(mean(differences))

differences

