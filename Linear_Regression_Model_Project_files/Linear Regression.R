library(tidyverse)
library(ggthemes)
library(corrgram)
library(corrplot)
df <- read.csv("student-mat.csv", sep = ';')
head(df) #get to know data set
summary(df) #Descriptive Stats 
any(is.na(df)) #check for null values

str(df) #check varibles for factors with levels

#Create a plot to look for coorelation
#grab only numeric columns
num.cols <- sapply(df, is.numeric)
#Filter to numeric columns for coorelation, cor.data returns coorleation table 
cor.data <- cor(df[,num.cols])
#view the coorelation table
cor.data

#coorlation tables are difficult to look at so we can install a 3rd party package to help visualize it
#install.packages("corrgram")
#install.packages("corrplot")
#now ensure the libraires are loaded
#library(corrgram)
#library(corrplot)

corrplot(cor.data, method = "color")
corrgram(df)
corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

#continuing exploratory data analysis 
ggplot(df, aes(G3)) +
    geom_histogram(bins = 20, alpha = .5, fill = "blue")







