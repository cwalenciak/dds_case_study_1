library(ggplot2)
library(plyr)


#============
# Data Setup
#============
setwd("C:/Users/cwale/OneDrive/Desktop/SMU/Winter18/Doing_Data_Science/Case_Study_1/data")

beers <- read.csv("Beers.csv")
colnames(beers)[1] <- c("Beer_Name")

breweries <- read.csv("Breweries.csv")
colnames(breweries)[2] <- c("Brewery_Name")
breweries$State <- trimws(breweries$State)

state_size <- read.csv("statesize.csv")
 
#============
# Question 1
#============
brew_count <- count(breweries$State)
colnames(brew_count) <- c("State", "Brewery_Count")
brew_count <- merge(brew_count, state_size[, c("Abbrev", "SqMiles")], by.x=c("State"), by.y=c("Abbrev"))
brew_count$brewery_density <- brew_count$Brewery_Count / (brew_count$SqMiles/1000)

x <- with(brew_count, order(-brewery_density))
brew_count <- brew_count[x, ]

# barplot(brew_count)

#============
# Question 2
#============
final <- merge(beers, breweries, by.x=c("Brewery_id"), by.y=c("Brew_ID"))

head(final, 6)

#============
# Question 3
#============
# NA's Count
colSums(is.na(final))

#============
# Question 4
#============
state_ibu_medians <- tapply(final$IBU, final$State, median, na.rm = T)
barplot(state_ibu_medians)

#============
# Question 5
#============
#State with max ABV
final[which.max(final$ABV), "State"]

#State with max IBU
final[which.max(final$IBU), "State"]


#============
# Question 6
#============
summary(final$ABV)


#============
# Question 7
#============

# Basic scatter plot
# ggplot(mtcars, aes(x=ABV, y=IBU)) + geom_point()
# Change the point size, and shape
ggplot(final, aes(x=ABV, y=IBU)) + geom_point() +  geom_smooth(method=lm) #size=2, shape=23

