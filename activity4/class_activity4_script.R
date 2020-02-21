#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################
#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
versicolor <- filter(iris,Species=="versicolor")

x <- c("Sepal.Length","Petal.Length","Sepal.Length")
y <- c("Sepal.Width","Petal.Width","Petal.Length")

lm.out <- list()

for (i in 1:3) {
  lm.out[[i]] <- lm(versicolor[,x[i]]~versicolor[,y[i]])
}




#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

iris2<-left_join(iris,height,by=c("Species"))
iris2$Petal.Width / iris2$Height.cm

#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) +
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) +
  geom_point() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())


#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width)) +
  geom_point(aes(color=Species,size=Species)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank())

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
# In ggplot, arguments and parameters are added in a modular form, by adding more geom_ elements and modifying the theme().
# In plot, almost every argument tends to be within the plot() function.