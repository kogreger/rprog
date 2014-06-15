## Q1
library(datasets)
data(iris)
?iris
names(iris)
levels(iris$Species)
by(iris$Sepal.Length, iris$Species, mean)


## Q2
apply(iris[, 1:4], 2, mean)


## Q3
library(datasets)
data(mtcars)
?mtcars
tapply(mtcars$mpg, mtcars$cyl, mean)


## Q4
names(mtcars)
abs(mean(mtcars[mtcars$cyl == 4, ]$hp) - mean(mtcars[mtcars$cyl == 8, ]$hp))


## Q5
debug(ls)
ls(mtcars)
