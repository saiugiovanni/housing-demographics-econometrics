library(readxl)
population_estimates_usa <- read_excel("population_estimates_usa.xlsx")

library(foreign)
library(xts)
library(ggplot2)
library(stargazer)

#final datset
population_estimates <- population_estimates_usa[-16,]

summary(population_estimates)

x <- seq(2000,2011)
y <- as.matrix(population_estimates[1:12,2])

#population estimates total
x11()
plot(x, y, main="Population estimates total", type="b", xlab="years", ylab="population estimates", col="red", lwd=2)

#population estimates growth rate
x11()
plot(x, as.matrix(population_estimates[1:12,3]), main="Population estimates growth rate", type="b", xlab="years", ylab="population estimates", col="green", lwd=2)

#population estimates male
x11()
plot(x, as.matrix(population_estimates[1:12,4]), main="Population estimates (male)", type="b", xlab="years", ylab="population estimates", col="blue", lwd=2)

#population estimates female
x11()
plot(x, as.matrix(population_estimates[1:12,5]), main="Population estimates (female)", type="b", xlab="years", ylab="population estimates", col="pink", lwd=2)

