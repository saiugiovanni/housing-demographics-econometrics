library(readxl)
population_estimates_usa_final <- read_excel("population_estimates_usa.xlsx")


new_data <- population_estimates_usa_final[-c(13, 14, 15, 16),]

#linear regression
df <- data.frame(y=as.numeric(new_data$`Population mid-year estimates Total`), birth=as.numeric(new_data$Births), death=as.numeric(new_data$Deaths), net_migration=as.numeric(new_data$`Net migration`), crude_birth=as.numeric(new_data$`Crude birth rate`), crude_death=as.numeric(new_data$`Crude death rate`))
cor(df)

fit <- lm(y ~ birth + death +net_migration + crude_birth + crude_death, data = df)
summary(fit)

#latex
library(stargazer)
stargazer(fit, summary = TRUE) 

#best model: birth, crude birth, net_migration
fit2 <- lm(y ~ birth + net_migration + crude_birth, data = df)
summary(fit2)

stargazer(fit2, summary = TRUE) 

#confidence intervals
IC <- confint(fit2, levels=0.95)
IC

stargazer(IC, summary = TRUE)

#population vs house prices

hp <- (new_data$`HOUSES PRICES` - new_data$`Population mid-year estimates Total (growth rate)`)/new_data$`Population mid-year estimates Total (growth rate)`
df1 <- data.frame(years=as.numeric(new_data$...1), house_prices=as.numeric(hp), population_growth=as.numeric(new_data$`Population mid-year estimates Total (growth rate)`), natural_increase=as.numeric(new_data$`Natural increase rate`), crude_death=as.numeric(new_data$`Crude death rate`), crude_birth=as.numeric(new_data$`Crude birth rate`), total_increase=as.numeric(new_data$`Total increase rate`), cpi=as.numeric(new_data$CPI), gdp=as.numeric(new_data$GDP))

#plot
x11()
plot(df1$years, df1$house_prices, main="House prices rate over the years", type="b", xlab="years", ylab="house prices rate", col="red", lwd=2)

fit3 <- lm(house_prices ~ crude_death + crude_birth + cpi + gdp, data = df1)
summary(fit3)

stargazer(fit3, summary = TRUE)

linearHypothesis(fit3, rbind(c(1,0,0,0,0),c(0,0,0,1,0)),c(0,0))

fit4 <- lm(house_prices ~ -1 + population_growth + crude_death + crude_birth + gdp, data = df1)
summary(fit4)

stargazer(fit4, summary = TRUE) 

fit5 <- lm(house_prices ~ -1 + crude_birth + gdp, data = df1)
summary(fit5)

stargazer(fit5, summary = TRUE)

IC1 <- confint(fit5, levels=0.95)
IC1

stargazer(IC1, summary = TRUE)

#H0: betai=0
beta1_test <- abs((fit3$coefficients[2]-0)/coef(summary(fit3))[2, "Std. Error"])
beta1_test #1.441095

#we reject H0 if beta1_test>qnrm(0.975)
qt(0.975, df=fit3$df.residual)
beta1_test > qt(0.975, df=fit3$df.residual)
#FALSE=cannot reject null hp (no statistical evidence that beta1=0)

beta2_test <- abs((fit3$coefficients[3]-0)/coef(summary(fit3))[3, "Std. Error"])
beta2_test #1.686098

#we reject H0 if beta3_test>qnrm(0.975)
qt(0.975, df=fit3$df.residual)
beta1_test > qt(0.975, df=fit3$df.residual)
#FALSE=cannot reject null hp (no statistical evidence that beta2=0)

#scatterplot

library(car)

x11()
par(mfrow=c(2,2)) #prepare a figure which contains 4 plots 
plot(new_data$`Population mid-year estimates Total`,new_data$`HOUSES PRICES`, main = "House price and Population mid-year estimates ",
     col.main = "red", col= "red")
plot(new_data$Births,new_data$`HOUSES PRICES`, main = "House price and Births ",
     col.main = "green", col= "green")
plot(new_data$Deaths,new_data$`HOUSES PRICES`, main = "House price and Deaths ",
     col.main = "blue", col= "blue")
plot(new_data$`Net migration`,new_data$`HOUSES PRICES`, main = "House price and Net migration ",
     col.main = "pink", col= "pink")
crude_death=as.numeric(new_data$`Crude death rate`)
crude_birth=as.numeric(new_data$`Crude birth rate`)
population_growth=as.numeric(new_data$`Population mid-year estimates Total (growth rate)`)


linearHypothesis(fit4, c("population_growth=0", "crude_death=0",
                         "crude_birth=0"))
linearHypothesis(fit4, rbind(c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), c(0,0,0))
lh <-linearHypothesis(fit4, rbind(c(0,1,0,0)), 0)

stargazer(lh, summary = TRUE)

#correlation
df2 <- data.frame(house_prices=as.numeric(hp), population_growth=as.numeric(new_data$`Population mid-year estimates Total (growth rate)`), natural_increase=as.numeric(new_data$`Natural increase rate`), crude_death=as.numeric(new_data$`Crude death rate`), crude_birth=as.numeric(new_data$`Crude birth rate`), total_increase=as.numeric(new_data$`Total increase rate`), net_migration=as.numeric(new_data$`Net migration`))
cor(df2)

stargazer(cor(df2), summary = TRUE)

#plot the errors
library(ggplot2)
library(lmtest)
library(sandwich)

#fit2:
plot(fit2)
bptest(fit2, ~ net_migration + birth + crude_birth, data = df)
bptest(fit2, ~ net_migration + birth + crude_birth, data = df, studentize = FALSE)

#fit4:
plot(fit4)
bptest(fit4, ~ population_growth + crude_death + crude_birth, data = df1)
bptest(fit4, ~ population_growth + crude_death + crude_birth, data = df1, studentize = FALSE)


#autoregressive model
library(dyn)
library(dynlm)
library(zoo)
library(lmtest)
library(orcutt)
library(sandwich)

mytime <- zoo(df)
y <- mytime[,1]
birth <- mytime[,2]
net_migration <- mytime[,4]
crude_birth <- mytime[,5]
birth_1 <- lag(birth, -1, na.pad = TRUE)
net_migration_1 <- lag(net_migration, -1, na.pad=TRUE)
crude_birth_1 <- lag(crude_birth, -1, na.pad=TRUE)

fit2_1 <- dynlm(y ~ birth + net_migration + crude_birth + birth_1 + net_migration_1 + crude_birth_1)
summary(fit2_1)

stargazer(fit2_1, summary = TRUE)

birth_2 <- lag(birth, -2, na.pad=TRUE)
birth_3 <- lag(birth, -3, na.pad=TRUE)
net_migration_2 <- lag(net_migration, -2, na.pad=TRUE)
net_migration_3 <- lag(net_migration, -3, na.pad = TRUE)
crude_birth_2 <- lag(crude_birth, -2, na.pad = TRUE)
crude_birth_3 <- lag(crude_birth, -3, na.pad = TRUE)

fit2_2 <- dynlm(y ~ birth+net_migration+crude_birth+L(birth,2)+L(net_migration,2)+L(crude_birth,2))
summary(fit2_2)

stargazer(fit2_2, summary = TRUE)

myt <- zoo(df1)
house_prices <- myt[,2]
population_growth <- myt[,3]
crude_death <- myt[,5]
crude_birth <- myt[,6]
population_growth_1 <- lag(population_growth, -1, na.pad=TRUE)
crude_death_1 <-lag(crude_death, -1, na.pad=TRUE)
crude_birth_1 <- lag(crude_birth, -1, na.pad=TRUE)

fit4_1 <- dynlm(house_prices ~ population_growth+crude_death+crude_birth+population_growth_1+crude_death_1+crude_birth_1)
summary(fit4_1)

stargazer(fit4_1, summary = TRUE)

#durbin-watson
dwtest(fit2_1) 
dwtest(fit4_1)

#breusch-pagan 
bgtest(fit2_1) 
bgtest(fit4_1)

Box.test(fit2_1$residuals, type = "Ljung-Box", lag = 6)
Box.test(fit2_1$residuals, type = "Box-Pierce", lag = 6)
Box.test(fit4_1$residuals, type = "Ljung-Box", lag = 6)
Box.test(fit4_1$residuals, type = "Box-Pierce", lag = 6)

x11()
acf(fit2_1$residuals)

x11()
acf(fit4_1$residuals)

#time series
library(nlWaldTest)
library(forecast)
library(xts)
library(orcutt)

df1 <- ts(df1, start = 2000, end = 2011, frequency = 1)
str(df1)

house_prices <- ts(df1[,2], start = 2000, end=2011, frequency=1)
population_growth <- ts(df1[,3], start = 2000, end=2011, frequency=1)
natural_increase <- ts(df1[,4], start = 2000, end=2011, frequency=1)
crude_death <- ts(df1[,5], start = 2000, end=2011, frequency=1)
crude_birth <- ts(df1[,6], start = 2000, end=2011, frequency=1)
total_increase <- ts(df1[,7], start = 2000, end=2011, frequency=1)
cpi <- ts(df1[,8], start = 2000, end=2011, frequency=1)
gdp <- ts(df1[,9], start = 2000, end=2011, frequency=1)

bic <- function(model)
{
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  return(
    round(c("p" = npar - 1,
            "N"=t,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}

population_growth <- window(df1[,3], start = 2000, end=2011)
natural_increase <- window(df1[,4], start = 2000, end=2011)
crude_death <- window(df1[,5], start = 2000, end=2011)
crude_birth <- window(df1[,6], start = 2000, end=2011)
total_increase <- window(df1[,7], start = 2000, end=2011)
house_prices <- window(df1[,2], start = 2000, end=2011)
cpi <- window(df1[,8], start = 2000, end=2011)
gdp <- window(df1[,9], start = 2000, end=2011)


ts1 <- dynlm(d(house_prices) ~ cpi +L(cpi,1)+L(cpi,2)+L(cpi,3)+L(cpi,4))
summary(ts1)

ts2 <- dynlm(d(house_prices) ~ cpi +L(cpi,1)+L(cpi,2)+L(cpi,3))
summary(ts2)

ts3 <- dynlm(d(house_prices) ~ cpi+L(cpi,1)+L(cpi,2))
summary(ts3)

ts4 <- dynlm(d(house_prices) ~ cpi+L(cpi,1))
summary(ts4)

ts5 <- dynlm(d(house_prices) ~ cpi)
summary(ts5)


bic(ts1)
bic(ts2)
bic(ts3)
bic(ts4)
bic(ts5)

stargazer(ts1, summary = TRUE)

#best fit -> ts1 (lowest BIC)

AIC(ts1, ts2, ts3, ts4, ts5)
stargazer(BIC(ts1, ts2, ts3, ts4, ts5), summary=TRUE)

fitardl <- dynlm(d(house_prices) ~ cpi+L(cpi,1)+L(cpi,2)+L(d(house_prices),1)+L(d(house_prices),2))
summary(fitardl)

delta <- fitardl$coef[1]
delta0 <- fitardl$coef[2]
delta1 <- fitardl$coef[3]
delta2 <- fitardl$coef[4]
theta1 <- fitardl$coef[5]
theta2 <- fitardl$coef[6]

alpha <- delta/(1-theta1-theta2)
beta0 <- delta0
beta1 <- delta1+beta0*theta1
beta2 <- delta2+beta1*theta1+beta0*theta2
beta3 <- beta2*theta1+beta1*theta2
beta4 <- beta3*theta1+beta2*theta2
beta5 <- beta4*theta1+beta3*theta2
beta6 <- beta5*theta1+beta4*theta2
beta7 <- beta6*theta1+beta5*theta2
beta8 <- beta7*theta1+beta6*theta2

#impact multiplier
beta0

#total multiplier 
tot_mult1 <- beta0 + beta1 + beta2 + beta3 + beta4 + beta5 + beta6 + beta7 + beta8
tot_mult1



library(FinTS)
d_hp <- diff(house_prices)
archTest <- ArchTest(d_hp, lags = 1, demean = TRUE)
archTest #not rejecting -> no ARCH effect 

#PCA 

df1.new <- data.frame(population_growth=as.numeric(new_data$`Population mid-year estimates Total (growth rate)`), natural_increase=as.numeric(new_data$`Natural increase rate`), crude_death=as.numeric(new_data$`Crude death rate`), crude_birth=as.numeric(new_data$`Crude birth rate`), total_increase=as.numeric(new_data$`Total increase rate`), cpi=as.numeric(new_data$CPI))
df1.pca = princomp(df1.new, scale=TRUE, center=TRUE)
summary(df1.pca)

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(df1.pca$sd^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(df1.new,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(df1.pca$sd^2)/sum(df1.pca$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue') # 80% of variability explained
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(df1.new),labels=1:ncol(df1.new),las=2)

x11()
par(mfrow = c(1,2))
barplot(df1.pca$sdev^2, main = 'Eigenvalues of each component')
barplot(cumsum(df1.pca$sdev^2)/sum(df1.pca$sdev^2), main = 'Cumulative Explained Variance', ylab = 'Variance Explained')

#sc test

y <- house_prices
fs.y <- Fstats(y ~ 1)
sctest(fs.y)
#we can reject at a level of 1% of confidence

x11()
plot(fs.y)

x11()
plot(y)
lines(breakpoints(fs.y))
breakpoints(fs.y)

sctest(fs.y, type="aveF") #uses asymptotic chi^2 instead of F statistics

d_y <- diff(house_prices)
fs.dy <- Fstats(d_y ~ 1)
sctest(fs.dy) #12% -> cannot reject H0 -> don't observe any change in our model

plot(d_y)
lines(breakpoints(fs.dy))
sctest(fs.dy, type="aveF")

#plot net migration
x <- seq(2000,2011)
x11()
plot(x, new_data$`Net migration`, main="Net migration over the years", type="b", xlab="years", ylab="net migration", col="orange", lwd=2)

