sink("~/Documents/HKS/AP1 201 - stats 2/ps1_log.txt")
library(ggplot2)
library(haven)
setwd("~/Documents/HKS/AP1 201 - stats 2")
read_dta("India_agric_yields.dta")
India_ag <- read_dta("India_agric_yields.dta")
new_India <- transform(India_ag, yield = output/plotarea)
t.test(new_India$yield, mu=5000)

M <- new_India$gender == 0 
male.yields = new_India[M,]$yield 
W <- new_India$gender == 1 
female.yields = new_India[W,]$yield 
t.test (male.yields, female.yields)

length(male.yields)
length(female.yields)

read_dta("gender.dta")
gender.data <- read_dta("gender.dta")

# Scatter plot with regression line
ggplot(gender.data, aes(y=wage, x=educ)) + geom_point() + geom_smooth(method="lm", se=FALSE)

# Bivariate Regression
fit <- lm(wage ~ educ, data=gender.data)
summary(fit)
fit1 <- lm(wage ~ educ==10, data=gender.data)
summary(fit1)

# Bivariate Reg: education vs residuals
uhat <- fit$residuals
#Find the mean of the residuals (to determine if they’re centered around 0
mean(uhat)
#Put in variable uhattest to regress
uhattest <- lm(educ ~ uhat, data=gender.data)
summary(uhattest)

#scatter - education vs residuals
ggplot(gender.data, aes(y=uhat, x=educ)) + geom_point() + geom_smooth(method="lm", se=FALSE)


sink()