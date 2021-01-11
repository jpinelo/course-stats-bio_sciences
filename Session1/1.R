################################################################################
############################ DESCRIPTIVE STATISTICS ############################
################################################################################

library("modeest")

x <- c(3,6,7,4,3,2,5,4,3,3,7,8,3)
mlv(x, method = "mfv")
hist(x)


x2 <- rnorm(50)
x2
plot(x2)
hist(x2)

mlv(x2,method="naive",bw=1/3) # finds interval with more observations (and considers the middle point as the mode)
mlv(x2,method="parzen",kernel="gaussian")

######################## MEASURES OF CENTRAL TENDENCY ##########################

x3<-c(3,6,NA,7,4,3,2,5,4,3,NA,3,7,8,3)
median(x3,na.rm=TRUE) # NOT sensitive to outliers
# arithmetic mean
mean(x3, na.rm=TRUE) # sensitive to outliers
mean(x2,na.rm=TRUE,trimm=0.1) # truncated mean (trimming data) remove outliers- here removing 0.1 or 10% from each tail of the distribution


# MEANS FOR vars that by nature are not additive or linear, but multiplicative, exponential or logarithmic

# GEOMETRIC mean 
# geometric mean good for rates of change
require(psych)
geometric.mean(x3)

# HARMONIC MEAN
# for ranges with high magnitudes (not for rates of change) 
require(psych)
harmonic.mean(x3)

############################ MEASURES OF DISPERSION ############################

#VARIANCE
z <- rnorm(13)
z

var(z) # variance (squared values of vars) (since square no negative values) (so used in models)
sd(z) # square root of variance (dispersion in same scale of vars) (better than var() to descibe the data so easier to interpret)
# sd for data n < 13 sd should be calculated dividing by n-1 bather than dividing by n

# never use standard error to describe data. it is useful for estimation of parameters

# Coefficient or Variation (useful to compare vars which have diff scales)
# divide standard deviation by the mean: cv = sd/mean


# Interquartile Range
a <- rnorm(1000)
IQR(a)

### CORRELATION COEFICIENT

# PARAMETRIC VS NON-PARAMETRIC

# PARAMETRIC ASSUMES DATA FOLLOW A DISTRIBUTION (normally normal)

# Perason's corr coef. - Parametric
# only for nomral distrib AND linear dependence 
# measures covariance (level of association between vars)


# NON-PARAMETRIC COEFICCIENTS
# No assumptions of normality or linearity
# One assumption: MONOTONIC (always increase or decrease)
# used for ranks

# Spearman's
# rank. (can also be used for ordinal vars for large data)

# Kendal (for ordinal vars with small data (n))


######### compare coefficients

x<-rnorm(30)
y1 <- (0.7*x+0.3)+rnorm(30,0,0.25) # rnorm() to add noise
y1
plot(x,y1)

y2<-(1/(1+exp(-2.5*x)))+rnorm(30,0,0.05) # sigmoid
plot(x, y2)

cor.test(x,y1,method="pearson") # good data for pearson
cor.test(x,y1,method="spearman")
cor.test(x,y1,method="kendall")

cor.test(x,y2,method="pearson") # good data for spearman
cor.test(x,y2,method="spearman")
cor.test(x,y2,method="kendall")




################################################################################
######################### PROBABILITY DESTTRIBUTIONS ###########################
################################################################################

# the probability of a possible outcome (value)

# Normal
dnrom()

# Poisson distribution
# Positive integers (discreet, so not continuous)
dpois()

# Gamma Distribution
# Positive and continuous


# Chi Square
dchisq(18.3,10) # density statistical probability at specific point (18.3 in this case, 10 is degrees of freedom). This is the p-value
pchisq(18.3,10) # integral of probability, or sum up to that point

require(sjPlot)
dist_chisq(deg.f=10)

############# DATA EXPLORATION
summary(x2)

require(pastecs)
stat.desc(x2) # great for checking completeness of data, namely missing values

require(psych)
describe(x2)

head(airquality)
summary(airquality)

# correlation between several vars
pairs(na.omit(airquality[1:4]), panel = function(x, y) {
    points(x, y)
    lines(lowess(x, y), lwd = 2, col = "red")
    })


# histograms
par(mfrow=c(2,2))
hist(airquality$Ozone)
hist(airquality$Solar.R)
hist(airquality$Wind)
hist(airquality$Temp)

# log does not make vars more normal in general, makes SOME. Other could do with dif. transformation
par(mfrow=c(2,2))
hist(log(airquality$Ozone))
hist(log(airquality$Solar.R))
hist(log(airquality$Wind))
hist(log(airquality$Temp))

# check summary per months
tapply(airquality$Ozone,airquality$Month,summary)



par(mfrow=c(2,2))
boxplot(airquality$Ozone~airquality$Month)
boxplot(airquality$Solar.R~airquality$Month)
boxplot(airquality$Wind~airquality$Month)
boxplot(airquality$Temp~airquality$Month)



require(ggplot2)
airquality$Month<-as.factor(airquality$Month)
ggplot(airquality,aes(x=Month,y=Ozone,fill=Month)) +
    geom_violin()




# In the file Cholesterol.csv, we have the measurements of Cholesterol in blood in
# a number of patients. It was recorded as well the age, the gender and the physical
# condition (attributed to 4 different categories) of the patients. Calculate the descriptive
# statistics that you consider appropriate for these data and perform basic plots
# to have a look at the relationships and distributions of the variables.

data <- read.csv2("Cholesterol.csv")
str(data)
summary(data)

# Cholesterol
mean(data$Chol.Blood)
median(data$Chol.Blood)
sd(data$Chol.Blood)




# Gender
plot(as.factor(data$Gender), data$Chol.Blood)
female <- data[data$Gender == "F", ]
male <- data[data$Gender == "M", ]
mean_female <- mean(female$Chol.Blood)
mean_female
mean_male <- mean(male$Chol.Blood)
mean_male

# Hospital
plot(as.factor(data$Hospital), data$Chol.Blood)

# Physical Condition
plot(as.factor(data$Physical.condition), data$Chol.Blood)


# Gender
rl <- lm(data = data, Chol.Blood ~ Age)
plot(data$Age, data$Chol.Blood)
abline(rl[[1]][1], rl[[1]][2], col = 'red')
abline(rl, col = 'red')

library(car)
outlierTest(rl) # not data outliers, but outliers on the model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


library(dplyr)
by_hosp <- data %>%
            group_by(Hospital) %>%
            summarise(n = n(),
                      mean_chol = mean(Chol.Blood),
                      sd_chol = sd(Chol.Blood),
                      mean_age = mean(Age),
                      sd_age = sd(Age))

by_hosp


















