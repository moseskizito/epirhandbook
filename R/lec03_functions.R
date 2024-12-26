
#dataset

data("chickwts")
head(chickwts)
dim(chickwts)
str(chickwts)

data("trees")
head(trees)
dim(trees)
str(trees)

#FINDING
?average
??average
?anova
apropos("nova")

#mandatory

Sys.time()
mean()
mean(chickwts$weight)

#optional
chickwts$weight[35] <- NA
mean(chickwts$weight) # doest return value due to missing value

mean(chickwts$weight, na.rm = TRUE)
mean(chickwts$weight, na.rm = TRUE, trim = 0.05)
mean(chickwts$weight, 0.05, TRUE)
data("chickwts")

# Applying functions to >1D objects

colSums(trees)
colMeans(trees)

?apply

apply(trees, 2, mean)

# ...
trees[1,1] <- NA
trees[4,2] <- NA
apply(trees, 2, mean)
apply(trees, 2, mean, na.rm=TRUE)

# aggregate

aggregate(chickwts$weight ~ chickwts$feed, FUN = "mean")
aggregate(weight ~ feed, FUN = mean, data = chickwts)

aggregate(chickwts$weight, by=list(chickwts$feed), FUN="mean")
?glm

glm(weight ~ feed, data = chickwts)

model1 <- glm(weight ~ feed, data = chickwts)
class(model1)
str(model1)

summary(model1)
model1$residuals
model1$coefficients


?ts

x <- 1:30
plot(x)

x <- ts(x, start = 1,frequency = 365)
x

plot(x)

#ts {stats}

install.packages("MASS")
library(MASS)
require(MASS)

help(package = "MASS")


ts <- rnorm(500, mean = 0, sd=2)
?cusum
??cusum
install.packages("qcc")
?cusum
library("qcc")
cusum(ts) # decision. interval 2
