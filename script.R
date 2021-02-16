## John Hopkins - Statistical Inference Project
## Simulation and Statistical Data Analysis

## PART 1 - Simulation Exercise 
# Simulate random variables from an exponential distribution with lambda = 0.2
# We want to evaluate the distribution of 40 averages of the exp(lambda = 0.2) distribution
set.seed(1234)
lambda <- 0.2
# What does the exponential distribution look like?
hist(rexp(1000, rate = 0.2))
# now simulate 1000 * 40 exponential dsitributions
n <- 40
nosim <- 1000
expMatrix <- matrix(rexp(n*nosim, rate = lambda), nrow = nosim)
mns <- apply(expMatrix, 1, mean)
sds <- apply(expMatrix, 1, sd)

# should look normal from CLT
hist(mns)
abline(v = 1 / 0.2, col = "red", lwd = 3)
hist(sds)
abline(v = 1 / 0.2, col = "red", lwd = 3)
# compare to the theoretical distribution
popMean <- 1 / 0.2
popSd <- 1 / 0.2

## PART 2 - Basic Inferential Data Analysis
library(datasets)
data("ToothGrowth")
head(ToothGrowth)
# perform t-test suspect different variance per group
g1 <- filter(ToothGrowth, supp == "OJ")$len
g2 <- filter(ToothGrowth, supp == "VC")$len
# looks like very different variance
ggplot(ToothGrowth, aes(supp, len)) + geom_point()
difference <- g1 - g2
mn <- mean(difference)
sd <- sd(difference)
t.test(g1, g2, paired = FALSE, var.equal = FALSE)$conf
t.test(g1, g2, paired = FALSE, var.equal = TRUE)$conf
# since the 95% CI contains 0, we cannot rule out that these are the same
## Here we will need to do inferential hypothesis testing
# facet by dose and boxplot
ggplot(ToothGrowth, aes(supp, len)) + geom_boxplot() + facet_wrap(~dose)
g1 <- g1[1:20]
g2 <- g2[1:20]
t.test(g1, g2, paired = FALSE, var.equal = FALSE)$conf
t.test(g1, g2, paired = FALSE, var.equal = TRUE)$conf

a1

