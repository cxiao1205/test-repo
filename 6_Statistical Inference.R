# Statistical Inference is the foundation of data science.
# two different inferential paradigms: Bayesians and frequentists
# In this class, focus on frequentist paradigm

library(beepr)
getwd()
setwd("C:/Users/Xiao.Cheng/Desktop/R Code/Data Science Specialization_Coursera")

# Probability
        # PMF: probability mass function. (equavalent to probability density function, but PMF is more used for discret situation and PDF is more often used for continous).
        # e.g. Bernoulli distribution: p(x)= theta^x * (1-theta)^(1-x), where x=0,1
x <- c(-0.5, 0, 1, 1, 1.5)
y <- c(0, 0, 2, 0, 0)
plot(x, y, lwd = 3, frame = FALSE, type = "l") # here is a example of prob density function: f(x)= 2x, if 0<x<1, and f(x)=0 otherwise. To calculate the cumulated prob, can also use pbeta() function
        # Diagnostic tests: Let + and - be the events that the result of a diagnostic test is positive or negative respectively, and let D and Dc be the event that the subject of the test has or does not have the disease respectively.
        # Sensitivity = P(+|D)
        # Specificity = P(-|Dc)
        # Positive predictive value = P(D|+)
        # Negative predictive value = P(Dc|-)
        # Prevalence of the disease = P(D)
        # Bayes formula infers: P(D|+)/P(Dc|+)=P(+|D)/P(+|Dc) * P(D)/P(Dc)
        # the formula above can be interpret as this: Post-test odds of D = Diagnostic loglihood ratio * pre-test odds of D
        # Sample mean follows normal distribution. Sample variance follows chi-square distribution
nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim * n),nosim), 1, mean))  # gen nosim * n numbers of random normal distributed numbers, make them as a matrix with nosim rows, apply the mean function to each column (so keep the row dimention), and calculate the standard deviation of the means (nosim number of means)
1 / sqrt(n)  # theoretical value, the sd of sample mean. Can check the simulated mean values for other 

library(UsingR); data(father.con); # Data example
x <- father.son$sheight
n <- length(x)
hist(x)
round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)), 2) # round 4 statistics for dataset to two digits. 

choose(8, 7)* .5^8 + choose(8, 8) * .5^8 # if each gender has an independent 50% probability for each birth, what's the probability of getting 7 or more girls out of 8 births?
pbinom(6, size = 8, prob = .5, lower.tail = FALSE) # same thing. 

pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)  # 2.8 sd away. probability more than that?
        # Poisson distribution: P(X = x; lambda) = lambda^x * exp(-lambda) / x!   used to model counts. the mean is lambda, and the variance is lambda. Useful for modeling event-time or survival data, modeling contingency tables, approximenting binomials when n is laege and p is small.
        # X ~ Poisson(lambda * t) where lambda=E[X|t] is the expected count per unit of time, and t is the total monitoring time 
ppois(3, lambda = 2.5*4) # If watching the bus stop for 4 hours, what is the probability that 3 or fewer people show up for the whole time? Suppose the number of people that show up at a bus stop is Poisson with a mean of 2.5 per hour.
        # when n is large and p is small the Poisson distribution in an accurate approximation to the binomial distribution
pbinom(2, size = 500, prob = 0.01) # We flip a coin with success probability 0.01 five hundred times. What's the probability of 2 or fewer sucesses?
ppois(2, lambda = 500 * 0.01)  # very close to above. since np is realatively small

# Asymptotics 
        # is the term for the behavior of statistics as the sample size (or some other relevant quality) limits to infinity (or some other relevant number)
        # LLN: Law of Large Numbers
n <- 1000
means <- cumsum(rnorm(n))/(1:n)
plot(means) # see the convergen tendency.
        # CLT: the Central Limit Theorem
        # Confidence Interval
n <- 20 # initiate a simple simulation for flip a coin. each simulation we flip 20 times
pvals <- seq(0.1, 0.9, by = 0.05) # the true value of successful flip changes from 0.1 to 0.9
nosim <- 1000 # simulate for 1000
coverage <- sapply(pvals, function(p){ # loop over pvals, calculate the following. for each successful flip prob, calculate the following:
        phats <- rbinom(nosim, prob = p, size = n)/n # gen 1,000 times of sim (each sim flip 10 times), calculate the mean
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n) # lower limit of the CI (95% CI)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n) # higher limit of the CI (95% CI)
        mean(ll < p & ul > p) # proportion of the times that they can cover that true value of p that I used to simulate the data
        
})
plot(pvals, coverage, type = "l")
abline(h = 0.95)  # we can see the confidence interval in simulated example in general not covers the true value of pop mean 95% of the times. CLT doesn't work accurately because n is not large enough. A quick fix is form the interval with (X + 2)/ (n+ 4). It is called the Agresti/Coull interval

n <- 100 # When we increase the n, the result is better (but still, not good enough)
pvals <- seq(0.1, 0.9, by = 0.05) # the true value of successful flip changes from 0.1 to 0.9
nosim <- 1000 # simulate for 1000
coverage <- sapply(pvals, function(p){ # loop over pvals, calculate the following. for each successful flip prob, calculate the following:
        phats <- rbinom(nosim, prob = p, size = n)/n # gen 1,000 times of sim (each sim flip 10 times), calculate the mean
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n) # lower limit of the CI (95% CI)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n) # higher limit of the CI (95% CI)
        mean(ll < p & ul > p) # proportion of the times that they can cover that true value of p that I used to simulate the data
        
})
plot(pvals, coverage, type = "l")
abline(h = 0.95)  # Better 

n <- 20 # Ajusted CI
pvals <- seq(0.1, 0.9, by = 0.05) # the true value of successful flip changes from 0.1 to 0.9
nosim <- 1000 # simulate for 1000
coverage <- sapply(pvals, function(p){ # loop over pvals, calculate the following. for each successful flip prob, calculate the following:
        phats <- (rbinom(nosim, prob = p, size = n)+2)/(n+4) # ADDING TWO SUCCESSFUL AND TWO FAILURES WHEN GEN RVs
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n) # lower limit of the CI (95% CI)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n) # higher limit of the CI (95% CI)
        mean(ll < p & ul > p) # proportion of the times that they can cover that true value of p that I used to simulate the data
        
})
plot(pvals, coverage, type = "l")
abline(h = 0.95)  # Much better! almost all above 95%. However, it implies the CIs might be too wide. 

        # Poisson interval
        # Assume a nuclear pump failed 5 times out of 94.32 days, what is a 95% confidence interval for the failure rate per day?
        # assume: X ~ Poisson(lambda * t). Estimate lambdahat = X / t. Var(lambdahat) = lambda/t. So lambdahat/t is our variance estimate (since for poisson, both the mean and the variance are lambda)
x <- 5  # number of events
t <- 94.32  # monitoring time
lambda <- x/t # estimated lambda
round(lambda + c(-1, 1)* qnorm(0.975) * sqrt(lambda/t), 3) # 95% confidence interval of true population lambda
poisson.test(x, T = 94.32)$conf  # gurantee 95% of coverage. might be too conservative
        # simulation with poisson coverage
lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 100 # total monitoring time
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t)/t # mean value of each simulation
        ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
        ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
        mean(ll < lambda & ul > lambda)
})
plot(lambdavals, coverage, type = "l") 
abline(h = 0.95) # if increase t to 1000, much better.

# T Confidence intervals
        # simply change the z quantiles to t quantiles. (thicker tails than normal, only one parameter: df). Intuition behind: (Xbar-mu)/(sigma/sqrt(n)) will definitely follow normal distribution. However, (Xbar-mu)/(S/sqrt(n)) will not, because S is only an estimator of sigma, so the value of S is a random variable. as n increases, S -> sigma. but for small n, may have big difference.
        # So the CI is xbar +/- t(n-1) * S / sqrt(n)
        # t works well whenever the distribution of the data is roughly symmetric and mound shaped. Paired obs are often analyzed using the t interval by taking differences.

        # compare the mean across groups: CI for the diff in mean is Ybar-Xbar +/- t(nx+ny-2) * Sp * (1/nx + 1/ny)^(1/2), Sp is pooled standard deviation. Sp^2= {(nx-1)*Sx^2+(ny-1)*Sy^2}/(nx+ny-2)
        # P-value: the probability of such sample being chosen if null is true.
        # Power of the test: prob of rejecting the null hypothesis when it is false. power = the prob of rejecting the null hypothesis, assumning that the true population mean is equal to the critical parameter value. 1 - Beta = prob(Xbar > Mu0 + z(1-alpha) * sigma / sqrt(n); mu = mu(a))

# Multiple comparisons: multiple testing corrections
        # Hypothesis testing/significance analysis is commonly overused. Correcting for multiple testing avoids false positives or discoveries. Two key components: Error measure and Correction
        # Type I error: False Positive. Type II errors: False negative 
        # e.g. suppose you perform 10,000 tests and positive test result for all of them (take the null). suppose alpha =0.05. Then 500 false positives! it's a lot! How do we avoid so many false positives?
        # May use FWER: family-wise error rate---alpha(fwer) = alpha/m, where m is the number of tests you did. Call the P-values less than alpha(fewer) significant---tooooo conservative
        # May use FDR: false discovery rate. Order the P values from smallest to larget, P(1), ..., P(m), where m is the number of tests you did. Call any P(i)<=alpha * (i/m) significant ----now less conservative, but might allow for more false positives. Mau behave strangely. 
        # May also adjust the P-values. e.g. Suppose P-values are P1, ..., Pm. Pi(fwer)=max(m * Pi, 1), for each P-value
set.seed(1010093)  # a case study: no true positives
pValues <- rep(NA, 1000)
for(i in 1:1000){
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2, 4]
}
sum(pValues < 0.05)  # Controls false positive rate. about 5% false positive
sum(p.adjust(pValues,method="bonferroni") < 0.05)  # controls FWER
sum(p.adjust(pValues,method="BH") < 0.05)  # controls FDR

set.seed(1010093)    # another case study, 50% true positives
pValues <- rep(NA, 1000)
for (i in 1:1000){
        x <- rnorm(20)
        # First 500 beta=0, last 500 beta=2
        if(i <= 500){y <- rnorm(20)}else{y <- rnorm(20, mean=2*x)}
        pValues[i] <- summary(lm(y~x))$coeff[2, 4]
}
trueStatus <- rep(c("zero","not zero"), each = 500)
table(pValues < 0.05, trueStatus)  # discovered all true relationships. but fail to reject 24 
table(p.adjust(pValues, method="bonferroni")<0.05, trueStatus)    # False positive number reduced
table(p.adjust(pValues, method="BH")<0.05, trueStatus)    # ajust the p value 
par(mfrow = c(1,2))
plot(pValues,p.adjust(pValues, method="bonferroni"), pch=19)
plot(pValues,p.adjust(pValues, method="BH"), pch=19) # compare the actual p-values to adjusted p-values

# Bootstrap
        # is a tremendously useful tool for constructing confidence intervals and calculating standard errors for difficult statistics
        # e.g. how would one derive a confidence interval for the median?
        # Now, suppose we have a 6-sided die. and the distribution of the outcome is 1/6 prob of each side (population distribution). What is the mean if we throw the die 50 times? 
        # of course one way to do it is theoretically (math derivation), and another way is simulation. But what if we only had one sample? (then we can only calculate the mean once---and no distribution for the mean, which could be not so accurate).
        # Bootstrap construct an estimated population distribution from the empirical distribution
library(UsingR)
data(father.son)
x <- father.son$sheight # son's height
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n*B, replace = TRUE), B, n)   # simulating the empirical distribution. Everyrow is a complete resampled dataset. 
resampledMedians <- apply(resamples, 1, median)  # keep the dimention 1 (rows), so calculate the column median
hist(resampledMedians) # histogram of the resampled median (bootstraping). We can now calculate the statistics of resampled median. 
hist(x) # oringinal distribution of the data
        # Bottom line: suppose that I have a statistic that estimates some population parameter, but I don't know its sampling distribution. It suggests using the distribution defined by the data to approximate its sampling distribution. 
        # Bootstrap Procedure:
        # 1. Sample n observations with replacement from the observed data resulting in one simulated complete data set
        # 2. Take the statistic of the simulated data set
        # 3. Repeat these two steps B times, resulting in B simulated statistics
        # 4. These statistics are approximately drawn from the sampling distribution of the stat of n observations; therefore we can Draw a histogram of them, Calculate their standard deviation to estimate the standard error of stat, Take the CIs
        # Note: the bottstrap is non-parametric, Better percentile bootstrap CI correct for bias. 

B <- 10000   # example code
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(0.025, 0.975))
g = ggplot(data.frame(medians = medians), aes(x = medians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g # sampling distribution of the median

# Permutation tests (used for group comparisons)
        # rank sum test, Fisher's exact test, ordinary permutation test, randomization tests
        # Consider the  null hypothesis that the distribution of the observations from each group is the same
        # Then, the group labels are irrelevant
        # Consider a data frame with count and spray
        # Permute the spray (group) labels
        # Recalculate the statistics, Mean difference in counts, Geometric means, T stat
        # Calculate the percentage of simulations where the simulated statistic was more extreme (toward the alternative) than the observed.

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]  # Permutation test example: B v C groups in InsectSprays 
y <- subdata$count # dead insects count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"]) #function to calculate the group mean diff
observedStat <- testStat(y, group) # group mean diff
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group))) # permute group labels
observedStat
mean(permutations > observedStat)  # P-value is very small 
hist(permutations, xlim = c(-15, 15))
abline(v = observedStat)
















