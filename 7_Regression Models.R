# Regression is the most fudamental topic for the data scientist.
# I personally recommend finished reading at least one of the econometrics text books. Entry level books include Stock and Watson or Wooldridge. Greene is for PhD or theoretical researchers
library(data.table)
library(dplyr)
# Least Square regression
library(UsingR); data(galton); library(reshape); long <- melt(galton)  # take the child v.s. parent data as example. reshape is useful to change the data from wide format to long format (tidy)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g+geom_histogram(colour = "black", binwidth = 1) + facet_grid(.~variable)
g  # histogram of child height and parent height

library(manipulate)  # interactive (GUI) with the plot. if no additional info, then the sample mean is the best estimator
myHist <- function(mu){ # in this case, mu can be changed
        mse <- mean((galton$child - mu)^2)  # mean squared error
        g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth = 1)
        g <- g + geom_vline(xintercept = mu, size = 3)
        g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5)) # now click on the gear button we can change the value of mu and the MSE will updated automatically.

y <- galton$child - mean(galton$child)  # manipulate code for regression line shlop can be changed
x <- galton$parent - mean(galton$parent)
freqData <- as.data.table(table(x, y))
names(freqData) <- c("child","parent","freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData,freq>0), aes(x = parent, y = child)) #basic layer
        g <- g + scale_size(range = c(2, 20), guide = "none") # ajust the scale radius (for each point)
        g <- g + geom_point(colour="grey50",aes(size = freq+20, show_guide = FALSE)) # outter part of the points
        g <- g + geom_point(aes(colour=freq, size = freq)) # inner part of the points
        g <- g + scale_colour_gradient(low = "lightblue", high="white") # add scale gradient (as legend)
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3) # fitted line
        mse <- mean((y-beta*x)^2) # calculate the mse
        g<- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3))) # show beta and mse values
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02)) # to see dynamic change of MSE given different beta

lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)  # regress on "centered" data

# ls regression example
library(UsingR)
data(galton)
library(dplyr); library(ggplot2) # load packages and data
freqData <- as.data.frame(table(galton$child, galton$parent)) # frequant count table
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character((freqData$child)))
freqData$parent <- as.numeric(as.character((freqData$parent)))
g <- ggplot(filter(freqData,freq>0), aes(x = parent, y = child)) #basic layer
g <- g + scale_size(range = c(2, 20), guide = "none") # ajust the scale radius (for each point)
g <- g + geom_point(colour="grey50",aes(size = freq+20, show_guide = FALSE)) # outter part of the points
g <- g + geom_point(aes(colour=freq, size = freq)) # inner part of the points
g <- g + scale_colour_gradient(low = "lightblue", high="white") # add scale gradient (as legend)
g

y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x))) # you can see the coef estimated by formula is the same as by built-in formula
yc <- y - mean(y) # centered y 
xc <- x - mean(x) # centered x
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2]) # the centered sample estimation is the same as the raw
lm(formula = yc ~ xc - 1)  # -1 means get rid of the intercept. the result is the same. regression to the origin

g <- g + geom_smooth(method= "lm", formula = y ~ x) # add regression line layer
g

# regression to the mean: e.g. why is it that children of tall parents tend to be tall, but not as tall as their parents? why is it that children of short parents tend to be short, but not as short as their parents?
x <- rnorm(100)
y <- rnorm(100)
odr <- order(x)
x[odr[100]] # The maximum x
y[odr[100]] # the y paried with x
        # P(Y < x | X = x) gets bigger as x heads into the very large values
        # Regression to the mean is a clinical phenomenon---like mean reverting in stock returns


# Linear Regression for Prediction
library(UsingR); data(diamond)
library(ggplot2)
g <- ggplot(diamond, aes(x = carat, y = price))
g <- g + xlab("Mass (carats)")
g <- g + ylab("price (SIN $)")
g <- g + geom_point(size = 6, colour = "black", alpha = 0.2)
g <- g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g <- g + geom_smooth(method = "lm", colour = "black")

fit <- lm(price ~ carat, data = diamond) # returns a list
coef(fit)
fit2 <- lm(price ~ I(carat -mean(carat)), data = diamond)
coef(fit2) #slope stays the same. but intercept is the expected price of the average sized diamond. 
newx <- c(0.16, 0.27, 0.34) # forecasting the price based on size
predict(fit, newdata = data.frame(carat = newx))
predict(fit)  # prediction based on observed values

# Residuals
        # In econometrics, normal distributed residuals is not a necessary assumption for linear regression models to work. However, normality of residuals does gurantees that the regression model is the best linear unbiased estimator. (BLUE)
        # useful for investigating poor model fit
        # bottom line: residuals should exhibit no pattern (white noise is good.)
        # Use of R square should be accompanied by other methods, like residual diagnose and simple scatter plot.
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y - yhat)))   # resid function returns the same residual as we calculate manually
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))
sum(e)  # almost 0
sum(e * x)  # almost 0
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE
     )
abline(fit, lwd = 2)
for (i in 1 : n)
        lines(c(x[i],x[i]), c(y[i], yhat[i]), col = "red", lwd = 2) # connect the y and yhat

plot(x, e, # now plot the residuals
     xlab = "Mass (carats)",
     ylab = "Residuals (SIN $)",
     bg = "lightblue",
     col = "black", cex = 2, pch = 21, frame = FALSE
)
abline(h = 0, lwd = 2)
for (i in 1 : n)
        lines(c(x[i],x[i]), c(e[i], 0), col = "red", lwd = 2)

par(mfrow = c(2, 3))
# Adjustment: adjustment may cause the fact to change! 
        # First simulation. The difference between group red and blue show the marginal effect disregarding the x. Essentially it's a linear regression has the form: y=b0+b1*x+b2*t+u. where t is a dummy. The plot assumes the slope b2 are the same for both group (t=0 and t=1).
        # in this plot, we have observations in different groups have similiar value of x. It makes the comparison (across groups) more reasonable and straight forward.
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


        # Second simulation
        # in this simulation, although the y mean of different groups (t=0 and t=1) are different, the treatment effect (b2 in y=b0+b1*x+b2*t+u) is really small (the diff of intercept between two reg lines). Plus, bo objects in group 0 and 1 have similar x
        # this makes the comparison really hard: x is probabily one of the reason why objects go to different groups (we clearly didn't randomize). 

n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


        # Third simulation 
        # this case is a better case than 2. we could find some overlaps (obs with similar x values in group 0 and group 1), which makes experiment design looks better. However, the y mean of group blue is lower than group red, but the blue regression line is higher than the red. that is to say, our adjust estimate is significant and the exact opposite of our unadjusted estimate. It's usually called Simpson's Paradox

n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


        # Fourth simulation
        # In this example, there is almost no marginal effect (group y means are almost the same), however, if we adjust for x we can see a significant treatment effect. (significant b2)
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)




        # Fifth simulation
        # we will get wrong model if we assume the same slope. the correct model setting for this case is y=b0+b1*x+b2*t+b3*x*t+u. In this case, the treatment effect depends on what level of x you're at.
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1)); 
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


        # Six simulation
        # In this case, the treatment effect is a continous variable, rather than a binary variable. Higher lighter values means higher, and more red darker values means lower. 
        # in the 3-d plot, you could see it clearly that most of the variations in y are explained by the value of x2.
        # samething in the continuous case. The adjustment may cause your facts to change in your model.
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2 
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

par(mfrow=c(1,1))
library(rgl)
plot3d(x1,x2,y) # 3d plot

# Residuals and Diagnostics


        # Do `?influence.measures` to see the full suite of influence measures in stats. The measures include
        # `rstandard` - standardized residuals, residuals divided by their standard deviations)
        # `rstudent` - standardized residuals, residuals divided by their standard deviations, where the ith data point was deleted in #the calculation of the standard deviation for the residual to follow a t distribution
        # `hatvalues` - measures of leverage
        # `dffits` - change in the predicted response when the $i^{th}$ point is deleted in fitting the model.
        # `dfbetas` - change in individual coefficients when the $i^{th}$ point is deleted in fitting the model.
        # `cooks.distance` - overall change in the coefficients when the $i^{th}$ point is deleted.
        # `resid` - returns the ordinary residuals
        # `resid(fit) / (1 - hatvalues(fit))` where `fit` is the linear model fit returns the PRESS residuals, i.e. the leave one out cross validation residuals - the difference in the response and the predicted response at data point $i$, where it was not included in the model fitting.

data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21) # if added to the regression, will not be considered as an outlier
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21) # outlier if added, but less leverage than the 4th point
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21) # outlier if added, but very close to the reg line, so less leverage
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21) # large leverage if included.


        # Case 1
        # This case is an example that how single outlier will tilt your regression line.
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  # strong correlation exists merely because the outlier (10,10)  
fit <- lm(y ~ x) # Now try to diagnose
round(dfbetas(fit)[1 : 10, 2], 3)   # regression deletion diagnostics (dfbetas, leave one out deletion)
round(hatvalues(fit)[1 : 10], 3)    # hat values should be between 0 and 1. it's a measurement of leverage for each data point

        # Case 2
        # This case shows you a special case when outlier adhere to your regression line pretty well
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2) 
round(dfbetas(fit2)[1 : 10, 2], 3) # diagnose --- dfbetas tells you nothing
round(hatvalues(fit2)[1 : 10], 3) # hatvalues still can tell you that there is an outlier here. 

        # a cute example to show why look into residules is important.
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)  # scatter plot. Not so useful
summary(lm(V1 ~ . -1, data = dat))$coef   # a linear reg fit. -1 means we want to remove the intercept
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.') # if we plot the residule we can obviously see a bird! which is certain a pattern (we may not able to explain though.)
data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit) # plot fit will return some automatic plots.

# Model selection: Which variables to include?
        # underfit (e.g. omitted variables)/overfit (too many inputs)
                # If we correctly or overfit the model, including all necessary covariates and/or unnecessary covariates, the variance estimate is unbiased. However, the variance of the variance is larger if we include unnecessary variables (consider dimension reduction tools like PCA).
        # Automatically model selection (check machine learning sector).
        # Randomization is the key. Experimental design is a quite big field. 
        # looking into the nested models (e.g. add regressors one by one, but new model is based on old model). use anova function to compare across models. 
        # "All models are wrong, but some of them are useful"



# Generalized Linear Models (GLM)
        # Some problem of lienar models: consider you have a vavriable has to be positive. however, your normality assumption will make the prediction negative with some probability, that is problematic! Also, what is the point to add a dummy variable into our model? why addictive?
          # Transformations are often hard to interpret
          # There's value in modeling the data on the scale that it was collected.
          # Particularly interpetable transformations, natural logarithms in specific, aren't applicable for negative or zero values. 

        # Three components: 1. An exponential family model for the response (e.g. normal, poisson, etc.) 2. A systematic component via a linear predictor. 3. A link function that connects the means of the response to the linear predictor. 
        # Check Nelder, J.A. and Wedderburn, R.W., 1972. Generalized linear models. Journal of the Royal Statistical Society: Series A (General), 135(3), pp.370-384.
        # Example 1: Assume Y ~ N(mu, sigma^2), how to etimate (model for) mu? So 1. we have a normal distribution for the response, and 2. define the linear predictor to be ita(i)=sum(X(i,k)*Beta(k)), and 3. Thie link function as g so that g(mu)=ita. In this case g(mu)=mu, so that mu=ita. A simple approach for max likelihood linear regression model!
        # Example 2: logistic regression. 1. Assume Y ~ bernoulli(mu), so that E(Y)=mu, where 0<=mu<=1. 2. Linear predictor to be ita(i)=sum(X(i,k)*Beta(k)), and 3. the link function in this case is logit function: g(mu)=ita=log(mu/(1-mu)), and g() is the natural log odds. Essentially we are trasform the mu (not the Y itself) into a form that could be interpreted by standard linear model.
        # Example 3: Poisson regression: 1. Assume that Y ~ Poisson(mu), so that E(Y) = mu, where 0<=mu,and 2. Linear predictor to be ita(i)=sum(X(i,k)*Beta(k)), and 3. Link function g(mu)=ita=log(mu)
        # So the original approach to derive a linear model is through minimize the least square. GLM actually generalized the derivation of the linear models by maximizing the likelihood.
        # For variance: Linear model Var= sigma^2 as constant. Bernoulli var=mu*(1-mu). Poisson var = mu. Please note the relationship between mu and var may not hold in practice! (one solution is to use quasi- models to make more flexible variance assumptions.)
        # Please note that all model setting ups for GLM are based on asymptotic assumptions. Hence for small samples the model may not work. 

# Logistic Regression: Y is a binomial variable.
setwd("C:/Users/Xiao.Cheng/Desktop/R Code/Data Science Specialization_Coursera")

# download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda", destfile="./data/ravensData.rda")  # restricted
load("./data/ravensData.rda")
head(ravensData)
lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)  # linear model for a quick check, but not a reasonable fit
summary(lmRavens)$coef

        # Odds = P/(1-P), so P = Odds/(1+Odds). Modeling the odds: log(odds) = b0 + b1*RS  and then we can get the P. 
        # in this case, b0 is the log odds of a Ravens win if they score zero points
        # b1 is the log odds of win prob for each point scored, compared to zero points
        # exp(b1) is the odds ratio of win prob for each point scored, compared to zero points. 
        # one way to see the slope is compare b0 + b1*(RS + 1) - bo + b1*(RS + 1) = b1. 
        # BTW, the concept of odds comes from the fair game: flip a coin with head prob p, if head you win X dollars, if tail you lose y dollars. so what relationship among x, y, p will give you expectation $0? so E=p*x+(1-p)*(-y)=0, hence y/x = p/(1-p) is the odds. so odds can be interpreted as how much should you be willing to pay for a p prob of winning?

logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family = "binomial")
summary(logRegRavens)
par(mfrow = c(1, 1))
plot(ravensData$ravenScore,logRegRavens$fitted,pch = 19, col = "blue", xlab = "Score", ylab = "Prob Ravens Win")
exp(logRegRavens$coeff)
exp(confint(logRegRavens))  # contains b1 = 0, or e^b1=1 within the CI. So the b1 is not significant
anova(logRegRavens,test="Chisq")

# Count Data: Poisson Regression
        # Data may also be in the form of rates. Linear Regression with transformation is an option.
        # X ~ Poisson(t*Lambda) if P(X = x) = (t*lambda)^x*exp(-t*lambda)/x!, for x = 0, 1, 2, ...
        # The mean of the Poisson is E[X] = t*lambda, or E[X/t] = lambda. The variance is Var(X)=t*Lambda. The poisson tends to a normal as t*lambda gets large

par(mfrow = c(1, 3)) # a simulation example of poisson regression
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE)

x <- 0 : 10000; lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

load("./data/gaData.rda") # the datafile was downloaded from Dr. Peng's github page
gaData$julian <- julian(gaData$date) # count # of days since 1970/1/1
head(gaData)  # so we have aggregated number of visits (to the website) since 1970/1/1, as well as the accumulated # of days
par(mfrow = c(1, 1))
plot(gaData$julian,gaData$visits,pch = 19, col = "darkgrey", xlab = "Julian", ylab = "Visits")
lm1 <- lm(gaData$visits ~ gaData$julian) # univariate linear regression model. 
abline(lm1, col = "red", lwd = 3)
        # now think about log transformation of the # of visits. now e^E[log(Y)] is the geometric mean of Y.
        # Log-linear setting up is actually could be interpreted as Poisson model. So E[Number of hits | b0, b1, julian] = exp(b0 + b1*julian) = exp(b0)*exp(b1*julian)
plot(gaData$julian,gaData$visits,pch = 19, col = "darkgrey", xlab = "Julian", ylab = "Visits")
glm1 <- glm(gaData$visits ~ gaData$julian, family = "poisson")
abline(lm1, col = "red", lwd = 3); lines(gaData$julian,glm1$fitted, col = "blue", lwd = 3)   # red is the linear fit. blue is the log-linear/poisson fit
plot(glm1$fitted, glm1$residuals, pch = 19, col = "grey", ylab = "Residuals", xlab = "Fitted")  # if we plot the fitted value with the residuals, it doesn't show that higher mean is asscociate with the higher variance. So it is not really a Poisson fit---need model agnostic standard errors. 
        # for rates, use log(E[rate])= log(Y) + b0 + b1*X
        # for Poisson regression, be careful with 0 inflation. 
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)

        
glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),col="grey",xlab="Date",
     ylab="Fitted Rates",pch=19)
lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)

# Hedgepodge: some additional notes for linear models. 
        # Thinking about that we need to fit: Y = f(X) + u
        # e.g. Y(i) = b0 + b1*X(i) + sum{I[(x(i)-ksi)>0]*(x(i)-ksi)}*gamma(k) + u(i)  like linear interpolation. (spline)
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)   # simulate x and y. 
knots <- seq(0, 8 * pi, length = 20)  # simulate a bunches of knots
splineTerms <- sapply(knots, function(knot) (x > knot)*(x - knot)) # create knot terms. then we have a matrix of knot terms
xMat <- cbind(1, x, splineTerms) # x matrix is the combine of intercept (all 1) and the knot terms matrix
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
        # what if we square the knot term? The fit becomes much smoothier!
splineTerms <- sapply(knots, function(knot) (x > knot)*(x - knot)^2) 
xMat <- cbind(1, x, splineTerms) 
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

        # The collection of regressors is called a basis. The example just above is called the spline basis. The key is you know where the knot points are, and you could manually setup these points as reflection points.
        # Now let's use fourier basis as another example.
notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25) # Harmonics using linear models example. 
t <- seq(0, 2, by = .001); n <- length(t)
c4 <- sin(2 * pi * notes4[1] * t); e4 <- sin(2 * pi * notes4[3] * t); 
g4 <- sin(2 * pi * notes4[5] * t)   # Create notes. 
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3) # Create Chords. 
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
fit <- lm(chord ~ x - 1) # Can R guess the choard? discrete fourier transform! (fits the linear model, with a lot of sin and cosin terms as regressors.)
plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")
a <- fft(chord); plot(Re(a)^2, type = "l")



























