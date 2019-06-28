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
        g <- ggplot(filter(freqData,freq>0), aes(x = parent, y = child))
        g <- g + scale_size(range = c(2, 20), guide = "none")
        g <- g + geom_point(colour="grey50",aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean((y-beta*x)^2)
        g<- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))





































