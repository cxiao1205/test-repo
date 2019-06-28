# Note: codes or words after "#" will be read as explainatory comments in R. R will NOT run / excute anything following a "#" sign.
# This script summarize the common command / functions / tools in R, 


# Coding Standards
# 1. Using text editor
# 2. Indenting: how the flows the program go
# 3. Limit the width of your code. Use ctrl + I to indent. Ideally should choose indenting>=4 
# 4. Limit the function lengths


# Working directory

getwd()

myfunction <- function() {
        x <- rnorm(100)
        mean(x)
}

# use source() if the function was saved on wd

second <- function(x) {
        x + rnorm(length(x))
}

# R Console Input and Evaluation
x <- 1
print(x)
x
msg <- "hello"
x <- 1:20
x

# Data Types
# Atomic claaes: numeric, logical, character, integer, complex
# enter 1 gives you numeric object. 1L gives you an integer
# attributes: properties--names, dimensions, class, length, other user-defined attributes metadata
# attributes() allows you to modify the attributes

# c() function
x <- c(0.5,0.6) # concatenate
x
x <- c(TRUE, FALSE)
x
x <- c(T,F)
x
x <- c("a", "b", "c")
x
x <- 9:29
x
x <- c(1+0i, 2+4i)
x
x <- vector("numeric",length = 10)
x
# if different classes, automatically coerce
y <- c(1.7, "a")
y
y <- c(TRUE, 2)
y
y <- c("a", TRUE)
y

# use as.* functions force coercion

x <- 0:6
x
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

# nonsensical coercion results NAs
x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)
as.complex(x)

# Lists: contain elements of different classes
x <- list(1, "a", TRUE, 1+4i)
x # elements of lists will have double [[]], and elements of vectors will have single []

# Matrices
m <- matrix(nrow =2, ncol =3)
m
dim(m)
attributes(m)
m <- matrix(1:6, nrow = 2, ncol = 3)
m
m <- 1:10
m
dim(m) <- c(2, 5) # change the dimension of m. so from 10 x 1 to 2 x 5
m
x <- 1:3
y <- 10:12
x
y
cbind(x, y)
rbind(x, y)

# Factors: categorical data, ordered / unordered. Dummy variables?
x <- factor(c("yes", "yes", "no", "yes", "no"))
x
table(x)
unclass(x) # degrade?
x <- factor(c("yes", "yes", "no", "yes", "no"),
            levels = c("yes", "no")) # sometimes we want to specify the baseline level. Otherwise just alphabetical
x

# Missing Values: NA / NAN
x <- c(1, 2, NA, 10, 3)
is.na(x)
x <- c(1, 2, NaN, NA, 4)
is.na(x) # return true for both NaN and NA
is.nan(x) # return true if NaN. False if NA

# Data Frames: special case of list (each column can be different data classes)
# attributes: row.names
x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
x
nrow(x)
ncol(x)

# Names attribute
x <- 1:3
names(x)
names(x) <- c("foo", "bar", "norf")
x
names(x)

x <- list(a = 1, b = 2, c = 3)
x

m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))
m

# Reading Tabular Data
# read.table, read.csv
# readLines, text files
# source, read R code
# dget, read R code / in text files
# load, read saved workspaces
# unserialize, read binary form
# for write, write.table, writeLines, dump, dput, save, serialize

# Read Large Datasets. 
# Tricks to shorten the reading time: specify separators, colClasses. 
# e.g. initial <- read.table("datatable.txt", nrows=100) # take a sample data
# classes <- sapply(initial, class) # looping each column by calling class() function
# tabAll <- read.table("datatable.txt", colClasses = classes)
# set nrows, help memory usage

# Textual Formats: not space efficiency
# dump / dput preserve thhe metadata
y <- data.frame(a = 1, b = "a")
dput(y)
dput(y, file = "y.R")
new.y <- dget("y.R")
new.y

x <- "foo"
y <- data.frame(a = 1, b = "a")
x
y
dump(c("x", "y"), file = "data.R")
rm(x, y)
source("data.R")

# Connections 
# open connections: file, gzfile, bzfile, url
str(file)
con <- url("http://www.jhsph.edu","r")
x <- readLines(con)
head(x)

# Subsetting
# []: always the same class as original, 
# [[]], $ 
x <- c("a", "b", "c", "c", "d","a")
x[1]
x[2]
x[1:4]
x[x > "a"]
u <- x > "a"
u
x[u]

x <- list(foo = 1:4, bar = 0.6)
x
x[1]
x[[1]]
x$bar
x[["bar"]]
x["bar"]

x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[c(1, 3)]
name <- "foo"
x[[name]]
x$name
x$foo
x <- list(a = list(10, 12,14), b = c(3.14, 2.81))
x[[c(1, 3)]]
x[[1]][[3]]
x[[c(2,1)]]

x <- matrix(1:6, 2, 3)
x
x[1,2]
x[2,1]
x[1, ]
x[, 2]
x[1,2, drop = FALSE] # if you want to preserve the dimensions

x <- list(aardvark = 1:5)
x$a # partial matching possible
x[["a"]] # no partial matching
x[["a",exact = FALSE]]

x <- c(1, 2, NA, 4, NA, 5) # removing NA values
bad <- is.na(x)
x[!bad]
y <- c("a", "b", NA, "d", NA, "f")
good <- complete.cases(x, y) # logical vector
good
x[good]
y[good]
airquality[1:6, ]
good <- complete.cases(airquality)
airquality[good, ][1:6, ]

x <- 1:4; y <- 6:9 # verctorized operations
x
y
x + y
x > 2
x >= 2
y == 8
x * y
x / y
x <- matrix(1:4, 2, 2); y <- matrix(rep(10, 4),2 ,2)
x
y
x * y
x / y
x %*% y # true matrix multiplication

# Control Structures. if, else, for, while, repeat, break, next, return
# if-else
if(x > 3){
        y <- 10
} else {
        y <-0
}

y <- if(x > 3){
        10
} else {
        0
}

# for
for(i in 1:10){
        print(i)
}

x <- c("a", "b", "c", "d")

for(i in 1:4){
        print(x[i])
}

for(i in seq_along(x)){
        print(x[i])
}

for(letter in x){
        print(letter)
}

for(i in 1:4) print(x[i])

x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))){
        for(j in seq_len(ncol(x))){
                print(x[i, j])
        }
}

# While
count <- 0
while(count < 10){
        print(count)
        count <- count + 1
}

z <- 5
while(z >= 3 && z <=10){
        print(z)
        coin <- rbinom(1, 1, 0.5)
        if(coin == 1){ ## random walk
                z <- z + 1
        } else{
                z <- z - 1
        }
}

# Repeat need to use with break
x0 <- 1 
tol <- 1e-8

repeat{
        x1 <- computeEstimate() # some estimating function
        
        if(abs(x1 - x0) < tol){
                break
        } else {
                
                x0 <- x1
        }
}

# next, return
for (i in 1:100) {
        if(i <= 20) {
                ## skip the first 20 iterations
                next
        }
}

# function
above <- function(x,n = 10){ # default n = 10
        use <- x > n
        x[use]
}
x <- 1:20
above(x,12)
above(x)

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc){
                means[i] <- mean (y[, i], na.rm = removeNA)
        }
        means
}
columnmean(airquality, FALSE)

# functions are first class objects in R. you can pass one function to another

mydata <- rnorm(100)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata) # argument 
sd(na = FALSE, mydata) # argument can be partially matched, if it is unique.

f <- function(a, b ){
        a^2
}
f(2)   # lazy evaluation

f <- function(a, b){
        print(a)
        print(b)
}
f(45) # lazy evaluation, value only evaluated when it is needed

my <- function(x, y, type = "1", ...){ # ... used as pass through. used heavily in generic function
        plost(x, y, type = type, ...)
}

args(paste)
paste("a", "b", sep = ":")
paste("a", "b", se = ":") # you can see when we have ... in argument list, partially matching of arguments input doesn't work

# Scoping Rules
# Symbol Binding---How R bind values to Symbol
lm <- function(x){x * x}
search() # order of matching rules: global environment (user's workspace), then each of the packages on the search list

f <- function(x,y){
        x^2 + y / z    # x and y are formal arguments, and z is free variable (should be defined in search environment)
}

f(1,2)
z <- 11
f(1,2)  # environment is the collection of all symbol value pairs.

make.power <- function(n){ # pass a function to another function
        pow <- function(x){
                x^n
        }
        pow
}
cube <- make.power(3)
square <- make.power(2)
cube(3)
square(3)
ls(environment(cube))
get("n", environment(cube))
ls(environment(square))
get("n", environment(square))

y <- 10 
f <- function(x){
        y <- 2
        y^2 + g(x)
}

g <- function(x){
        x*y
}
f(3) # lexical scoping result is 34

rm(list = ls()) # remove all objects
g <- function(x){
        a <- 3
        x+a+y
}
g(2)
y <- 3
g(2)

# Application: Optimization
make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)){
        params <- fixed # logical, if you want to fixed any argument inputs.
        function(p){
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                b <- -0.5*sum((data-mu)^2) / (sigma^2)
                -(a+b)
        }
}

set.seed(1); normals <- rnorm(100, 1, 2)
nLL <- make.NegLogLik(normals) # construct object function
nLL
ls(environment(nLL))

optim(c(mu = 0, sigma =1),nLL)$par
nLL <- make.NegLogLik(normals, c(FALSE, 2)) # fixing sigma=2. Reconstruct object function
optimize(nLL, c(-1, 3))$minimum
nLL <- make.NegLogLik(normals, c(1, FALSE)) # fixing mu=1. Reconstruct object function
optimize(nLL, c(-1e-6, 10))$minimum
x <- seq(1.7, 1.9, len =100) # generate sequence of value x, between 1.7 and 1.9
y <- sapply(x, nLL) # generate vector y, where values are based on solving function nLL based on different x values.
plot(x,exp(-(y-min(y))), type = "l") # plot 


# Dates and Times in R
# Dates are represented by the Date Class (# of days since 1970-01-01), Times are represented by the POSIXct or the POSIXlt class (# of seconds since 1970-01-01).
# POSIXct: very large integer
# POSIXlt: list---can save date/weekdays, etc. 

x <- as.Date("1970-01-01")
x
unclass(x) # to remove the generic of x?
unclass(as.Date("1970-01-02"))

x <- Sys.time()
x
unclass(x)
x$sec
p <- as.POSIXlt(x)
names(unclass(p))
p$sec

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M") # convert string to time object
x
class(x)

x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
x-y # error because data type is different
x <- as.POSIXlt(x)
x-y

x <- as.Date("2012-03-01"); y <- as.Date("2012-02-28")
x-y
x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT") # specify time zone
y-x

# Loop Functions
# lapply: Loop over a list and evaluate a function on each element
# sapply: Same as lapply but try to simplify the result
# apply: Apply a function over the margins of an array
# tapply:  Apply a function over subsets of a vector
# mapply: Multivariate version of lapply
# split is useful, in conjunction with lapply

# lapply(X, FUN, ...)
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20,1), d = rnorm(100,5))
lapply(x,mean) # loop the mean function over a, b, c, d elements in list x
x <- 1:4
lapply(x, runif) # first argument of runif is # of rv you want to generate.
lapply(x,runif, min = 0, max = 10) # pass argument to runif function
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x
lapply(x, function(elt) elt[,1]) # elt is anonymouse function (not defined anywhere else).

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20,1), d = rnorm(100,5))
sapply(x,mean) # simply the loop results. e.g. compress a list object to a vector
mean(x)

# apply(X, MARGIN, FUN, ...)
str(apply)
x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean) # keep the second dimension, so take the column means
apply(x, 1, mean) # keep the first dimension, so take the row means
# for better performance, use rowSums(= apply(x,1,mean)), rowMeans(=apply(x,1,mean)), colSums(=apply(x,2,sum)), colMeans(=apply(x,2,mean)) instead.
apply(x, 1, quantile, probs = c(0.25,0.75)) 

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10)) # 3 dimentional array
apply(a, c(1, 2), mean) # keep the first two dimentions
rowMeans(a, dims = 2) # give you same result

# mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
str(mapply) # multivariate apply
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1)) # a list of 4 elements. 1. repeat 1 four times 2. repeat 2 three times.....
mapply(rep, 1:4, 4:1) # do the same thing
noise <- function(n, mean, sd){
        rnorm(n, mean, sd)
}
noise(5, 1, 2)
noise(1:5, 1:5, 2) # work as noise(5,5,2)
mapply(noise, 1:5, 1:5, 2) # changing the n and mean in the same time. output list have 5 lists, with diff # of rvs, and diff means. Work as the same as list(noise(1,1,2), noise(2,2,2), noise(3,3,2), noise(4,4,2), noise(5,5,2))

# tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)
x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
f <- gl(3, 10) # generate group / factor identifiers
f
tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE) # if don't want to simply results
tapply(x, f, range) # find group range

# split(x, f, drop = FALSE, ...)
x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
f <- gl(3, 10) # generate group / factor identifiers
f
split(x, f) # create a list, which split x vector based on identifiers f
lapply(split(x,f),mean) # same as tapply
head(airquality)
s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm =TRUE)) # ignore NAs

x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
interaction(f1, f2)
str(split(x, list(f1, f2))) # split automatically generate interaction
str(split(x, list(f1, f2), drop = TRUE)) # drop NA obs

# Debugging Tools in R
# message, warning, error, condition
log(-1) # an example of warning
printmessage <- function(x){
        if(x > 0)
                print("x is greater than zero")
        else
                print("x is less than or equal to zero")
        invisible(x)  # x will not be printed
}
printmessage(1)
printmessage(NA) # an example of error

printmessage2 <- function(x){ # fix error
        if(is.na(x))
                print("x is a missing value!")
        else if(x > 0)
                print("x is greater than zero")
        else
                print("x is less than or equal to zero")
        invisible(x)  # x will not be printed
}
x <- log(-1)
printmessage2(x)

# traceback: prints out the function call stack after an error occurs
# debug: flags a function for "debug" mode which allows you to step through execution of a function one line at a time
# browser: suspends the execution of a function wherever it is called and puts the function in debug mode
# trace: allows you to insert debugging code into a function a specifc places
# recover: allows you to modify the error behavior so that you can browse the function call stack

mean(x) # remove all objects before excute this. will report error
traceback()
lm(y ~ x)
traceback()

debug(lm)
lm(y ~ x) # a new workspce called browser will pop up. It is essentially your function environment. Will scan line by line until the error line can be identified 
options(error = recover) # set global environment---where recover functions will apply when any error occurs
read.csv("nosuchfile")

# The str Function: what is in the object
str(str)
str(lm)
str(ls)
x <- rnorm(100, 2, 4)
summary(x)
str(x)
f <- gl(40, 10)
str(f)
summary(f)
head(airquality)
str(airquality)
m <- matrix(rnorm(100), 10, 10)
str(m)
s <- split(airquality, airquality$Month)
str(s)

# Simulation - Generate Random Numbers
# d for density, r for random number generation, p for cumulative distribution, q for quantile function. (e.g. dnorm, rnorm...)
x <- rnorm(10)
x
x <- rnorm(10, 20, 2)
summary(x)
set.seed(1)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5) # seed is important. not real random
rpois(10, 1) # poisson distribution
rpois(10, 2)
rpois(10, 20)
ppois(2, 2) # Pr(x <= 2)
ppois(4, 2) # Pr(x <= 4)
ppois(6, 2) # Pr(x <=6 )

# Simulating a Linear model
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2*x + e
summary(y)
plot(x, y)

set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2*x + e
summary(y)
plot(x, y)

set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y)

# Random Sampling
set.seed(1)
sample(1:10, 4)
sample(1:10, 4)
sample(letters, 5)
sample(1:10) # I guess it's shuffling
sample(1:10, replace = TRUE) # allow multiple draws

# R Profiler
# Profiling is a systematic way to examine how much time is spend in different parts of a program.
# Design first, then optimize. root of all evil
# use multiple cores. 
# system.time
system.time(readLines("http://www.jhsph.edu")) # return proc_time object
#user  system elapsed 
#0.14    0.00    0.75         # elapsed time > user time (program time). 
hilbert <- function(n){
        i <- 1:n
        1/outer(i - 1, i, "+") # outer product
}
x <- hilbert(1000)
system.time(svd(x))# singular value decomposition, take advantage of multicores. 
#user  system elapsed 
#2.11    0.05    2.20       # elapsed time <- user time(if multiple cores) 
system.time({
        n <- 1000
        r <- numeric(n)
        for (i in 1:n){
                x <- rnorm(n)
                r[i] <- mean(x)
        }
})   # almost same time
# Rprof(), printout function call stack. default sampling interval is 0.02 seconds.
# summaryRprof(), list by time
# DO NOT use system.time() and Rprof() together
