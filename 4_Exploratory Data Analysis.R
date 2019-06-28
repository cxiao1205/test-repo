# This section focuses on things we could do before starting modeling. At least we should have some ideas on the data we are working with before we actually build some models.
# Graphics, plots, hierarchical clustering, K-means clustering and dimension reduction

library(beepr)
getwd()
setwd("C:/Users/Xiao.Cheng/Desktop/R Code/Data Science Specialization_Coursera")
# Principals of Analytic Graphics
# 1. Show comparisons. i.e. null hypothesis vs alternative hypothesis
# 2. Show causality, explanation, systematic structure
# 3. Show multivariate data. e.g. to avoid Simpson's paradox
# 4. Integration of evidence (e.g. publish paper tables and charts)
# 5. Describe and document the evidence with appropriate labels, scales, sources, etc. 
# 6. Content is king

# Exploratory Graphs
fileUrl <- "https://raw.githubusercontent.com/rdpeng/courses/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv" # sample data file available on github, Dr. Peng's page. Please download the raw file, not the data page (html)
download.file(fileUrl, destfile = "./data/avgpm25.csv") # if MAC, add methond = "curl". download 

pollution <- read.csv("./data/avgpm25.csv", colClasses = c("numeric","character","factor","numeric","numeric"))
head(pollution)
summary(pollution$pm25) # six number summary
summary(pollution)
boxplot(pollution$pm25, col = "blue") # simple boxplot
abline(h=12) # add a line at 12
hist(pollution$pm25, col = "green") # simple histogram
rug(pollution$pm25) # plots all the points below the histogram. see the clusters distribution
hist(pollution$pm25, col = "green", breaks = 100) # smaller bars intervals
rug(pollution$pm25) 
hist(pollution$pm25, col = "green") # smaller bars intervals
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)  # v means vertical line. col is color, lwd is the line width
barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region") # simple barplot

boxplot(pm25 ~ region, data = pollution, col = "red") # multiple boxplots
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) # combining plots. show plots on a "matrix" view with two rows, and 1 column. mar helps to set up the margins
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
par(mfrow = c(1,1)) # reset the default multi plots combination settings. c(1,1) means we don't combine the plots
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2) # lty sets up the line type
with(pollution, plot(latitude, pm25, col = region)) # separate the plot by different colors based on the region variable
abline(h = 12, lwd = 2, lty = 2) 
par(mfrow = c(1,2), mar = c(5, 4, 2, 1)) # Again, combine the following two plots
with(subset(pollution,region == "west"), plot(latitude, pm25, main = "West")) 
with(subset(pollution,region == "east"), plot(latitude, pm25, main = "East"))

# Plotting Systems in R
# 1. The Base Plotting System: "Artist's palette" model. start with blank canvas and build up from there. Plot + annotate functions (like title, label, etc.) .
# 2. The Lattice System: Plots are created with a single function call.
# 3. The ggplot2 System: splits difference between base and lattice in a number of ways
# note the packages related usually are automatically loaded when you started R

# The Base Plotting System
par(mfrow = c(1,1)) # reset the default multi plots combination settings. c(1,1) means we don't combine the plots
?par # help page for plot settings
hist(airquality$Ozone) # histogram of Ozone
with(airquality, plot(Wind, Ozone)) # default scatter plot
airquality <- transform(airquality, Month = factor(Month)) # turn Month variable into factor variable
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)") # separate boxplot the Ozone by Month. 
# pch = the plotting symbol (default is open circle)
# lty = the line type (default is solid line)
# lwd = line width
# col = color
# xlab = x axis label
# ylab = y axis label
# we can use par() function to specify global graphics parameters
# las: the orientation of the axis labels on the plot
# bg: the background color
# mar: the margin size
# oma: the outer margin size (default is 0)
# mfrow: number of plots per row, column (plots are filled row-wise)
# mfcol: number of plots per row, column (plots are filled column-wise)
par("lty") # you can check the default 
par("col")
par("pch")
par("bg")
par("mar")  # start with the column, go clockwise until 4 directions. so 4 numbers.
par("mfrow")

# Base Plotting Functions
# plot: scatterplot
# lines: add lines to a plot, given a vector x values and a corresponding vector of y values. Connects the dots
# points: add points
# text: add text labels to t aplot using specified x, y coordinates
# title: add annotations to x, y axis labels, title, subtitle, outer margin
# mtext: add arbitrary text to the margins
# axis: adding axis ticks/labels
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City") # Add a title
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City")) # put title directly
with(subset(airquality, Month ==5), points(Wind, Ozone, col = "blue")) # mark month 5 data blue

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n")) # doesn't plot anything
with(subset(airquality, Month ==5), points(Wind, Ozone, col = "blue")) # mark month 5 data blue
with(subset(airquality, Month !=5), points(Wind, Ozone, col = "red")) # mark other months red
legend("topright",pch = 1, col = c("blue", "red"), legend = c("May", "Other Months")) # set legend (explain what does the circles stand for). put the legend on the top right corner, symbols are default (circles), colors are blue and red, and legend reads as May and Other Months from top to down
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20)) # again, pch is the symbol we use to plot each obs
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2) # we can add regression line directly on the scatter plot

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0)) # setup the multiple plotting as well as margins and outter margins
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
        plot(Temp, Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in New York City", outer = TRUE) # Add title into the panel plots
})

# Base Plotting Demonstration
par(mfrow=c(1,1)) 
x <- rnorm(100)# generate 100 random numbers
hist(x)
y <- rnorm(100)# generate 100 random numbers
plot(x,y) # scatter plot of x and y

par(mar = c(2, 2, 2, 2)) # we can setup the margins (number of lines leaves blank on each side. Bottom, left, up, and right)
plot(x,y) # in thie case, our labels will be missing since the bottom and left margins are too small

par(mar = c(4, 4, 2, 2)) 
plot(x,y) # now it's better

plot(x, y, pch =20) # change the plot dot to solid circle. Check ?pch for more info
example(points) # we can actually check the example files for points. for # 21 to 25, we can specify the boundry colors and fill colors.

x <- rnorm(100)# generate 100 random numbers
y <- rnorm(100)# generate 100 random numbers
plot(x, y, pch =20) # change the plot dot to solid circle. Check ?pch for more info
title("Scatterplot") # add title
text(-2, -2,"Label")
legend("topleft", legend = "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit)
abline(fit, lwd = 3, col = "blue")

plot(x, y, xlab = "Weight", ylab = "Height", main = "Scatterplot", pch = 20)
legend("topright", legend = "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit, lwd = 3, col = "red")

z <- rpois(100, 2)
par(mfrow = c(2, 1))
plot(x, y, pch = 20)
plot(x, z, pch = 19) # too large margin
par(mar = c(2, 2, 1, 1))
plot(x, y, pch = 20)
plot(x, z, pch = 19) # better margin

par(mfrow = c(1, 2), mar = c(4,4,2,2))
plot(x, y, pch = 20)
plot(x, z, pch = 19) 

par(mfrow = c(1, 1))
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50, labels = c("Male", "Female"))
str(g)
plot(x, y)
# plot different factors in diff colors
plot(x, y, type = "n") # initialize but null plot
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue")

# Graphic Device--plots must be created on a graphics device
# See ?Devices for more information
# How Does a plot Get Created?
# 1. use plot function.
# 2. use file devices
pdf(file = "myplot.pdf")  # Open PDF device; create 'myplot.pdf' in my working directory
with(faithful, plot(eruptions, waiting))  # in R, this way should show nothing on the screen However, RStudio visualize the steps
title(main = "Old Faithful Geyser data")
dev.off() # now under your working directory you should find a plot pdf file called myplot.pdf
# vector devices. e.g. pdf, svg, win.metafile, postscript
# bitmap devices. e.g. png, jpeg, tiff, bmp
dev.cur()
# try to copy a plot into another png file
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.copy(png, file = "geyserplot.png")
dev.off() # now you should see png file under the wd. 

# Lattice Plotting System
# useful when high dimensional plot / high density plots
# xyplots: main function for creating scatterplots
# bwplot: box-and-whiskers plots
# histogram: histograms
# stripplot: like a boxplot but with actual points
# dotplot: plot dots on "violin strings"
# splom: scatterplot matrix; like pairs in base plotting system
# levelplot, contourplot: for plotting "image" data
xyplot(Ozone ~ Wind, data = airquality)
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1)) # plot conditional on month variable (month as categorical variable)
p <- xyplot(Ozone ~ Wind, data = airquality) # can save the xyplot as an object "trellis"
print(p)

library(grid)
library(lattice)
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f*x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2,1)) # latiice panel functions. Plot with 2 panels. 
xyplot(y ~ x | f, panel = function(x, y, ...){  # panel sets up the custom panel function
        panel.xyplot(x, y, ...)  # call the default panel function for xyplot
        panel.abline(h = median(y), lty = 2)  # Add a horizontal line at the median
})

xyplot(y ~ x | f, panel = function(x, y, ...){  # panel sets up the custom panel function
        panel.xyplot(x, y, ...)  # call the default panel function for xyplot
        panel.lmline(x, y, col = 2)  # Add a regression line
})

# ggplot2 plotting system
# Grammar of Graphics
# mapping from data to aesthetic attributes (color, shape, size) of geometric objects (points, lines, bars).
library(ggplot2)
str(mpg)  # use mpg dataset as an example. Fuel economy data from 1999 and 2008 for 38 popular models of car
qplot(displ, hwy, data = mpg) # Hello world ploting, hwy v.s. displ
qplot(displ, hwy, data = mpg, color = drv) # choose drv as a factor, print different drv level data in diff colors.
qplot(displ, hwy, data = mpg, geom = c("point","smooth")) # show the point and smooth curve, with 95% confidence interval. Other options includes density
qplot(hwy, data = mpg, fill = drv) # histogram, show different drv level in different colors.
qplot(displ, hwy, data = mpg, facets = .~drv) # split plots by different drv. . means no different row figures
qplot(hwy, data = mpg, facets = drv~.,binwidth = 2) # split histograms by different drv. . means no different column figures

# what do you need: A data frame, Aesthetic mappings (for color, sieze), geoms (geometric objects like points lines shapes), facets (for conditional plots), stat (for statistical transformations like binning, quantiles, smoothing), scales (what scale an aesthetic map uses), coordinate system
# like the artist's palette

# qplot(logpm25, NocturnalSympt, data = maacs, facets = .~bmicat, geom = c("point", "smooth"), method = "lm")  # read as: initialize a two-way scatterplot of logpm25 and NocturnalSympt, dataset is maacs, separate the plot into a matrix plotting, and the factor (category) is bmicat, by columns. the geom we choose is point and smoother (like a curve), and the method we use is a linear regression model

# e.g. g <- ggplot(maacs, aes(logpm25, NocturnalSympt))  # no layers in plot yet.
#  p <- g + geom_point()   #  geom_point()  tells you the layers
#  g + geom_point() + gem_smooth() # add smoother also 
#  g + geom_point() + gem_smooth(method = "lm")  # make the smoother a lm regression line
#  g + geom_point() + facet_grid(. ~ bmicat) + gem_smooth(method = "lm")  # add facet feature (matrix plotting, or diff levels)
#  for annotation: xlab(), ylab(), labs(), ggtitle()
#  for globally setup, use theme(). e.g. theme(legend.position = "none) .   The default theme: theme_gray() and More start/plain: theme_bw()
# g + geom_point(color = "steelblue", size = 4 , alpha = 1/2)  # steelblue is a constant color value. alpha is the transparence level.
# g + geom_point(aes(color = bmicat), size = 4 , alpha = 1/2) # color based on the value of bmicat variable. 
# g + geom_point(aes(color = bmicat)) + labs(title = "MAACS Cohort) + labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")  # setup the labels also 
# + geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)  # can setup for the smoother. se = FALSE to turn down the Confidence intervals
# + theme_bw(base_family = "Times") # change the theme to BW, and Font to Times

testdat <- data.frame(x = 1:100, y = rnorm(100)) # gen testdata, x and y
testdat[50,2] <- 100 # setup an outlier for y
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3)) # plot the test dat, x versus y, type y means a line plot. set y axis limit between -3 and 3
g <- ggplot(testdat, aes(x = x, y = y)) # setup ggplot
g + geom_line() # layers like geom_line()
g + geom_line() + ylim(-3,3)  # note: ggplot will exclude anything beyond (-3, 3)
g + geom_line() + coord_cartesian(ylim = c(-3, 3))  #use the coor_cartesian function to include outliers also.

# cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length = 4), na.rm = TRUE) # calculate the deciles of the data
# maacs$no2dec <- cut(maacs$logno2_new, cutpoints) # cut NO2 into 3 categories
# g <- ggplot(maacs, aes(logpm25, NocturnalSymt))
# g + geom_point(alpha = 1/3)  # add points
#   + facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4)  # Make panels
#   + geom_smooth(method="lm", se=FALSE, col="steelblue") # add smoother
#   + theme_bw(base_family = "Avenir", base_size = 10) # change theme
#   + labs(x = expression("log " * PM[2.5]))
#   + labs(y = "Nocturnal Symptoms")
#   + labs(title = "MAACS Cohort")    # add labels

# Hierarchical clustering (sensitive to scaling, outliers)
# how to visualize high dimentional data?
# An agglomerative approach. Find closest two things, put them toghether, find next closest. Need to define distance, and a merging approach. Prodeces a tree showing how close things are to each other
# distant metric: euclidean, Manhattan (like shortest path search---need to walk along the city blocks)

set.seed(1234)
par(mar = c(0, 0, 0, 0))  # set margin
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)      # create 12 random numbers, with 3 diff group mean (1, 2, 3)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2) # create 12 random numbers, with actually 2 diff group mean (1,2)
plot(x, y, col = "blue", pch = 19, cex = 2)   # plot each xy pair
text(x + 0.05, y + 0.05, labels = as.character((1:12))) # label each point
dataFrame <- data.frame(x = x, y = y)
dist(dataFrame) # distant matrix
# Find the closest two points. In this case point 5 and point 6. Merge them together
# Find the next closest two points. In this case 10 and 11. Merge as super points.
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
par(mar = c(6, 4, 2, 2))
plot(hClustering) # decision tree / cluster dendrogram. You have to cut this tree at some certain points to decide how many clusters.
# following is myplclust function, a user defined function available online
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
        ## modifiction of plclust for plotting hclust objects *in colour*!
        ## Copyright Eva KF Chan 2009
        ## Arguments:
        ##    hclust:    hclust object
        ##    lab:        a character vector of labels of the leaves of the tree
        ##    lab.col:    colour for the labels; NA=default device foreground colour
        ##    hang:     as in hclust & plclust
        ## Side effect:
        ##    A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height,2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x<0)]
        x <- x[which(x<0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot( hclust, labels=FALSE, hang=hang, ... )
        text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}

hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))
# how to cluster?
# 1. average linkage: merge two point, measure new superpoint as the average of x and y coordinate (like gravity)
# 2. Complete Linkage: take futherst dist in two clusters
# use heatmap() function to visualize large matrix
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)

# K-means Clustering
# Assign centriod to each cluster (you need some basic ideas of how many clusters: K)
# Recalculate centroids
set.seed(1234)
par(mar = c(0, 0, 0, 0))  # set margin
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)      # create 12 random numbers, with 3 diff group mean (1, 2, 3)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2) # create 12 random numbers, with actually 2 diff group mean (1,2)
plot(x, y, col = "blue", pch = 19, cex = 2)   # plot each xy pair
text(x + 0.05, y + 0.05, labels = as.character((1:12))) # label each point
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)   # is a kmeansObj, which is a list. Assume 3 centers
names(kmeansObj)
kmeansObj$cluster

par(mar = rep(0.2, 4)) # set plot margin
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)  # cex used to magnify the point (pch)
points(kmeansObj$centers, col = 1:3, pch = 3, lwd = 3) # plot the centers

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")  # heatmap
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

# Dimension Reduction
set.seed(12345)
par(mar = rep(0.2, 4), mfrow = c(1,1))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])   # heat map for 10 columns and 40 rows
heatmap(dataMatrix)
# add some patterns
set.seed(678910) 
for (i in 1:40){
        # flip a coin
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip){
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)  # add 0 and 3 to existing columns if heads
        }
}
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)  # columns shows significant trend---5 on the left and 5 on the right. But row shows no significant trend since we add no pattern to rows (or random)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1,3),mar = rep(4, 4))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

# if You have multivariate variables X1,...Xn, so X1 = (X11,...,X1m), you try to :
# 1. Find a new set of multivariate variables that are uncorrelated and explain as much variance as possible (statistical)
# 2. If you put all the variables together in one matrix, find the best matrix createed with fewer variables (lower rank) that explains the original data (data compression)

# Singular value decomposition
svd1 <- svd(scale(dataMatrixOrdered)) # svd object, a list of u, d, v (X=UD(V^T)), where the columns of U are orthogonal (left singular vectors), the columns of V are orthogonal (right singular vectors), and D is a diagonal matrix (sigular values). If you scaled each column in X (standardized the variables) first, then PCA = V
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1,xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19) # variance explaination. In this case, 40% of data variations could be explained by the first singular value
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)  # PCA object.
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0, 1))

set.seed(678910)
dataMatrix <- matrix(rnorm(400), nrow = 40)
for (i in 1:40){
        # flip a coin
        coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
        coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
        # if coin 1 is heads, add a common pattern to that row
        if(coinFlip1){
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)  # if heads, add 0 0 0 0 0 5 5 5 5 5 to the row
        }
        if(coinFlip2){
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)  # if heads, add 0 5 0 5 0 5 0 5 0 5 to the row
        }
}

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]  # clustered based on hclust order

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), xlab = "Column", ylab = "Pattern 1", pch = 19)  # if we know the pattern 1, it's a easy plot
plot(rep(c(0, 1), 5), xlab = "Column", ylab = "Pattern 2", pch = 19)   # if we know the patter 2, it's a easy plot, too

svd2 <- svd(scale(dataMatrixOrdered))   # svd algorithm --- we need to refer the patterns from the data
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")  # try to piick up the first pattern. Not so obvious
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")  

svd1 <- svd(scale(dataMatrixOrdered))  # variance explanation
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained", pch = 19) # shift pattern can explain a large portion of variations

# Missing Values
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA  # sampling the first 100 obs (column first), replace 40 of them with NA. 
svd1 <- svd(scale(dataMatrix2))  # you will see error message. svd function doesn't work with NAs. Dr. Peng suggested o use impute package, which is available from bioconductor.org , to impute the missing values. 


# Face example. data available on Dr. Peng's Github
par(mfrow = c(1, 1))
load("./data/face.rda")
image(t(faceData)[,nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")  # singular value decomposion. can see first singular value explains 40% of variation, with second 23% and third 15%. 
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1] # %*% is a matrix operator (matrix multiply). and right hand of the equation returns the matrix represented by first component

approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[, 1:5]) # first 5 component
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10]) # first 10 component

par(mfrow = c(1, 4))
image(t(approx1)[,nrow(faceData):1], main = "(a)") # first component, no face
image(t(approx5)[,nrow(faceData):1], main = "(b)") # much better
image(t(approx10)[,nrow(faceData):1], main = "(c)") # better
image(t(faceData)[,nrow(faceData):1], main = "(d)") # oringinal

# Other dimension reduction tools include factor analysis, independent component analysis, and latent semantic analysis

# Working with Color in R Plots
# colorRamp example
pal <- colorRamp(c("red", "blue"))  # like mixing red and blue together.
pal(0) # return RBG component. sum to 255 
pal(1)
pal(0.5)
pal(seq(0, 1, len = 10)) # a swquence of value, from red to blue
# colorRampPalette
pal <- colorRampPalette(c("red", "yellow"))
pal(2)    # return two colors, red and yellow (integer numbers, but represented as 16 bit codes)---#FF0000 means FF(max) red, 00(no) green, and 00(no) blue. #FFFF00 means FF(max) red, FF(max) green, and 00(no) blue. Yellow is mixed by red and green.
pal(10)   # return ten colors

#RColorBrewer package
par(mfrow = c(1, 1))
library(RColorBrewer)  # sequential, diverging, and qualitative palettes
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)   # show the density in scatter plot also
plot(x, y, pch = 19) # show overlapping only
plot(x, y, col = rgb(0, 0, 0, 0.2), pch = 19)  # with transparency plot

# Clustering Case Study --- Samsung
load("./data/samsungData.rda")  # again, samsungdata canbe found on Dr. Peng's Github page. The data was used to identify human activities, such as stand up, lied down, walk, run, climb up and down.
names(samsungData)[1:12]
table(samsungData$activity)
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))  # convert activity variable to factor variable
sub1 <- subset(samsungData, subject == 1)   # subset
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)  # you can see the scatterplot of each activity

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
        ## modifiction of plclust for plotting hclust objects *in colour*!
        ## Copyright Eva KF Chan 2009
        ## Arguments:
        ##    hclust:    hclust object
        ##    lab:        a character vector of labels of the leaves of the tree
        ##    lab.col:    colour for the labels; NA=default device foreground colour
        ##    hang:     as in hclust & plclust
        ## Side effect:
        ##    A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height,2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x<0)]
        x <- x[which(x<0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot( hclust, labels=FALSE, hang=hang, ... )
        text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}

par(mfrow = c(1, 1))
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity)) # try to use euclidean distance to hierarchical clustering things (first three columns). no obvious ressults

par(mfrow = c(1, 2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[10]) # take a look at the 10th and 11th column
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])
distanceMatrix <- dist(sub1[, 10:12]) # try clustering the 10-12th columns
hclustering <- hclust(distanceMatrix)
par(mfrow = c(1, 1))
myplclust(hclustering, lab.col = unclass(sub1$activity))  # able to differenciate moving and non-moving activities
svd1 = svd(scale(sub1[, -c(562, 563)])) # delete two rows.
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19) # first and second singular vectors
plot(svd1$u[, 2], col = sub1$activity, pch = 19)
# the first singular vector ploting shows relatively clear signs to differentiate moving-activities from non-moving activities. However, the second singular vector ploting was not so clear. 
plot(svd1$v[, 2], pch = 19) # plot of second right singular vector
maxContrib <- which.max(svd1$v[, 2])  # find the max value within the vector
distanceMatrix <- dist(sub1[, c(10:12, maxContrib)])  # clustering based on the max value found
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))  #cluster dendrogram. separating moving activities are observable

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6) # Now try K-mean clustering. Note random start points may be sub-optimal. usually set nstart > 1 
table(kClust$cluster, sub1$activity)  # the result is not good enough...
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 1)
table(kClust$cluster, sub1$activity) # choose nstart = 1, the result little better
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity) # choose nstart = 100, the result little better

plot(kClust$center[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "") # plot the first cluster (Laying). High for first three features 
plot(kClust$center[4, 1:10], pch = 19, ylab = "Cluster Center", xlab = "") # plot the second cluster 














