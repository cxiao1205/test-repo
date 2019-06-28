
# Set this folder as my default directory in the future.
setwd("C:/Users/Xiao.Cheng/Desktop/R Code")

# Reading Lines of a Text File
con <- url("http://www.jhsph.edu","r")
x <- readLines(con)
head(x)

# Packages
a <- available.packages()
install.packages("slidify")
install.packages(c("slidify","ggplot2","devtools"))

# if need big data / other packages. Check their website.
source("http://bioconductor.org/biocLite.R")
biocLite()

library(ggplot2)

find.package("devtools")
library(devtools)



