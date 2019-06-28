# Raw data >- Processing script -> tidy data -> data analysis -> data communication
# All steps should be recorded
# 1. The Raw data
# 2. A tidy data set
# 3. A code book describing each variable and its values in the tidy data set. units
# 4. An explicit and exact recipe you used to go from 1 to 2 & 3. (Instruction list.)
library(beepr) # this package helps you a little bit in time management---use beep() function for a sound beep once your code get fully executed by R  
# Downloading files
getwd()
setwd("C:/Users/Xiao.Cheng/Desktop/R Code/Data Science Specialization_Coursera")

if(!file.exists("data")){
        dir.create("data")
} # if no folder under wd is called "data", create a new folder names "data"

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD" # go to baltimore cameras home page to find the downloadable link
download.file(fileUrl, destfile = "./data/cameras.csv") # if MAC, add methond = "curl". download the file and write it as cameras.csv
list.files("./data")
dateDownloaded <- date()
dateDownloaded

# Reading Local flat files
cameraData <- read.table("./data/cameras.csv") # error
cameraData <- read.table("./data/cameras.csv", sep = ",",header = TRUE) # need to specify separator
head(cameraData) # can also set quote, na.strings, nrows, skip

# Reading Excel files
library(xlsx)
cameraData <- read.xlsx("./data/cameras.xlsx",sheetIndex = 1, header = TRUE) # create this file manually. Download method not available on Baltimore data
head(cameraData)
colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./data/cameras.xlsx",sheetIndex=1,colIndex=colIndex,rowIndex=rowIndex)
cameraDataSubset
# also can try write.xlsx, read.xlsx2, XLConnect package

# Reading XML  (markup language)
  # Start tags(correspond to general labels), End tags, Empty tage
  # e.g. <Greeting> Hello, world </Greeting>
  # attributes are components of the lable 
  # e.g. <step number="3"> Connect A to B. <step>
library(XML) # install first
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl,useInternal=TRUE) # doesn't work dut to internet settings by BOCI
rootNode <- xmlRoot(doc)
xmlName(rootNode)    
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE) # doesn't work dut to internet settings by BOCI
  # skip this part. I doubt this is due to the internet setup 

# Reading Json (Javascript Object Notation)
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
  # Again, error. Plus, I don't really think we have any in-house generated data in this format.

# data.table package. Datatables is more efficient than dataframes
library(data.table)
DF = data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)

DT = data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)
tables()
DT[2,]
DT[DT$y=="a",]
DT[c(2,3)]
DT[,c(2,3)]

{
  x=1
  y=2
}
k={print(10);5} # use expressions to summarize datasets
DT[,list(mean(x),sum(z))] # list the mean of x column and sum of the z column
DT[,table(y)] # tabulating the y coloumn
# := in R, fast add, remove and update subsets of  columns, by reference. 

DT[,w:=z^2] # add new column to the data table

DT2 <- DT
DT[, y:=2] # Note for datatables, use copy function if you need an independent copy of the table. Otherwise when you make some changes through := method to DT, DT2 also changes

DT[,m:={tmp <- (x+z); log2(tmp+5)}] # multiple operations.
DT[,a:=x>0] # plyr like operations. 
DT[,b:=mean(x+w),by=a] # generate mean(x+w), group by variable a. 

set.seed(123);
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE)) # sampling a,b,c 10,000 times. TRUE means put the sample back to the space once it shows up.
DT[, .N, by=x] # count the # of times appeared group by x

DT <- data.table(x=rep(c("a","b","c"),each=100),y=rnorm(300))
setkey(DT, x) # set the key of x
DT['a'] # search a by the default key

DT1 <- data.table(x=c('a','a','b','dt1'), y=1:4)
DT2 <- data.table(x=c('a', 'b', 'dt2'), z=5:7)
setkey(DT1, x); setkey(DT2, x)
merge(DT1, DT2) # merge by keys set before

big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
file <- tempfile()
write.table(big_df, file=file, row.names = FALSE, col.names = TRUE, sep="\t", quote=FALSE)
system.time(fread(file))
system.time(read.table(file, header=TRUE, sep="\t"))  # fread is much faster

# read from mySQL database
# OK, connection error again
library(RMySQL)
ucscDb <- dbConnect(MySQL(),user="genome",
                    host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb,"show")
# also for reading from HDF5. Skip for now.

# Reading from The Web
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode # hard to read. Use XML package
library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes = T) # failed
xpathSApply(html, "//title", xmlValue)

# Subsetting and Sorting
set.seed(13435)
X <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)] = NA
X[,1] # first column
X[,"var1"]
X[1:2,"var2"] # first two rows and var2 column
X[(X$var1 <= 3 & X$var3 > 11),]
X[(X$var1 <= 3 | X$var3 > 15),]
X[which(X$var2 > 8),] # to omit NAs

sort(X$var1) # sorting
sort(X$var1,decreasing = TRUE)
sort(X$var2,na.last = TRUE)
X[order(X$var1),] # reorder rows is var1 in increasing order
X[order(X$var1,X$var3),]
library(plyr) # same thing with plyr package
arrange(X,var1)
arrange(X,desc(var1)) # descending
X$var4 <- rnorm(5) # adding column
X
Y <- cbind(X,rnorm(5)) # cbind function 

#Summarizing data
if(!file.exists(("./data"))){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./data/restaurants.csv")
restData <- read.csv("./data/restaurants.csv")
head(restData,n=3) # show first 3 rows. default 6 rows
tail(restData,n=3) # show bottom 3 rows

summary(restData) # factor varirables: count. Numerical: quantile.
str(restData)
quantile(restData$councilDistrict,na.rm=TRUE) # view percentiles.
quantile(restData$councilDistrict,probs=c(0.5,0.75,0.9))
table(restData$zipCode,useNA="ifany") # tabulate / count frequent
table(restData$councilDistrict,restData$zipCode) # two-way count

sum(is.na(restData$councilDistrict)) # sum of the NA count of the councilDistrict variable
any(is.na(restData$councilDistrict)) # if any values for councilDistrict variable is NA
all(restData$zipCode > 0) # if all of zipcode variable is greater than 0 or not. In this case one of the entry is actually negative.

colSums(is.na(restData)) # count missing values for each column
all(colSums(is.na(restData))==0) # if any columns have any missing values
table(restData$zipCode %in% c("21212")) #are there any values of zipCode equals 21212?
table(restData$zipCode %in% c("21212","21213")) # for multiple criterias
restData[restData$zipCode %in% c("21212","21213"),] # row subsetting

data("UCBAdmissions") # load this data
DF = as.data.frame(UCBAdmissions)
xt <- xtabs(Freq ~ Gender + Admit, data=DF) # count frequncy, by gender by admit (two way count)
xt
warpbreaks$replicate <- rep(1:9, len = 54)
xt = xtabs(breaks ~.,data=warpbreaks) # count breaks, break down by all other variables.
xt
ftable(xt) # summarize data in much smaller, more compact form
fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units="Mb") # you could check the size of the data object. You could also check it in the work environment window if you use RStudio.
rm(list=ls()) # remove all objects in working environment
beep()

# Creating New Variables
# Missingness indicators, Factorize quantitative variables, Applying transforms
# Go to data.baltimorecity.gov website to check the url for this restaurants dataset
if(!file.exists("./data")){dir.creat("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile = "./data/restaurants.csv")
restData <- read.csv("./data/restaurants.csv")

s1 <- seq(1,10,by=2); s1   # generate sequences. could be done by indicate the intervals
s2 <- seq(1,10,length=3); s2  # or by how many seq numbers do you want
x <- c(1,3,8,25,100); seq(along = x) # or just be the same lentgh of the data
restData$nearMe = restData$neighborhood %in% c("Roland Park", "Homeland")  # find restaurants in these two areas. 
table(restData$nearMe)
restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong,restData$zipCode<0) # two-way listing check if ifelse was correctly implemented
restData$zipGroups = cut(restData$zipCode,breaks=quantile(restData$zipCode)) # cut data into percentiles
table(restData$zipGroups)
table(restData$zipGroups,restData$zipCode) # see the number of obs falls into zipGroups and zipcode

library(Hmisc)
restData$zipGroups = cut2(restData$zipCode,g=4) # break down in quantile
table(restData$zipGroups)

restData$zcf <- factor(restData$zipCode) # turn zipCode into factor variables
restData$zcf[1:10]
class(restData$zcf)

yesno <- sample(c("yes","no"),size=10,replace=TRUE)
yesnofac = factor(yesno,levels=c("yes","no")) # turn into factor variable. yes=low value. no=high value
relevel(yesnofac,ref="yes")
as.numeric(yesnofac)

library(plyr)
restData2 = mutate(restData, zipGroups=cut2(zipCode,g=4)) # use mutate function to add zipGroups in the new dataframe restData2
table(restData2$zipGroups)

# Reshaping data. 
# The goal is tidy data: each var forms a column, each obs forms a row, each table/file stores data about one kind of obs

library(reshape2)
head(mtcars)
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp")) # melt means we transform the dataset by specifying which variables are id variables, and which variables are measure variables.
head(carMelt,n=3)
tail(carMelt,n=3)
cylData <- dcast(carMelt, cyl ~ variable)  # recast data into a total different shape. this is a summarize, read as: for 4 cyl type, we have 11 measurements of mpg, and 11 measurements of hp. For 6 cyl type, we have ....
cylData <- dcast(carMelt,cyl~variable,mean)
cylData  # summarize by mean value of mpg and hp under each cyl type

head(InsectSprays)
tapply(InsectSprays$count,InsectSprays$spray,sum)  # apply count, along the spray, the function sum. Sum the value of count in each spray class.

spIns = split(InsectSprays$count,InsectSprays$spray) # split InsectSprays into a list. Following steps do the samething as tapply sum above 
spIns
sprCount = lapply(spIns,sum)
sprCount
unlist(sprCount) # turn list into a vector/array
sapply(spIns,sum) # do the samething

ddply(InsectSprays,.(spray),summarize,sum=sum(count)) #ddply function. We want to take the variable spray (witin ddply), summarize it, by sumup the count variable

spraySums <- ddply(InsectSprays,.(spray),summarize,sum=ave(count,FUN = sum))
dim(spraySums)
head(spraySums) # spraySums will have same length with the oringinal dataset InsectSprays. this is due to the ave founction.

# dplyr package is designed to working with/ managing dataframes
library(dplyr)
# select, filter, arrange, rename, mutate, summarise / summarise
# first argument is always data frame
# e.g. head(select(chicago, city:dptp))  # select all columnes between city and dptp. use - to drop all columnes you don't need
# chic.f <- filter(chicago, pm25tmean2 >30)  # example of subsetthing the dataset based on certain criterias.
# chicago <- arrange(chicago, date)  # sort. use desc(date) for descending purpose
# chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)  # rename the selected columnes.
# chicago <- mutate(chicago, tempcat = factor(1 * (tmpd > 80),labels = c("cold","hot")))
# hotcold <- group_by(chicago, tempcat)   # new data structure
# %>% is the pipeline operator: output of each pipeline operator into the following one. Sequence operation is readable and powerful.

# Merging data
# x, y, by, by.x, by.y, all
# default is merge all common column names
# also can use arrange(join(df1,df2),id) the join function. Faster, but less full featured. More often used if you have multiple data frames (use join_all)

# Editing text variables
cameraData <- read.csv("./data/cameras.csv") # Again, Baltimore traffic camera data
names(cameraData)
tolower(names(cameraData)) # may want to make all name variables lower cases. Also have toupper command
splitNames = strsplit(names(cameraData),"\\.") # create a list of names variables. \\. is the split rule
splitNames[[5]]
splitNames[[6]]  # can see Location.1 is splitted to Location and 1

mylist <- list(letters = c("A","b","c"),numbers = 1:3, matrix(1:25, ncol = 5)) # Recall the list operators. create a new list of 3 elements, letters, numbers, and a matrix
head(mylist)
mylist[1]
mylist$letters
mylist[[1]]

splitNames[[6]][1] # Location
firstElement <- function(x){x[1]}
sapply(splitNames,firstElement)   # keep the first word before "."

testName <- "this_is_a_test"
sub("_","",testName) # replace the first _ with nothing
gsub("_","",testName) # replace all _ 

grep("Alameda",cameraData$intersection) # locate the intersection which contain the letters Alameda. String Search
table(grepl("Alameda",cameraData$intersection)) # return true or false
cameraData2 <- cameraData[!grepl("Alameda",cameraData$intersection),] # subset with rows which don't contain the word Alameda
grep("Alameda",cameraData$intersection,value=TRUE) # return the data entries
grep("JeffStreet",cameraData$intersection) # no match result
length(grep("JeffStreet",cameraData$intersection))

library(stringr) # some useful functions related to string manipulating
nchar("Jeffrey Leek") # count number of characters (space included)
substr("Jeffrey Leek",1,7) # substring with the first 7 characters
paste("Jeffrey","Leek") # paste together
paste0("Jeffrey","Leek") # If you don't need the space, use paste0
str_trim("Jeff      ") # trim off any extra spaces

# Regular Expressions: general search terms. Text processing purpose
# ^i think = strings start with i think
# morning $ = strings ends with morning
# [Bb][Uu][Ss][Hh] = word "bush" with any letters capitalized or not
# ^[Ii] am = strings started with I am or i am
# [a-z] or [a-zA-Z] = all letters
# ^[0-9][a-zA-Z] will match any strings start with a number, follow with letters
# [^?.]$ = here ^ means not either of these. so ^?. means not ? and not . . it mathces all lines with no ? or . at the end position
# 9.11 = . can be any one character. so 9.11 matches 9-11, 9/11, etc.
# flood|fire = alternatives. matches all lines with either flood or fire. Another example is flood|earthquake|hurricane|coldfire
# ^[Gg]ood|[Bb]ad = a string start with [Ggood], or a string contains [Bb]ad
# ^([Gg]ood|[Bb]ad) = a string start with [Gg]ood or [Bb]ad
# [Gg]eorge([Ww]\.)? [Bb]ush = search for [Gg]eorge [Bb]ush, with W. or w. in the middle or not. ? means not sure. Make sure to use \ to represent any symbols since symbols are metacharacters
# * = any number, including none, of the item
# + = at least one of the item
# (.*) = strings include (), with anything or nothing in the middle.
# [0-9]+(.*)[0-9] = at least one number follow by anything follow by at last one number
# [Bb]ush( +[^ ]+ +){1,5} debate = [Bb]ush(...)debate, things in the middle follows the rule: at least one space, follow by at least one non-space char, follow by at least one space, and this pattern should occur at least one time and at most five times
# {} specifies the repetition of the expression. {m,n} means at least m times and no more than n times. {m} means exactly m matches. {m,} means at least m matches
#  +([a-zA-Z]+) +\1 + = at least one space follow by at least one letter, follow by at least one space, follow by the exact same match that we saw within the parenthesis, follow by at least one space. \1 represents the text matched
# ^s(.*)s = strings start with s, follow by anything, follow by another s. Note this expression has "greedy" property---it tries to match as long strings as possible. (greedy algorithm)
# ^s(.*?)s$ = strings start with s, follow by anything in the middle, follow by s at the end. not greedy 

# Working with Dates
d1 = date() # date function
d1  # gives you the date and time
class(d1)  # d1 is a character object

d2 = Sys.Date()
d2
class(d2) # d2 is a date object
format(d2,"%a %b %d") # formatting dates. %d = day as number (0-31), %a = abbreviated weekday, %A = unabbreviated weekday, %m = month(00-12), %b = abbreviated month, %B = unabbrevidated month, %y = 2 digit year, %Y = four digit year

x = c("1jan1960","2jan1960","31mar1960","30jul1960"); z = as.Date(x, "%d%b%Y")
z
z[1] - z[2]
as.numeric(z[1]-z[2])

weekdays(d2) # tell you which day 
months(d2) # tell you which month
julian(d2) # julian date---# of days since origin

library(lubridate) # lubridate is a handy package with dates object
ymd("20140108") # convert a number to a date. Auto detect the format
mdy("08/04/2013")
dmy("03-04-2013")
ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03",tz="Pacific/Auckland") # set timezone
?Sys.timezone # check the system timezone code
x = dmy(c("1jan2013","2jan2013","31mar2013","30jul2013"))
wday(x) # weekday
wday(x,label=TRUE)

# Data Resources
# 1. Open Government Sites. e.g. United Nations
# 2. Gapminder. Development / human health 
# 3. Survery data from the US. asdfree
# 4. Infochimps Marketplace
# 5. Kaggle
# 6. Collections by data scientists. 
# 7. More specialized collections. 
# 8. API's with R interfaces. e.g. twitteR


