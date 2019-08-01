# Reproducible research is important. 
# How to communicate data analysis, using code by writing documents that are very dynamic, and by sharing data so that other people can reproduce the work that you're doing

library(beepr)
getwd()
setwd("C:/Users/Xiao.Cheng/Desktop/R Code/Data Science Specialization_Coursera")

# Why Do we need reproducible researc? 
# technologies increasing data collection
# merge new databases
# computing power increased, so able to more sophisticated analyses
# computational version of everything

# try knitr package in R instead of latex

# Structure of a Data Analysis
# 1. Define the question: Narrow down your question. 
# e.g. detect emails that are SPAM --> quantitative characteristics of the emails to classify them as SPAM/HAM
# 2. Define the ideal data set: Descriptive, Exploratory, Inferential, Predictive, Causal, Mechanistic. 
# 3. Determine what data you can access: free, buy, terms of use, generate it yourself
# 4. Obtain the data. e.g. UCI machine learning repository (for SPAM email classification)
# 5. Clean the data. Quality is important.
# 6. Exploratory data analysis
# 7. Statistical prediction/modeling
# 8. Interpret results
# 9. Challenge results
#10. Synthesize/write up results
#11. Create reproducible code
par(mfrow = c(1, 1))
library(kernlab)  # use spam data in this package as an example
data(spam) # load the data
str(spam[, 1:5])  # check the variables 
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5) # generate random indicator to divide dataset into test set and training set
table(trainIndicator)
trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator ==0, ]
names(trainSpam)
head(trainSpam)  # check the data structures. layout the frequence of the words appear in each email
table(trainSpam$type) # quick count of how many nonspam and spam emails in train set.
plot(trainSpam$capitalAve ~ trainSpam$type) # plot the capitalAve, by type. However, this ploting is not clear because of the scaling. Now try to do log transformation
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)  # spam have much higher weight of capital letters. 
plot(log10(trainSpam[, 1:4] + 1)) # scatter plot of the first 4 variables.
hCluster = hclust(dist(t(trainSpam[, 1:57]))) # clustering. for now not so useful since it separate the capitalTotal and capitalLog variables. may need to redo to see in a better scaling
plot(hCluster)
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))  # log transform, and don't use last two variables
plot(hClusterUpdated) # looks much better

trainSpam$numType = as.numeric(trainSpam$type) - 1  # turn the type variable into numeric variable
costFunction = function(x, y) sum(x != (y > 0.5))   # simple logistic regression model
cvError = rep(NA, 55) # initiate the cross validated error vector
library(boot)  # bootstrap package
for (i in 1:55){  # will loop over all variables. For each variable, run a univariate logistic regression model. record the cross-validated error in the cvError vector
        lmFormula = reformulate(names(trainSpam)[i], response = "numType")
        glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
        cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2] # I think it's fine if you don't understand what's going on here. will learn these in regression section.
}

names(trainSpam)[which.min(cvError)] # charDollar is the one with the minimum cross-validated error
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam) # use the best model from the training set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5] = "spam" # set threshold as 0.5
table(predictedSpam, testSpam$type)
(61 + 458)/(1346 + 458 + 61 + 449) # error rate is about 22%

# Organizing Your Analysis
# markdown and readme files

# Markdown = simplified markup language
        # text to HTML conversion tool for web writers. 

#  Italics:  *This text will appear italicized!*
#  Bold: **This text will appear bold!**
#  Hedings:  # This is a main heading
#            ## This is a secondary heading
#            ### This is a tertiary heading
#  Unordered Lists:  - first item in list
#                    - second item in list
#                    - third item in list
#  Ordered Lists:    1. first item in list
#                    2. second item in list
#                    3. third item in list      (numbers haven't to be in order)
#  Links: [Johns Hopkin Bloomberg School of Public Health](http://www.jhsph.edu/)
#         [Download R](http://www.r-project.org/)
#         [RStudio](http://www.rstudio.com/)
# Advanced Linking: I spend so much time reading [R bloggers][1] and [Simply Statistics][2]!
#                   [1]: http://www.r-bloggers.com/   "R bloggers"
#                   [2]: http://simplystatistics.org/   "Simply Statistics"
# Newlines: New lines require a double space after the end of a line.

# R Markdown: if you want to embed R code in text file. 
        # need knitr package to evaluate the R markdown file
        # use ```{r} and ``` to open and close r chunck in markdown file
        # check the file markdown_demo.Rmd as an example

# Literate Statistical Programming with knitr
        # could use Sweave system also to document in Latex, and programming in R
        # knitr supports RMarkdown, LaTex, and HTMLs as documentation languages
        # Can export to PDF, HTML
        # use knitr-ex1.Rmd as an example

# Communicating Results
        # Hierarchy of Information: Research Paper: Title / Author list, Abstract, Body / Results, Supplementary Materials / the gory details, Code / Data / really gory details.
        # Hierarchy of Information: Email Presentation: Subject line / Sender info (summarize findings?), Email body (brief description of the problem / context, recall what was proposed and executed; summarize findings / results; 1-2 paragraphs, suggest some options of actions may need to take, if questions need to be addressed try to make them yes or no), Attachments (R Markdown file, knitr report, stay concise), Links to Supplementary Materials (Code / Software / Data, GitHub repository / Project web site)

# RPubs might be interesting with knitr (publish your r markdown file online)

# Reproducible Research Checklist
        # Do: start with good science
        # Don't: Do things by hand. e.g. removing outliers, QA / QC (Quality Assurance / Quality Control), Validating, "We're just going to do this once..."
        # Don't: Point And Click. e.g. GUI, things are highly interactive
        # Do: Teach a Computer
        # Do: Keep Track of Your Software Environment. e.g. sessionInfo()
        # Don't: Save Output
        # Do: Set Your seed (when simulate)
        # Do: Think About the Entire Pipeline: Raw -> processed -> analysis -> report

# Evidence-based Data Analysis
        # Replication and Reproducibility
        # Background and Underlying Trends
        # The result?

