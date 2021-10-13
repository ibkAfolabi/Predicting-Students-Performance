
#loading necessary data
recruit.df <- read.csv("cis.csv")



## barchart of sex vs. meancgpa
# compute mean cpga per sex = (male, female)
data.for.plot <- aggregate(recruit.df$cgpa1, by = list(recruit.df$sex), FUN = mean)
names(data.for.plot) <- c("sex", "Meancgpa1")

barplot(data.for.plot$Meancgpa1, names.arg = data.for.plot$sex,
        xlab = "sex", ylab = "Avg. cgpa1")

# alternative plot with ggplot
ggplot(data.for.plot) + geom_bar(aes(x = sex, y = Meancgpa1), stat = "identity")


## barchart of yoe vs. meancgpa
# compute mean cpga per yoe
data.for.plot <- aggregate(recruit.df$cgpa1, by = list(recruit.df$yoe), FUN = mean)
names(data.for.plot) <- c("yoe", "Meancgpa1")

barplot(data.for.plot$Meancgpa1, names.arg = data.for.plot$yoe,
        xlab = "yoe", ylab = "Avg. cgpa1")

## barchart of YOG vs. meancgpa
# compute mean cpga per YOG
data.for.plot <- aggregate(recruit.df$cgpa1, by = list(recruit.df$YOG), FUN = mean)
names(data.for.plot) <- c("YOG", "Meancgpa1")

barplot(data.for.plot$Meancgpa1, names.arg = data.for.plot$YOG,
        xlab = "YOG", ylab = "Avg. cgpa1")


## histogram of ageentry
hist(recruit.df$ageentry, xlab = "ageentry")
# alternative plot with ggplot
library(ggplot2)
ggplot(recruit.df) + geom_histogram(aes(x = ageentry), binwidth = 5) 

## boxplot of cgpa1 for different values of ageentry
boxplot(recruit.df$cgpa1 ~ recruit.df$ageentry, xlab = "ageentry", ylab = "cgpa1")




## histogram of ageexit
hist(recruit.df$ageexit, xlab = "ageexit")
# alternative plot with ggplot
library(ggplot2)
ggplot(recruit.df) + geom_histogram(aes(x = ageexit), binwidth = 5) 


## boxplot of cgpa1 for different values of ageentry
boxplot(recruit.df$cgpa1 ~ recruit.df$ageexit, xlab = "ageexit", ylab = "cgpa1")

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(recruit.df$L100 ~ recruit.df$KL, xlab = "KL", ylab = "L100")
boxplot(recruit.df$L200 ~ recruit.df$KL, xlab = "KL", ylab = "L200")
boxplot(recruit.df$L300 ~ recruit.df$KL, xlab = "KL", ylab = "L300")
boxplot(recruit.df$L400 ~ recruit.df$KL, xlab = "KL", ylab = "L400")



## histogram of CUSATSCORE
hist(recruit.df$CUSATSCORE, xlab = "CUSATSCORE")
# alternative plot with ggplot
library(ggplot2)
ggplot(recruit.df) + geom_histogram(aes(x = CUSATSCORE), binwidth = 5) 

## boxplot of cgpa1 for different values of CUSATSCORE
boxplot(recruit.df$CUSATSCORE ~ recruit.df$KL, xlab = "KL", ylab = "CUSATSCORE")

## histogram of JAMBSCORE
hist(recruit.df$JAMBSCORE, xlab = "JAMBSCORE")
# alternative plot with ggplot
library(ggplot2)
ggplot(recruit.df) + geom_histogram(aes(x = JAMBSCORE), binwidth = 5) 

## boxplot of cgpa1 for different values of CUSATSCORE
boxplot(recruit.df$JAMBSCORE ~ recruit.df$KL, xlab = "KL", ylab = "JAMBSCOREE")



## simple plot 
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal,
# and scatter plots in the remaining panels.
plot(recruit.df[, c(6,8,9,10,11,13,14,15)])
# alternative, nicer plot (displayed)
library(GGally)
ggpairs(recruit.df[, c(6,8,9,10,11,13,14,15)])


#CODE FOR SELECTING THE BEST SUBSET OF VARIABLES TO USE FOR THE PREDICTION
# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
#CODE FOR SELECTING THE BEST SUBSET OF VARIABLES TO USE FOR THE PREDICTION
# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.

#loading necessary data
recruit.df <- read.csv("cisExhustive4.csv")

library(leaps)

search <- regsubsets(cgpa1 ~ ., data = recruit.df, nbest = 1, nvmax = dim(recruit.df)[2],
                     method = "exhaustive")
sum <- summary(search)
# show models
sum$which
# show metrics
sum$rsq
sum$adjr2
sum$Cp


#LINEAR REGRESSION MODEL TO PREDICT THE PERFORMANCE BASED ON RECRUITMENT DATA RESULT
recruit.df <- read.csv("cisExhustive4.csv")
# use first 863 rows of data
recruit.df <- recruit.df[1:863, ]
# select variables for regression
selected.var <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:863), 647)
train.df <- recruit.df[train.index, selected.var]
valid.df <- recruit.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
recruit.lm <- lm(cgpa1 ~ ., data = train.df)
# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(recruit.lm)


library(forecast)
# use predict() to make predictions on a new set.
recruit.lm.pred <- predict(recruit.lm, valid.df)

some.residuals <- valid.df$cgpa1[1:43] - recruit.lm.pred[1:43]
data.frame("Predicted" = recruit.lm.pred[1:43], "Actual" = valid.df$cgpa1[1:43],
           "Residual" = some.residuals)

# use accuracy() to compute common accuracy measures.
accuracy(recruit.lm.pred, valid.df$cpga1)


library(forecast)
recruit.lm.pred <- predict(recruit.lm, valid.df)
all.residuals <- valid.df$cgpa1 - recruit.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")





#LINEAR REGRESSION MODEL TO PREDICT THE PERFORMANCE BASED ON RECRUITMENT DATA RESULT(no age of entry and amb score)
recruit.df <- read.csv("cisExhustive4.csv")
# use first 863 rows of data
recruit.df <- recruit.df[1:863, ]
# select variables for regression
selected.var <- c(1, 2,  4, 5, 6, 7, 8)
# partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:863), 647)
train.df <- recruit.df[train.index, selected.var]
valid.df <- recruit.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
recruit.lm <- lm(cgpa1 ~ ., data = train.df)
# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(recruit.lm)


library(forecast)
# use predict() to make predictions on a new set.
recruit.lm.pred <- predict(recruit.lm, valid.df)

some.residuals <- valid.df$cgpa1[1:43] - recruit.lm.pred[1:43]
data.frame("Predicted" = recruit.lm.pred[1:43], "Actual" = valid.df$cgpa1[1:43],
           "Residual" = some.residuals)

# use accuracy() to compute common accuracy measures.
accuracy(recruit.lm.pred, valid.df$cpga1)


library(forecast)
recruit.lm.pred <- predict(recruit.lm, valid.df)
all.residuals <- valid.df$cgpa1 - recruit.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

