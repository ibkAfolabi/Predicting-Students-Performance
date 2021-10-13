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
