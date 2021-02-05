library(h2o)
library(caTools)

invisible(h2o.init())

dataset <- read.table('Data/Datasets/grades.csv', sep=",", header= TRUE)

#Cenário 2
#dataset=dataset[,c("goout","Dalc","Walc","transition")]
#Cenário 3
#dataset=dataset[,c("famsup","schoolsup", "paid", "Pstatus", "famrel" ,"transition")]

#data_splitting
set.seed(123)
split=sample.split(dataset$transition, SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#training/testing
train_= as.h2o(training_set)
test = as.h2o(test_set)
print(train)
y <- "transition"
x <- setdiff(names(training_set), y)

training_set[, y] <- as.factor(training_set[, y])
test_set[, y] <- as.factor(test_set[, y])

aml <- h2o.automl(x = x, y = y,
                  training_frame = train_,
                  max_models = 20,
                  seed = 1,max_runtime_secs = 20)

lb <- aml@leaderboard
aml@leader
print(lb)