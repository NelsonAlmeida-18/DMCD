library(h2o)
library(caTools)


invisible(h2o.init())

grades <-read.table("Data/Datasets/student-por.csv",sep=",",header=TRUE)
#######################################
drops <- c("school","G1","G2")
grades=grades[ , !(names(grades) %in% drops)]
grades$romantic=factor(grades$romantic, levels = c("no","yes"),labels = c(0,1))
grades$internet=factor(grades$internet, levels = c("no","yes"),labels = c(0,1))
grades$higher=factor(grades$higher, levels = c("no","yes"),labels = c(0,1))
grades$nursery=factor(grades$nursery, levels = c("no","yes"),labels = c(0,1))
grades$activities=factor(grades$activities, levels = c("no","yes"),labels = c(0,1))
grades$paid=factor(grades$paid, levels = c("no","yes"),labels = c(0,1))
grades$famsup=factor(grades$famsup, levels = c("no","yes"),labels = c(0,1))
grades$schoolsup=factor(grades$schoolsup, levels = c("no","yes"),labels = c(0,1))
grades$guardian=factor(grades$guardian, levels = c("mother","father", "other"),labels = c(1,2,3))
grades$reason=factor(grades$reason, levels = c("home","reputation", "course","other"),labels = c(1,2,3,4))
grades$Fjob=factor(grades$Fjob, levels = c("teacher","health", "services","at_home","other"),labels = c(1,2,3,4,5))
grades$Mjob=factor(grades$Mjob, levels = c("teacher","health", "services","at_home","other"),labels = c(1,2,3,4,5))
grades$Pstatus=factor(grades$Pstatus, levels = c("T","A"),labels = c(1,2))
grades$famsize=factor(grades$famsize, levels = c("LE3","GT3"),labels = c(1,2))
grades$address=factor(grades$address, levels = c("U","R"),labels = c(1,2))
grades$sex=factor(grades$sex, levels = c("M","F"),labels = c(1,2))
###########################################
grades$transition <- cut(grades$G3,c(-1,9,20),c("fail","pass"))

#data_splitting
set.seed(123)
split=sample.split(grades$G3, SplitRatio = 0.75)
training_set=subset(grades,split==TRUE)
test_set=subset(grades,split==FALSE)

# convert data as h2o type
test_h = as.h2o(test_set)
train_h = as.h2o(training_set)

predictors <- c("Dalc","Walc","goout","internet")
response <- "transition"

trans_gbm <- h2o.gbm(x = predictors,
                     y = response,
                     distribution = "bernoulli",
                     nfolds=10,
                     seed= 123,
                     keep_cross_validation_predictions = TRUE,
                     training_frame=train_h)


perf <- h2o.performance(trans_gbm)
grades <- as.h2o(grades)
pred <- h2o.predict(trans_gbm, newdata=test_h)
print(perf)
new_df <- pred

#h2o.std_coef_plot(trans_gbm)
plot(trans_gbm)


perf <- h2o.performance(trans_gbm)
grades <- as.h2o(grades)
pred <- h2o.predict(trans_gbm, newdata=test_h)

print(perf)
plot(perf,colorize=TRUE)
