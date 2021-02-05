library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram)
library(rpart)
library(rminer)

#original data frames
student_mat <-read.table("Data/Datasets/student-mat.csv",sep=",",header=TRUE)
student_por <-read.table("Data/Datasets/student-por.csv",sep=",",header=TRUE)

#new data frames
novoStudent_mat <- cbind(student_mat["Mjob"],student_mat["Fjob"], student_mat["famsup"], student_mat["schoolsup"], student_mat["paid"],student_mat["G1"],student_mat["G2"],student_mat["G3"],student_mat["higher"])
novoStudent_por <- cbind(student_por["Mjob"],student_por["Fjob"], student_por["famsup"], student_por["schoolsup"], student_por["paid"],student_por["G1"],student_por["G2"],student_por["G3"],student_por["higher"])

#dataframe merged
data_df <- rbind(novoStudent_mat, novoStudent_por)
#pass/fail
pass<-cut(data_df$G3,c(-1,9,20),c("fail","pass"))
data_df$transition <- pass


#categorical to nominal MJob, Fjob
#data_df$NewMjob <- as.numeric(as.factor(data_df$Mjob))
#data_df$NewFjob <- as.numeric(as.factor(data_df$Fjob))

#categorical to numerical Famsup, school sup
#data_df$FamSup <- as.numeric(as.factor(data_df$famsup))
#data_df$SchoolSup <- as.numeric(as.factor(data_df$schoolsup))
#data_df$Paid <- as.numeric(as.factor(data_df$paid))
#Numeric DataFrame
newData_df <- cbind(data_df["transition"], data_df["G3"],data_df["Fjob"],data_df["Mjob"],data_df["famsup"],data_df["schoolsup"],data_df["paid"],data_df["higher"])

#training & testing dataset
training <- newData_df[, 2:6]
testing <- newData_df [, 1]

#trainingOutcomes
trainingOutcomes <- newData_df[1:962, 2:6]
testingOutcomes <- newData_df[963:1044, 1]

print(testingOutcomes)

#decision tree
fit <- rpart(formula=testingOutcomes,data=trainingOutcomes ~ .,method="class")
#summary(fit)

inputs=newData_df[,2:6] 
# select from 2 ("sex") to 29 ("health")
#inputs <- list("NewFjob","NewMjob","FamSup","SchoolSup")
#inputs2 <- rowSums(newData_df[inputs])
#print(inputs2)
# select outputs: binary task "pass"
bout=which(names(newData_df)=="pass")
#cat("output class:",class(math[,bout]),"\n")
# two white-box examples:
#B1=fit(transition???.,newData_df[,c(inputs,bout)],model="naive")
stor <- fit (testingOutcomes~.,trainingOutcomes, model="naive")
summary(stor)
print(stor@object)
