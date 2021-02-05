
student_mat <-read.table("Data/Datasets/student-mat.csv",sep=",",header=TRUE)
student_por <-read.table("Data/Datasets/student-por.csv",sep=",",header=TRUE)


# Merge both dataframes
grades <- rbind(student_mat, student_por) 
all <- rbind(student_mat, student_por) 

## Data Transformation
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


#Target in orignial dataset and numeric dataset
all$transition <- cut(all$G3, 
                          breaks=c(-Inf, 10,  Inf), 
                          labels=c("fail","pass"))
grades$transition <- cut(grades$G3, 
                      breaks=c(-Inf, 10,  Inf), 
                      labels=c("fail","pass"))
grades$transition=factor(grades$transition, levels = c("fail","pass"),labels = c(0,1))
# Feature Scaling
#grades['age'] = scale(grades['age'])
#grades['failures'] = scale(grades['failures'])
#grades['absences'] = scale(grades['absences'])


write.table(grades, "Data/Datasets/grades.csv", row.names = FALSE, col.names = TRUE, sep=",")
write.table(all, "Data/Datasets/all.csv", row.names = FALSE, col.names = TRUE, sep=",")