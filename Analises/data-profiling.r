library(haven)
library(readr)
library(ggplot2)

grades_df <-read.table("Data/Datasets/all.csv",sep=",",header=TRUE)

## Quick summary
# summary(student_por)
# summary(student_mat)
summary(grades_df)

## Sum of missing values
# sum(is.na(student_por))
# sum(is.na(student_mat))
# sum(is.null(student_por))
# sum(is.null(student_mat))
sum(is.na(grades_df))
sum(is.null(grades_df))


# Grades Frequency
ggplot(grades_df, aes(x =G3 , y = )) + 
  geom_bar(color='lightyellow2',
           fill='coral2') +
  labs(x = "Grades",
       y = "Frequency",
       color='cyl',
       title = "Grades Distribution")


## Relation between study hours, absences and sex
#data <- grades_df[order(as.numeric(grades_df$GA))]

ggplot(data = grades_df,
       mapping = aes(x = absences, 
                     y = G3,
                     color = sex)) +
  geom_point(alpha = .25,
             size = 2) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              size = 1.5) + 
  labs(x = "Absences",
       y = "Final Grades",
       col = "Sex",
       title = "Tendency of impact of absences on final grades by sex")



#
data <- table(grades_df$romantic, grades_df$freetime)
data_df <- as.data.frame.matrix(data)

print(data_df)
x_freetime <- as.vector(sort(unique(grades_df$freetime)))
y_romantic <- vector()
y_non_romantic <- vector()

for (x in x_freetime) {
  romantic_data <- subset(grades_df, freetime == x & romantic == "yes",
                          select=c(failures, G1, G2, G3, GA))
  
  non_romantic_data <- subset(grades_df, freetime == x & romantic == "no",
                              select=c(failures, G1, G2, G3, GA))
  
  y_romantic <- c(y_romantic, mean(romantic_data$G3))
  y_non_romantic <- c(y_non_romantic, mean(non_romantic_data$G3))
}

ggplot(grades_df, mapping = aes(x=freetime, fill=romantic)) +
        geom_bar(position="dodge") 
        
# 
# barplot(data_df, 
#         main="Impact of freetime and relation status on final grades",
#         xlab="Free Time",
#         ylab="Frequency",
#         col=c("coral3", "cornflowerblue"),
#         legend = rownames(data), beside = TRUE) 
par(new=TRUE)
plot(x_freetime, 
       y_romantic,
       type="b", axes = FALSE, xlab = "", ylab = "")
par(new=TRUE)
plot(x_freetime,
     y_non_romantic,
     type="b", axes = FALSE, xlab = "", ylab = "Average Grades")

