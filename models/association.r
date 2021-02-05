#install.packages('arules')
library(arules)
dataset = read.csv('Data/Datasets/all.csv', sep=";") 
drops <- c("school","G1","G2")
dataset=dataset[ , !(names(dataset) %in% drops)]
dataset$G3 <- cut(dataset$G3, 
                  breaks=c(-Inf, 10, 15, Inf), 
                  labels=c("Reprovou","Satisfatorio","Excelente"))
dataset$age <- cut(dataset$age, 
                  breaks=c(-Inf, 17, 20, Inf), 
                  labels=c("-17","18-20","21-"))
dataset$absences <- cut(dataset$absences, 
                   breaks=c(-Inf, 20, 40, Inf), 
                   labels=c("-20","21-40","41-"))
dataset$Fedu=factor(dataset$Fedu, levels = c("0","1", "2","3","4"),labels = c("none","primary","basic","high","university"))
dataset$Medu=factor(dataset$Medu, levels = c("0","1", "2","3","4"),labels = c("none","primary","basic","high","university"))
dataset$traveltime=factor(dataset$traveltime, levels = c("1", "2","3","4"),labels = c("Less_15m","15-30","10-60","more_1hour"))
dataset$studytime=factor(dataset$studytime, levels = c("1", "2","3","4"),labels = c("Less_2h","2-5","5-10","more_10hour"))
dataset$failures=factor(dataset$failures, levels = c("0","1", "2","3"),labels = c("none","1","2","3"))
dataset$famrel=factor(dataset$famrel, levels = c("1", "2","3","4","5"),labels = c("Muito Mau","Mau","Razoavel","Bom","Excelente"))
dataset$health=factor(dataset$health, levels = c("1", "2","3","4","5"),labels = c("Muito Mau","Mau","Razoavel","Bom","Excelente"))
dataset$freetime=factor(dataset$freetime, levels = c("1", "2","3","4","5"),labels = c("Muito Pouco","Pouco","Suficiente","Mais Suficiente","Bastante"))
dataset$goout=factor(dataset$goout, levels = c("1", "2","3","4","5"),labels = c("Muito Pouco","Pouco","Suficiente","Mais Suficiente","Bastante"))
dataset$Dalc=factor(dataset$Dalc, levels = c("1", "2","3","4","5"),labels = c("Muito Pouco","Pouco","Suficiente","Mais Suficiente","Bastante"))
dataset$Walc=factor(dataset$Walc, levels = c("1", "2","3","4","5"),labels = c("Muito Pouco","Pouco","Suficiente","Mais Suficiente","Bastante"))

trans3 <- as(dataset, "transactions")
#summary(trans3)

itemFrequencyPlot(trans3, topN = 10)

# Training Apriori on the dataset
rules = apriori(data = trans3, parameter = list(support = 0.6, confidence = 0.8))

# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])
