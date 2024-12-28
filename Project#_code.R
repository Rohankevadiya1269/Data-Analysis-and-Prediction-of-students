std_perf<- read.csv("S:\\Study\\OneDrive - DePaul University\\1st Quarter\\DSC 423 Data Analysis and regression\\Project stuff\\Data_set\\Student_performance.csv")
head(std_perf)

#We are checking the summary
summary((std_perf))

#We are checking if there are missing values or not
colSums(is.na(std_perf))