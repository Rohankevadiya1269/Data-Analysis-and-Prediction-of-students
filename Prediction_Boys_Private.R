# STEP -- Data Cleaning 

student_performance<- read.csv("S:\\Study\\OneDrive - DePaul University\\1st Quarter\\DSC 423 Data Analysis and regression\\Project stuff\\Data_set\\Student_performance.csv",stringsAsFactors = TRUE)
head(student_performance)
str(student_performance)

# summary of the data
summary(student_performance)

#checking for the null values
colSums(is.na(student_performance))

#removing null values with the help of na.omit method
student_performance <- na.omit(student_performance)
colSums(is.na(student_performance))

# Removing duplicate rows
student_performance <- student_performance[!duplicated(student_performance), ]

# Data Filtering
library(dplyr)

Data_std <- student_performance %>% filter(Gender == "Male", School_Type == "Private")
print(Data_std)
nrow(Data_std)

Data_std <- Data_std %>% select(-Gender,-School_Type)
model <- lm(Exam_Score ~ ., data = Data_std)
summary(model)

# Creating Dummy Variables
library(caret)
dummy_model <- dummyVars(~ ., data = Data_std)

# Apply the model to create dummy variables
dummy_data <- predict(dummy_model, newdata = Data_std)

# Convert to a data frame
dummy_data <- data.frame(dummy_data)

# View the result
print(dummy_data)
str(dummy_data)

model <- lm(Exam_Score ~ . , data = dummy_data)
summary(model)

#feature Selection

backward_model <- step(model, direction = "backward")
summary(backward_model)

# Data visualization
#plot(backward_model)

# multicolinearity

library(dplyr)
library(car)
vif (backward_model)

# prediction

library(ggplot2)
library(caret)

set.seed(123)

# Splitting the data in two parts: 80& training and 20% testing
trainIndex <- createDataPartition(dummy_data$Exam_Score,p=0.8,list = FALSE,times = 1)

train_data <-  dummy_data[trainIndex, ]
test_data <- dummy_data[-trainIndex, ]

#train model

training_model <- lm(Exam_Score ~ ., data = train_data)

#Testing

predictions<- predict(backward_model,newdata = test_data)


summary(training_model)

rmse <- sqrt(mean((predictions - test_data$Exam_Score)^2))
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")
rsq <- cor(predictions, test_data$Exam_Score)^2
cat("R-squared: ", rsq, "\n")

# Optionally, visualize the predictions
ggplot(data = test_data, aes(x = Exam_Score, y = predictions)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  ggtitle("Actual vs Predicted Exam Scores") +
  xlab("Actual Exam Score") +
  ylab("Predicted Exam Score")

