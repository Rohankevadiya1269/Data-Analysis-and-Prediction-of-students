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

library(dplyr)

# Create a new data frame with conditions
Data_std2 <- student_performance %>%
  filter(Gender == "Male", School_Type == "Public")

print(Data_std2)

plot(Data_std2$Learning_Disabilities, Data_std2$Exam_Score)

model <- lm(Data_std2$Exam_Score ~ Data_std2$Extracurricular_Activities)
summary(model)

model <- lm(Data_std2$Exam_Score ~ Data_std2$Internet_Access)
summary(model)

model <- lm(Data_std2$Exam_Score ~ Data_std2$Peer_Influence)
summary(model)

model <- lm(Data_std2$Exam_Score ~ Data_std2$Learning_Disabilities)
summary(model)

model <- lm(Data_std2$Exam_Score ~ Data_std2$Distance_from_Home)
summary(model)

model <- lm(Exam_Score ~ ., data = subset(Data_std2, select = -c(School_Type, Gender)))
summary(model)

Data_std2 <- Data_std2 %>% select(-School_Type, -Gender)
model <- lm(Exam_Score ~ . ,data=Data_std2)
summary(model)

library(caret)
dummy_model <- dummyVars(~ ., data = Data_std2)

# Apply the model to create dummy variables
dummy_data <- predict(dummy_model, newdata = Data_std2)

# Convert to a data frame
dummy_data <- data.frame(dummy_data)

# View the result
print(dummy_data)
str(dummy_data)
```
```{r}
first_order_model <- lm(Exam_Score ~ 
                          Hours_Studied + Attendance + Parents_involvment + 
                          Access_to_resource + Extracurricular_Activities.No + 
                          Previous_Scores + Motivational_level + 
                          Internet_Access.No + Tutoring_Sessions + 
                          Family_income_. + Teacher_quality + 
                          Peer_Influence.Negative + Peer_Influence.Neutral + 
                          Physical_Activity + Learning_Disabilities.No + 
                          Distance_from_Home.Far + Distance_from_Home.Moderate,
                        data = dummy_data)

summary(first_order_model)

second_order_model <- lm(Exam_Score ~ 
                           Hours_Studied + Attendance + Parents_involvment + 
                           Access_to_resource + Extracurricular_Activities.No + 
                           Previous_Scores + Motivational_level + 
                           Internet_Access.No + Tutoring_Sessions + 
                           Family_income_. + Teacher_quality + 
                           Peer_Influence.Negative + Peer_Influence.Neutral + 
                           Physical_Activity + Learning_Disabilities.No + 
                           Distance_from_Home.Far + Distance_from_Home.Moderate + 
                           I(Hours_Studied^2) + I(Attendance^2) + 
                           I(Parents_involvment^2) + I(Access_to_resource^2) + 
                           I(Extracurricular_Activities.No^2) + 
                           I(Previous_Scores^2) + I(Motivational_level^2) + 
                           I(Internet_Access.No^2) + I(Tutoring_Sessions^2) + 
                           I(Family_income_.^2) + I(Teacher_quality^2) + 
                           I(Peer_Influence.Negative^2) + I(Peer_Influence.Neutral^2) + 
                           I(Physical_Activity^2) + I(Learning_Disabilities.No^2) + 
                           I(Distance_from_Home.Far^2) + I(Distance_from_Home.Moderate^2) + 
                           Hours_Studied:Attendance + Hours_Studied:Parents_involvment + 
                           Attendance:Parents_involvment +
                           Hours_Studied:Access_to_resource + 
                           Attendance:Access_to_resource + 
                           Hours_Studied:Previous_Scores + Hours_Studied:Motivational_level +
                           Attendance:Previous_Scores + Attendance:Motivational_level +
                           Parents_involvment:Access_to_resource + 
                           Parents_involvment:Previous_Scores + 
                           Parents_involvment:Motivational_level + 
                           Access_to_resource:Previous_Scores + 
                           Access_to_resource:Motivational_level,
                         data = dummy_data)
summary(second_order_model)

model <- lm(Exam_Score ~ . ,data=dummy_data)
summary(model)
both_model <- step(model, direction = "both")
summary(both_model)

plot(both_model)

install.packages("car")
library(car)

# Fit the model
model <- lm(Exam_Score ~ ., data = dummy_data)

vif(both_model)

# Ensure the dataset is correctly loaded into 'dummy_data'
# Check structure and presence of "Exam_Score"
str(dummy_data)  # Should show a column named "Exam_Score"

# Check correlation matrix excluding "Exam_Score"
cor(dummy_data[setdiff(names(dummy_data), "Exam_Score")], use = "complete.obs")

# Fit the initial model using the correct data argument
both_model <- lm(Exam_Score ~ ., data = dummy_data)

# Check for aliased coefficients
alias(both_model)

# Remove problematic predictors and refit the model if necessary
# For demonstration, let's assume "Predictor1" is removed
dummy_model <- dummy_data[, -which(names(dummy_data) == "Predictor1")]
dummy_model <- lm(Exam_Score ~ ., data = dummy_model)

# Check Variance Inflation Factors (VIF)
library(car)  # Load the car package
vif(both_model)

library(caret)  
library(ggplot2)  

set.seed(123)  

# Split data into training (80%) and testing (20%) sets
trainIndex <- createDataPartition(dummy_data$Exam_Score, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- dummy_data[trainIndex, ]
test_data <- dummy_data[-trainIndex, ]

#train model
model <- lm(Exam_Score ~ ., data = train_data)

#test set
predictions <- predict(model, newdata = test_data)

# see model performance
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