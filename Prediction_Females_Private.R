Std_per <- read.csv("S:\\Study\\OneDrive - DePaul University\\1st Quarter\\DSC 423 Data Analysis and regression\\Project stuff\\Data_set\\Student_performance.csv", stringsAsFactors=TRUE)
head(Std_per)
str(Std_per)

summary(Std_per)
colSums(is.na(Std_per))
td_per <- na.omit(Std_per)
colSums(is.na(Std_per))
Std_per <- Std_per[!duplicated(Std_per), ]
library(dplyr)

# Create a new data frame with conditions
Data_std <- Std_per %>%
  filter(Gender == "Female", School_Type == "Private")

print(Data_std)
library(dplyr)


ibrary(dplyr)

Data_std <- Data_std %>% select(-School_Type, -Gender)
model <- lm(Exam_Score ~ . ,data=Data_std)
summary(model)

library(caret)
dummy_model <- dummyVars(~ ., data = Data_std)

# Apply the model to create dummy variables
dummy_data <- predict(dummy_model, newdata = Data_std)

# Convert to a data frame
dummy_data <- data.frame(dummy_data)

# View the result
print(dummy_data)
str(dummy_data)

model <- lm(Exam_Score ~ . ,data=dummy_data)
summary(model)

backward_model <- step(model, direction = "backward")
summary(backward_model)


plot(backward_model)

# multicolinearity  
```{r}
library(dplyr)
library(car)

vif (backward_model)
# prediction 
```{r}
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
predictions <- predict(backward_model, newdata = test_data)

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
