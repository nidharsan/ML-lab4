# Set a fixed random seed for reproducibility
set.seed(10923)

# Number of students
num_students <- 500

# Simulate study hours (ranging from 1 to 20 hours)
study_hours <- sample(1:20, num_students, replace = TRUE)

# Simulate quiz scores (ranging from 0 to 100)
quiz_scores <- sample(0:100, num_students, replace = TRUE)

# Simulate forum participation (ranging from 0 to 50 posts)
forum_posts <- sample(0:50, num_students, replace = TRUE)

# Simulate previous grades (ranging from 0 to 100)
previous_grades <- sample(0:100, num_students, replace = TRUE)

# Simulate final grades (ranging from 0 to 100)
final_grades <- 0.3 * study_hours + 0.4 * quiz_scores + 0.2 * forum_posts + 0.1 * previous_grades + rnorm(num_students, mean = 0, sd = 5) + 25

# Create a data frame
student_data <- data.frame(StudyHours = study_hours, QuizScores = quiz_scores, ForumPosts = forum_posts, PreviousGrades = previous_grades, FinalGrades = final_grades)

# View the first few rows of the generated data
head(student_data)

# Splitting the data into training and testing sets (80% training, 20% testing)
set.seed(10923) # Set seed for reproducibility
sample_index <- sample(1:nrow(student_data), 0.8 * nrow(student_data))
train_data <- student_data[sample_index, ]
test_data <- student_data[-sample_index, ]

# Building models with increasing number of features
model1 <- lm(FinalGrades ~ StudyHours, data = train_data)
model2 <- lm(FinalGrades ~ StudyHours + QuizScores, data = train_data)
model3 <- lm(FinalGrades ~ StudyHours + QuizScores + ForumPosts, data = train_data)
model4 <- lm(FinalGrades ~ StudyHours + QuizScores + ForumPosts + PreviousGrades, data = train_data)

# Making predictions on the test set for each model
predictions1 <- predict(model1, newdata = test_data)
predictions2 <- predict(model2, newdata = test_data)
predictions3 <- predict(model3, newdata = test_data)
predictions4 <- predict(model4, newdata = test_data)

# Evaluation metrics: Mean Squared Error (MSE)
mse1 <- mean((test_data$FinalGrades - predictions1)^2)
mse2 <- mean((test_data$FinalGrades - predictions2)^2)
mse3 <- mean((test_data$FinalGrades - predictions3)^2)
mse4 <- mean((test_data$FinalGrades - predictions4)^2)

# Print MSE for each model
cat("MSE Model 1:", mse1, "\n")
cat("MSE Model 2:", mse2, "\n")
cat("MSE Model 3:", mse3, "\n")
cat("MSE Model 4:", mse4, "\n")

# Model Accuracy based on Prediction Interval
# Get the predictions and prediction intervals for each model
pred_int1 <- predict(model1, newdata = test_data, interval = "prediction")
pred_int2 <- predict(model2, newdata = test_data, interval = "prediction")
pred_int3 <- predict(model3, newdata = test_data, interval = "prediction")
pred_int4 <- predict(model4, newdata = test_data, interval = "prediction")

# Extract lower and upper bounds of the prediction interval for each model
lower_bound1 <- pred_int1[, "lwr"]
upper_bound1 <- pred_int1[, "upr"]
lower_bound2 <- pred_int2[, "lwr"]
upper_bound2 <- pred_int2[, "upr"]
lower_bound3 <- pred_int3[, "lwr"]
upper_bound3 <- pred_int3[, "upr"]
lower_bound4 <- pred_int4[, "lwr"]
upper_bound4 <- pred_int4[, "upr"]

# Compute accuracy for each model
accuracy1 <- sum(test_data$FinalGrades >= lower_bound1 & test_data$FinalGrades <= upper_bound1) / nrow(test_data)
accuracy2 <- sum(test_data$FinalGrades >= lower_bound2 & test_data$FinalGrades <= upper_bound2) / nrow(test_data)
accuracy3 <- sum(test_data$FinalGrades >= lower_bound3 & test_data$FinalGrades <= upper_bound3) / nrow(test_data)
accuracy4 <- sum(test_data$FinalGrades >= lower_bound4 & test_data$FinalGrades <= upper_bound4) / nrow(test_data)

# Print accuracy for each model
cat("Model 1 Accuracy using Prediction Interval:", accuracy1, "\n")
cat("Model 2 Accuracy using Prediction Interval:", accuracy2, "\n")
cat("Model 3 Accuracy using Prediction Interval:", accuracy3, "\n")
cat("Model 4 Accuracy using Prediction Interval:", accuracy4, "\n")