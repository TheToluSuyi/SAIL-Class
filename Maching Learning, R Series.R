#install caTools
install.packages("caTools")

# Load necessary libraries
library(ggplot2)
library(caTools)
# Load the dataset
data <- mtcars
# View the first few rows of the dataset
head(data)
# Set seed for reproducibility
set.seed(123)
# Split the data into training (70%) and testing (30%) sets
split <- sample.split(data$mpg, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
# Train the linear regression model
model <- lm(mpg ~ wt, data = train_data)
summary(model)
# Make predictions on the test set
predictions <- predict(model, newdata = test_data)
# Compare predicted vs actual values
comparison <- data.frame(Actual = test_data$mpg, Predicted = predictions)
comparison
# Plot the data and the regression line
ggplot(train_data, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Linear Regression: MPG vs Weight", x = "Weight (1000 lbs)", y = "MPG")
#############################################
#Unsupervised Learning
