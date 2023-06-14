# Load the stroke dataset
stroke_data <- read.csv("D:/2nd Year - 2nd Semerster/Statistical Data Modelling/Assignment Data_Set/healthcare-dataset-stroke-data.csv")

# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
install.packages("caTools")
library(caTools)
split <- sample.split(stroke_data$stroke, SplitRatio = 0.7)
train_data <- subset(stroke_data, split == TRUE)
test_data <- subset(stroke_data, split == FALSE)

# Convert bmi to a factor with levels that are present in the training data
levels <- unique(train_data$bmi)
test_data$bmi <- factor(test_data$bmi, levels = levels)

# Perform logistic regression on the training data
model <- glm(stroke ~ ., data = train_data, family = binomial)

# Make predictions on the testing data
predictions <- predict(model, newdata = test_data, type = "response")

# Convert probabilities to binary predictions
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the accuracy of the predictions
library(caret)
confusionMatrix(table(binary_predictions, test_data$stroke))
print(confusionMatrix(factor(binary_predictions), factor(test_data$stroke)))




#finding the datatype of bmi
class(stroke_data$bmi)
#converting the type of bmi
stroke_data$bmi <- as.numeric(stroke_data$bmi)

# Create a new variable to group BMI into three categories
stroke_data$BMI_group <- cut(stroke_data$bmi, breaks = c(0, 18.5, 25, Inf), labels = c("Underweight/Normal", "Overweight", "Obese"))

# View the new variable
head(stroke_data$BMI_group)




# Extracting avg_glucose_level values corresponding to each BMI group
underweight_normal <- stroke_data[stroke_data$BMI_group == "Underweight/Normal",]$avg_glucose_level
overweight <- stroke_data[stroke_data$BMI_group == "Overweight",]$avg_glucose_level
obese <- stroke_data[stroke_data$BMI_group == "Obese",]$avg_glucose_level


# Perform Shapiro-Wilk test for normality on avg_glucose_level
shapiro_test <- shapiro.test(underweight_normal)
shapiro_test <- shapiro.test(overweight)
shapiro_test <- shapiro.test(obese)
#since the p value of all the group are 2.2e-16< p value , we reject the null hypothesis

#since the data doesn't follow a normal distribution, we will use kruskal-walis test
#Ho: there is no difference between the avg_glucose_level among the 3 BMI groups
#H1: there is a difference between the avg_glucose_level among the 3 BMI groups
kruskal.test(avg_glucose_level~BMI_group, data=stroke_data)

# Decision: Since p-value < 0.05, we reject H0.

# Conclusion: 
# We conclude that at least two group means are significantly different at 0.05 significance level.



# We can use pairwise.wilcox.test() to calculate pairwise comparisons between 
# group levels with corrections for multiple testing.

help(pairwise.wilcox.test)

# Run pairwise Wilcoxon rank-sum test with Benjamini-Hochberg adjustment
pairwise.wilcox.test(stroke_data$avg_glucose_level, stroke_data$BMI_group, p.adjust.method = "BH")

# Result:
#H0: There is no significant difference between obese and underweight/Normal
# The p-value is 3.3e-05  (< 0.05) . Therefore we reject the null hypothesis
# We can conclude that there is a significant difference between obese and underweight/Normal at 0.05 level.


#H0: There is no significant difference between obese and overweight
# The p-value is 3.0e-07    (< 0.05) . Therefore we reject the null hypothesis

# We can conclude that there is a significant difference between obese and overweight at 0.05 level.
