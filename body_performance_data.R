#Project: Looking at how total number of sit-ups completed compares to physical factors such as body composition and age
#After analyzing the original data, I created a predictive model to see if there is an accurate way to predict the number of sit-ups an individual can complete based on body composition
#This data is provided by the "Body performance Data" data set by Kukuroo3 (2022). This data set was discovered on Kaggle

#Opening required libraries:
library(dplyr)
library(corrplot)
library(ggplot2)

#Importing the data:
file_path = "/Users/stevenleicht/Downloads/body_performance_data.csv"
bp_data = read.csv(file_path)
head(bp_data)
summary(bp_data)
str(bp_data)

#Selecting the data:
selected_bp_data = bp_data %>%
  select(sit.ups.counts, age, height_cm, weight_kg, body.fat_.)
print(selected_bp_data)

#Scaling/converting 'height_cm' to inches for proper analysis purposes:
scaled_height_data = selected_bp_data %>%
  mutate(height_cm * 0.393701)
print(scaled_height_data)

#Running a correlation matrix to determine if there is a correlation for sit-ups with body composition:
cor_matrix <- cor(selected_bp_data, use = "complete.obs")
print(cor_matrix)
#Comparing age to sit-ups, it seems that there is a strong negative relationship. Moreover, the older the individual, the fewer sit-ups they can perform (-0.54)
#Comparing height to sit-ups, it seems that there is a mild positive relationship. Moreover, the taller the individual, the more likely they are to perform more sit-ups (0.50)
#Comparing weight to sit-ups, it seems that there is a slight positive relationship. Moreover, the heavier the individual, the more likely they are to perform more sit-ups (0.29)
#Comparing body fat percentage to sit-ups, it seems that there is a strong negative relationship. Moreover, the higher the body fat percentage, the fewer sit-ups completed (-0.61)
#Conclusion: it is certain that the younger an individual is, along with the lower their body fat percentage, the greater amount of sit-ups they can perform. Also, the taller and heavier the individual, the greater amount of sit-ups they can perform, but this is not as statistically significant

#Seeing if the correlation matrix is statistically significant:
cor.mtest = function(mat, conf.level = 0.95) {
  mat = as.matrix(mat)
  n = ncol(mat)
  p.mat = matrix(NA, n, n)
  diag(p.mat) = 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp = cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] = p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) = rownames(p.mat) <- colnames(mat)
  p.mat
}
p_values_matrix = cor.mtest(selected_bp_data)
print(p_values_matrix)
#The p-value is <0.001 when comparing all selected factors of body composition to sit-ups, showcasing that the correlation matrix is statistically significant

#Creating a predictive model (multiple linear regression model) to predict number of sit-ups based on body composition:
mlr_model <- lm(sit.ups.counts ~ age + height_cm + weight_kg + body.fat_., data = selected_bp_data)
summary(mlr_model)
#The residual standard error is 9.181 sit-ups on 13,388 degrees of freedom
#The adjusted r-squared value is 0.5864. In this case, that means that the model is relatively similar and significant to the actual values
#The p-value is <0.001, meaning that the model is very statistically significant

#Now, I will compare the predictive model to the actual data to see how accurate the predictive model is at estimating the number of sit-ups completed for each individual
pm_vs_ad = 91.686 - 0.439 * selected_bp_data$age - 0.206 * selected_bp_data$height_cm +
  0.354 * selected_bp_data$weight_kg - 1.073 * selected_bp_data$body.fat_.
print(pm_vs_ad)

#Creating a visual to measure the differences between the actual data and the predictive model:
#First, determining if the number of sit-ups from the actual data is the same from the predictive model:
print(selected_bp_data$sit.ups.counts)
if(length(pm_vs_ad) != length(selected_bp_data$sit.ups.counts)) {
  stop("The lengths of pm_vs_ad and sit_ups_counts must be the same.")
}

#Creating a separate data frame for visualization purposes:
visual_data <- data.frame(
  Index = seq_along(pm_vs_ad),
  Predicted = pm_vs_ad,
  Actual = selected_bp_data$sit.ups.counts
)

#Creating a plot to measure the differences:
ggplot(visual_data, aes(x = Index)) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1, alpha = 0.5) +
  labs(title = "Actual vs. Predicted Sit-Ups by Body Composition",
       x = "Individual",
       y = "Sit-Ups",
       color = "Legend") +
  xlim(c(14, 113)) +
  theme_minimal()

#Overall conclusion: there are definitely significant correlations between the number of sit-ups performed and... 
#body composition. The predictive model does an adequate job at predicting the total number of sit-ups compared to...
#the actual data. However, there are plenty of other factors that can relate to physical performance...
#I'm interested in continuing this research and discovering a more profound version or model to calculate overall... 
#physical performance for multiple populations (athletes, general public, etc.)
