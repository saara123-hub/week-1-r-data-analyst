# Week 1 - Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic Passenger Dataset

library(ggplot2)

# 1. Import
titanic <- read.csv("data/titanic.csv")

# 2. Inspect
head(titanic)
str(titanic)
summary(titanic)

# 3. Missing values
colSums(is.na(titanic))

# 4. Missing-value treatment
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

titanic$Embarked[is.na(titanic$Embarked)] <- get_mode(titanic$Embarked)

# 5. Duplicate check
sum(duplicated(titanic))

# 6. Convert categorical variables to factors
titanic$Survived <- factor(titanic$Survived,
                            levels = c(0, 1),
                            labels = c("No", "Yes"))
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)

# 7. Outlier inspection
boxplot(titanic$Age, main = "Age Outliers", ylab = "Age")
boxplot(titanic$Fare, main = "Fare Outliers", ylab = "Fare")

# 8. Min-max normalization
min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
titanic$Fare_scaled <- min_max(titanic$Fare)

# 9. Descriptive statistics
summary(titanic$Age)
summary(titanic$Fare)
table(titanic$Sex)
table(titanic$Pclass)
table(titanic$Survived)

# 10. Visualizations
ggplot(titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Count by Sex",
       x = "Sex", y = "Number of Passengers")

ggplot(titanic, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(title = "Survival Count by Passenger Class",
       x = "Passenger Class", y = "Number of Passengers")

ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(title = "Age Distribution of Passengers",
       x = "Age", y = "Count")

ggplot(titanic, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.5) +
  labs(title = "Age vs Fare", x = "Age", y = "Fare")

# 11. Correlation
numeric_data <- titanic[, c("Age", "SibSp", "Parch", "Fare")]
cor(numeric_data, use = "complete.obs")
