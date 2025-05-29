# ---------------------------
# 📦 Load Required Packages
# ---------------------------
install.packages(c("ggplot2", "infotheo"))
library(ggplot2)
library(infotheo)

# ---------------------------
# 📁 Load Dataset
# ---------------------------
heart <- read.csv("/Users/mithizaman/Documents/12/Introduction to data science/code/Task_1/HeartDiseaseTrain-Test.csv")

# ---------------------------
# 🔄 Preprocess Columns
# ---------------------------
heart$sex <- factor(heart$sex, labels = c("Female", "Male"))
heart$cp <- factor(heart$cp)
heart$target <- factor(heart$target, labels = c("No Disease", "Disease"))

# Convert cholesterol to numeric if it's been incorrectly loaded or converted
heart$chol <- as.numeric(as.character(heart$chol))

# ---------------------------
# ✅ Lab Task 1: Visualization
# ---------------------------

# Univariate Plots Histogram
hist(heart$age, main = "Age Distribution", xlab = "Age", col = "lightsteelblue")

# Univariate Plots Bar Chart
barplot(table(heart$sex), main = "Sex Count", xlab = "Sex", col = c("lightpink", "lightblue"))

# Scatter plot (numeric vs numeric, colored by target)
ggplot(heart, aes(x = age, y = chol, color = target)) +
  geom_point() +
  labs(title = "Age vs Cholesterol", x = "Age", y = "Cholesterol")

# Violin plot (categorical vs numeric)
ggplot(heart, aes(x = cp, y = chol, fill = target)) +
  geom_violin(trim = FALSE) +
  labs(title = "Chest Pain Type vs Cholesterol", x = "Chest Pain Type", y = "Cholesterol") +
  scale_fill_manual(values = c("No Disease" = "orange", "Disease" = "yellow")) +
  theme_minimal()


install.packages("infotheo")
library(infotheo)

heart <- read.csv("/Users/mithizaman/Documents/12/Introduction to data science/code/Task_1/HeartDiseaseTrain-Test.csv")

# 🔄 Preprocess

heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$target <- factor(heart$target)
heart$target_num <- as.numeric(as.character(heart$target))  # for correlation

# (Only Numeric Columns)

cat("Pearson Correlation:")
print(cor.test(heart$age, heart$target_num))
print(cor.test(heart$chol, heart$target_num))


# ANOVA (Categorical vs Numeric Target)

cat("\nANOVA :\n")
print(summary(aov(target_num ~ sex, data = heart)))
print(summary(aov(target_num ~ cp, data = heart)))

# ---------------------------
# Chi-Squared Test (Categorical vs Categorical)
# ---------------------------
cat("\nChi-Squared :\n")
print(chisq.test(heart$sex, heart$target, correct = FALSE))

print(chisq.test(heart$cp, heart$target, correct = FALSE))

# ---------------------------
# 📌 4. Mutual Information (Discretize Numeric Variables First)
# ---------------------------
# Discretize age and cholesterol
heart$age_disc <- discretize(heart$age, disc = "equalfreq", nbins = 5)
heart$chol_disc <- discretize(heart$chol, disc = "equalfreq", nbins = 5)

cat("\n📌 Mutual Information:\n")
print(mutinformation(heart$age_disc, heart$target))
print(mutinformation(heart$sex, heart$target))
print(mutinformation(heart$cp, heart$target))
print(mutinformation(heart$chol_disc, heart$target))


