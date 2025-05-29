install.packages(c("ggplot2", "infotheo"))
library(ggplot2)
library(infotheo)

heart <- read.csv("/Users/mithizaman/Documents/12/Introduction to data science/code/Task_1/HeartDiseaseTrain-Test.csv")

heart$sex <- factor(heart$sex, labels = c("Female", "Male"))
heart$cp <- factor(heart$cp)
heart$target <- factor(heart$target, labels = c("No Disease", "Disease"))

heart$chol <- as.numeric(as.character(heart$chol))



hist(heart$age, main = "Age Distribution", xlab = "Age", col = "lightsteelblue")



barplot(table(heart$sex), main = "Sex Count", xlab = "Sex", col = c("lightpink", "lightblue"))




ggplot(heart, aes(x = age, y = chol, color = target)) +
  geom_point() +
  labs(title = "Age vs Cholesterol", x = "Age", y = "Cholesterol")




ggplot(heart, aes(x = cp, y = chol, fill = target)) +
  geom_violin(trim = FALSE) +
  labs(title = "Chest Pain Type vs Cholesterol", x = "Chest Pain Type", y = "Cholesterol") +
  scale_fill_manual(values = c("No Disease" = "orange", "Disease" = "yellow")) +
  theme_minimal()


