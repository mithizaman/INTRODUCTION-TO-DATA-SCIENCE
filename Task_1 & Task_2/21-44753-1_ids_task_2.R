install.packages("infotheo")
library(infotheo)

heart <- read.csv("/Users/mithizaman/Documents/12/Introduction to data science/code/Task_1/HeartDiseaseTrain-Test.csv")

heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$target <- factor(heart$target)
heart$target_num <- as.numeric(as.character(heart$target))  # for correlation


cat("Pearson Correlation:")
print(cor.test(heart$age, heart$target_num))
print(cor.test(heart$chol, heart$target_num))


cat("\nANOVA :\n")
print(summary(aov(target_num ~ sex, data = heart)))
print(summary(aov(target_num ~ cp, data = heart)))


cat("\nChi-Squared :\n")
print(chisq.test(heart$sex, heart$target, correct = FALSE))
print(chisq.test(heart$cp, heart$target, correct = FALSE))


heart$age_disc <- discretize(heart$age, disc = "equalfreq", nbins = 5)
heart$chol_disc <- discretize(heart$chol, disc = "equalfreq", nbins = 5)
cat("\nMutual Information:\n")
print(mutinformation(heart$age_disc, heart$target))
print(mutinformation(heart$sex, heart$target))
print(mutinformation(heart$cp, heart$target))
print(mutinformation(heart$chol_disc, heart$target))
