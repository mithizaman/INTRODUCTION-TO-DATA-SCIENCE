Dataset_mid<-read.csv("/Users/mithizaman/Documents/9th semester/INTRODUCTION TO DATA SCIENCE/mid_project/mid_project.csv",header = TRUE,sep = ",")
Dataset_mid
colSums(is.na(Dataset_mid))
Dataset_mid$Caesarian[is.na(Dataset_mid$Caesarian)] <- mean(Dataset_mid$Caesarian, na.rm = TRUE)
Dataset_mid<- na.omit(Dataset_mid)
Dataset_mid
Dataset_mid[,3]
max(Dataset_mid$Gender)
Dataset_mid$Gender<-edit(Dataset_mid$Gender) 
Dataset_mid
Dataset_mid[,4]
max(Dataset_mid$weight.kg.) 
Dataset_mid$weight.kg.<-edit(Dataset_mid$weight.kg.) 
Dataset_mid
Dataset_mid[,7]
max(Dataset_mid$Blood) 
Dataset_mid$Blood<-edit(Dataset_mid$Blood) 
Dataset_mid

Dataset_mid$Gender
invalid_indices <- grep("Mmale", Dataset_mid$Gender)
Dataset_mid$Gender[invalid_indices] <- "Male"
Dataset_mid
Dataset_mid$Gender
invalid_indices <- grep("Feemale", Dataset_mid$Gender)
Dataset_mid$Gender[invalid_indices] <- "Female"
Dataset_mid
Dataset_mid$Gender<-edit(Dataset_mid$Gender) 
Dataset_mid
Dataset_mid$weight.kg.
invalid_indices <- grep("75X", Dataset_mid$weight.kg.)
Dataset_mid$weight.kg.[invalid_indices] <- 75
Dataset_mid
Dataset_mid$Caesarian<- as.numeric(format(round(Dataset_mid$Caesarian,0)))
Dataset_mid
Dataset_mid$weight.kg. <- as.numeric(format(Dataset_mid$weight.kg.,0)) 
Dataset_mid
Dataset_mid$Heart <- factor(Dataset_mid$Heart,levels = c(1,0),labels = c("Positive", "Negative"))
Dataset_mid
round(Dataset_mid$weight.kg.)
Dataset_mid

mean(Dataset_mid$Age)
median(Dataset_mid$Age)
sd(Dataset_mid$Age)


mean(Dataset_mid$weight.kg.)
median(Dataset_mid$weight.kg.)
sd(Dataset_mid$weight.kg.)

mean(Dataset_mid$Delivery_number)
median(Dataset_mid$Delivery_number)
sd(Dataset_mid$Delivery_number)

mean(Dataset_mid$Delivery_time)
median(Dataset_mid$Delivery_time)
sd(Dataset_mid$Delivery_time)


hist(Dataset_mid$Age) 
hist(Dataset_mid$weight.kg.,col=3) 
hist(Dataset_mid$Delivery_number ,col=5)
hist(Dataset_mid$Delivery_time ,col=7)
hist(Dataset_mid$Caesarian,col=4)
barplot(table(Dataset_mid$Gender), ylab = "Frequency", xlab = "Gender")

install.packages("dplyr") 
install.packages("matrixStats") 
library(matrixStats) 
library(dplyr)
Dataset_mid %>% summarise_if(is.numeric, sd)

plot(Dataset_mid$Age,col=5) 
plot(Dataset_mid$weight.kg., col=8)
plot(Dataset_mid$Delivery_number, col=7) 
plot(Dataset_mid$Delivery_time, col=6)
plot(Dataset_mid$Caesarian , col=10)

age_bounds <- quantile(Dataset_mid$age, c(0.25, 0.75)) 
IQR_age <- IQR(Dataset_mid$age)
lower_age <- age_bounds[1] - 1.5 * IQR_age
upper_age <- age_bounds[2] + 1.5 * IQR_age
Dataset_mid<- Dataset_mid[Dataset_mid$age >= lower_age & Dataset_mid$age <= upper_age,]

Q1 <- quantile(Dataset_mid$weight.kg., 0.25)
Q3 <- quantile(Dataset_mid$weight.kg., 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

Dataset_mid <- Dataset_mid[Dataset_mid$Age >= lower_bound & Dataset_mid$Age <= upper_bound,]
plot(Dataset_mid$Age,col=5) 
Q1 <- quantile(Dataset_mid$weight.kg., 0.25)
Q3 <- quantile(Dataset_mid$weight.kg., 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

Dataset_mid <- Dataset_mid[Dataset_mid$weight.kg.>= lower_bound & Dataset_mid$weight.kg. <= upper_bound,]
plot(Dataset_mid$weight.kg.,col=8)

Q1 <- quantile(Dataset_mid$Delivery_number, 0.25)
Q3 <- quantile(Dataset_mid$Delivery_number, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

Dataset_mid <- Dataset_mid[Dataset_mid$Delivery_number>= lower_bound & Dataset_mid$Delivery_number <= upper_bound,]
plot(Dataset_mid$Delivery_number ,col=7)

Dataset_mid
