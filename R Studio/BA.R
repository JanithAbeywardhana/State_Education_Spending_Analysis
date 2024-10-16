EduStates_New <- read.csv("C:/Users/User/Desktop/BA Final/CSE5014- Writ 1/EduStates_New.csv")

#find minimum
min(EduStates_New$dollars)


#Maximum of EduStates
max(EduStates_New$dollars)


#mean value of EduStates
mean(EduStates_New$dollars)

#median Value of EduStates
median(EduStates_New$dollars)

#mode Value of EduStates
mode(EduStates_New$dollars)


#summary of EduStates
summary(EduStates_New$dollars)
sd(EduStates_New$dollars)

install.packages("dplyr")
install.packages("ggplot2")
install.packages("DescTools")
install.packages("nortest")
library(Rcmdr)


#Task 05 Percent
mean(EduStates_New$percent)
mode(EduStates_New$percent)
median(EduStates_New$percent)
var(EduStates_New$percent)
sd(EduStates_New$percent)
hist(EduStates_New$percent, prob = TRUE, main = "percent Distribution", xlab = "percent", ylab = "Density")
View(EduStates_New)
curve(dnorm(x, mean = mean(EduStates_New$percent),sd = sd(EduStates_New$percent)), add = TRUE, col="blue")



#Task 05 dollars
mean(EduStates_New$dollars)
mode(EduStates_New$dollars)
median(EduStates_New$dollars)
var(EduStates_New$dollars)
sd(EduStates_New$dollars)
hist(EduStates_New$dollars, prob = TRUE, main = "dollars Distribution", xlab = "dollars", ylab = "Density")
View (EduStates_New)
curve(dnorm(x, mean = mean(EduStates_New$dollars), sd = sd(EduStates_New$dollars)),add = TRUE, col= "red")


#Task 05 Pay
mean(EduStates_New$pay)
mode(EduStates_New$pay)
median(EduStates_New$pay)
var(EduStates_New$pay)
sd(EduStates_New$pay)
hist(EduStates_New$pay, prob = TRUE, main = "Pay Distribution", xlab = "pay", ylab = "Density")
View(EduStates_New)
curve(dnorm(x, mean = mean(EduStates_New$pay), sd = sd(EduStates_New$pay)), add = TRUE, col="green")


#Task 05 Poppulaton
mean(EduStates_New$pop)
mode(EduStates_New$pop)
median(EduStates_New$pop)
var(EduStates_New$pop)
sd(EduStates_New$pop)
hist(EduStates_New$pop, prob = TRUE, main = "pop Distribution", xlab = "pop", ylab = "Density")
View(EduStates_New)
curve(dnorm(x, mean = mean(EduStates_New$pop), sd = sd(EduStates_New$pop)), add = TRUE, col = "blue")


# View the first few rows of the data
head(data)

# Calculate summary statistics for Percent and Dollars
summary(data$Percent)
summary(data$Dollars)

# Perform a correlation test
cor_test <- cor.test(data$Percent, data$Dollars)

# Print the correlation test results
print(cor_test)

# Create a scatter plot to visualize the relationship between Percent and Dollars
ggplot(data, aes(x = Percent, y = Dollars)) +
  geom_point() +
  labs(title = "Scatter Plot of Percentage of Students Taking SAT vs State Spending",
       x = "Percentage of Students Taking SAT",
       y = "State Spending on Public Education per Student ($1000s)") +
  theme_minimal()

# Fit a linear regression model
lm_model <- lm(Dollars ~ Percent, data = data)

# Print the summary of the linear regression model
summary(lm_model)

summary(EduStates_New$percent)
sd(EduStates_New$percent)

summary(EduStates_New$pop)
sd(EduStates_New$pop)

summary(EduStates_New$pay)
sd(EduStates_New$pay)

shapiro.test(EduStates_New$percent)

#shapiro test for dollars
shapiro.test(EduStates_New$dollars)

#His for percent 
hist(EduStates_New$percent, main = "Histogram for percent", xlab = "percent")
qqnorm(EduStates_New$percent)
qqline(EduStates_New$percent, col = "blue")

correlation_result <- cor.test(EduStates_New$percent, EduStates_New$dollars, method = "pearson")
correlation_result <- cor.test(EduStates_New$percent, EduStates_New$dollars, method = "spearman")
print(correlation_result)

install.packages("nortest")
library(nortest)

ad.test(EduStates_New$percent)

#ad test for dollars
ad.test(EduStates_New$dollars)

#pearson test for percent
pearson.test(EduStates_New$percent)

#pearson test for dollars
pearson.test(EduStates_New$dollars)

#pearson correlation 
pearson_correlation_result <- cor.test(EduStates_New$percent, EduStates_New$dollars, method = "pearson")
print(pearson_correlation_result)

#8th Question 

#shapiro test for percent
shapiro.test(EduStates_New$percent)


#shapiro test for pay
shapiro.test(EduStates_New$pay)

#Anderson darling test for Percent 
ad.test(EduStates_New$percent)

#Anderson darling test for pay 
ad.test(EduStates_New$pay)

#Pearson test for percent 
pearson.test(EduStates_New$percent)

#Pearson test for pay 
pearson.test(EduStates_New$pay)


#pearson correlation 
pearson_correlation_result <- cor.test(EduStates_New$percent, EduStates_New$pay, method = "pearson")
print(pearson_correlation_result)


#9th Question 

#shapiro test for percent 
shapiro.test(EduStates_New$percent)

#shapiro test for pop 
shapiro.test(EduStates_New$pop)

#Anderson darling test for percent
ad.test(EduStates_New$percent)

#Anderson darling test for pop
ad.test(EduStates_New$pop)

#Pearson test for percent
pearson.test(EduStates_New$percent)

#Pearson test for pop
pearson.test(EduStates_New$pop)


#pearson correlation 
pearson_correlation_result <- cor.test(EduStates_New$percent, EduStates_New$pop, method = "pearson")
print(pearson_correlation_result)

model <- lm(dollars ~ percent + pay, data = EduStates_New)
summary(model)

model <- lm(percent ~ dollars, data = EduStates_New)
summary(model)

plot(EduStates_New$dollars, EduStates_New$percent,
     main = "Scatterplot of Percent vs Dollars",
     xlab = "Dollars",
     ylab = "Percent")
abline(lm(percent ~ dollars, data = EduStates_New), col = "red")

model <- lm(percent ~ pay, data = EduStates_New)
summary(model)

plot(EduStates_New$pay, EduStates_New$percent,
     main = "Scatterplot of Percent vs pay",
     xlab = "Pay",
     ylab = "Percent")
abline(lm(percent ~ pay, data = EduStates_New), col = "blue")


model <- lm(percent ~ pop, data = EduStates_New)
summary(model)

attach(EduStates_New)
plot(EduStates_New$pop, EduStates_New$percent,
     main = "Scatterplot of Percent vs Pop",
     xlab = "Pop",
     ylab = "Percent")
abline(lm(percent ~ pop, data = EduStates_New), col = "black")
