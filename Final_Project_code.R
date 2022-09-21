setwd("D:/MPS Analytics/ProbAndStats/Code/Datasets")
install.packages(c("tidyverse", "ggplot2", "Hmisc","dplyr","reshape", "car" )) #Importing required packages
lapply(c("tidyverse", "ggplot2", "Hmisc", "reshape", "dplyr", "car"), library, character.only = TRUE) #Loading the imported packages
Walmart.sales <- read.csv("Walmart_Store_sales.csv", header = TRUE, sep = ",") #importing the train dataset from working directory
summary(Walmart.sales) #Summary of the dataset
str(Walmart.sales) # Structure of the dataset
describe(Walmart.sales) #Describe the dataset
Walmart.sales <- rename(Walmart.sales, c(Store ="Store.Num", Unemployment = "Unemployment.rate", Date = "Week.Date", Holiday_Flag = "Holiday.Flag", Fuel_Price = "Fuel.Price", Weekly_Sales = "Weekly.Sales")) #Renaming the vaibles
Walmart.sales$Holiday.Flag<- as.factor(Walmart.sales$Holiday.Flag) #Converting holiday flag to factor datatype
Walmart.sales$Week.Date <- as.Date(Walmart.sales$Week.Date, tryFormats = c("%d/%m/%Y", "%d-%m-%Y")) #Converting the Week.Date data tye from character to date 
Year <- subset(separate(Walmart.sales, "Week.Date", c("Year", "Month", "Day"), sep = "-"), select = Year) #Extracting week from the Week.Date
summary(Year)
max(Walmart.sales$Weekly.Sales)
Year <- as.numeric(unlist(Year))
Walmart.sales <- cbind(Walmart.sales, Year) #Adding Year to the Walmart.sales dataset
nrow(Walmart.sales) #Counting number of rows
names(Walmart.sales) #variables of the dataset
options(scipen= 9999) #setting scipen= 9999 to avoid the scientific values

#Eliminating Outliers 
Q <- quantile(Walmart.sales$Weekly.Sales, probs=c(.75), na.rm = FALSE) #Calculating third quantile probability
iqr <- IQR(Walmart.sales$Weekly.Sales) #Computing IQR
QU <- quantile(Walmart.sales$Unemployment.rate, probs=c(.25, .75), na.rm = FALSE) #Calculating third quantile probability
iqru <- IQR(Walmart.sales$Unemployment.rate) ##Computing IQR
QT <- quantile(Walmart.sales$Temperature, probs=c(.25), na.rm = FALSE) #Calculating third quantile probability
iqrt <- IQR(Walmart.sales$Temperature) #Computing IQR
unemployment.mean <- mean(Walmart.sales$Unemployment.rate) #Computing Mean
sales.mean <- mean(Walmart.sales$Weekly.Sales)#Computing Mean
temp.mean <- mean(Walmart.sales$Temperature) #Computing Mean
Walmart.sales$Weekly.Sales <- ifelse(Walmart.sales$Weekly.Sales < (Q[1]+1.5*iqr), Walmart.sales$Weekly.Sales, sales.mean) # Replacing the Outliers with mean 
Walmart.sales$Unemployment.rate <- ifelse((Walmart.sales$Unemployment.rate > (QU[1]-1.5*iqru)) & (Walmart.sales$Unemployment.rate < (QU[2]+1.5*iqru)), Walmart.sales$Unemployment.rate, unemployment.mean) # Replacing the Outliers with mean 
Walmart.sales$Unemployment.rate <- ifelse((Walmart.sales$Unemployment.rate > (QU[1]-1.5*iqru)), Walmart.sales$Unemployment.rate, unemployment.mean) # Replacing the Outliers with mean 
Walmart.sales$Temperature <- ifelse((Walmart.sales$Temperature > (QT[1]-1.5*iqrt)), Walmart.sales$Temperature, temp.mean) # Replacing the Outliers with mean 
str(Walmart.sales) #Structure of the dataset
summary(Walmart.sales) #To get the summary repor of datset

#Collecting the sample for regression
Walmart.sample <- sample_n(Walmart.sales, 28) # Collecting 28 random samples from the Walmart.sales dataset
str(Walmart.sample)
summary(Walmart.sample)
psych::describe(Walmart.sample, skew = F) #Describing the dataset

# Box plots for understanding the distribution of sample data.

ggplot(Walmart.sample, aes(Weekly.Sales))+ #Box plot
  geom_boxplot(color = "green")+
  labs(title = "Weekly sales box plot", x = "Weekly Sales") 
ggplot(Walmart.sample, aes(Unemployment.rate))+ #Box Plot
  geom_boxplot(color = "green")+
  labs(title = "Unemployment.rate box plot", x = "Unemployment.rate")
ggplot(Walmart.sample, aes(Fuel.Price))+ #Box Plot
  geom_boxplot(color = "green")+
  labs(title = "Fuel.Price box plot", x = "Fuel.Price")

par(mar=c(4,4,1,1))
qqnorm(Walmart.sample$Weekly.Sales, main = "Normal Q-Q Plot for weekly sales", col = "black", pch = 16) #Normal Q-Q Plot
qqline(Walmart.sample$Weekly.Sales, col="red")
qqnorm(Walmart.sample$Fuel.Price, main = "Normal Q-Q Plot for Fuel Price", col = "black", pch = 16) #Normal Q-Q Plot
qqline(Walmart.sample$Fuel.Price, col="red")
qqnorm(Walmart.sample$Unemployment.rate, main = "Normal Q-Q Plot for Unemployment rate", col = "black", pch = 16) #Normal Q-Q Plot
qqline(Walmart.sample$Unemployment.rate, col="red")

#Regression 1:
regressionfit1 <- lm(Weekly.Sales ~ Fuel.Price, data = Walmart.sample)
summary(regressionfit1)
ggplot(data = Walmart.sample, mapping = aes(x = Fuel.Price, y = Weekly.Sales))+ #Scatter Plot
  geom_point(color = "green", alpha = .7, size = 2)+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5)+
  scale_y_continuous(label = scales::dollar)+
  labs(title = "Fuel.Price and Weekly.Sales regression", 
       subtitle = "Sales of 45 walmart retail stores",
       x = "Fuel.Price", 
       y = "Weekly Sales")
a <- data.frame(Fuel.Price = 6) # creating the data frame for the regression model
predict(regressionfit1, a) #predicting the weekly sales  value


#Regression 2:
regressionfit2 <- lm(Weekly.Sales ~ Unemployment.rate, data = Walmart.sample)
summary(regressionfit2)
ggplot(data = Walmart.sample, mapping = aes(x = Unemployment.rate, y = Weekly.Sales))+ #Scatter Plot
  geom_point(color = "green", alpha = .7, size = 2)+
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5)+
  scale_y_continuous(label = scales::dollar)+
  labs(title = "Unemployment.rate and Weekly.Sales Regression", 
       subtitle = "Sales of 45 walmart retail stores",
       x = "Unemployment rate", 
       y = "Weekly Sales")
a <- data.frame(Unemployment.rate = 8) # creating the data frame for the regression model
predict(regressionfit2, a)

#Multi variable regression
reg.model <- lm(Weekly.Sales ~ Fuel.Price+ Unemployment.rate, data = Walmart.sample) #Creating the regression model
summary(reg.model) #Summary of the model
avPlots(reg.model) #Creating the alternate variable plot
a <- data.frame(Fuel.Price = 6, Unemployment.rate = 8) # creating the data frame for the regression model
predict(reg.model, a) #Predicting the weekly sales

#Performing the hypothesis test for unemployment rate
store.sales.2010 <- subset(Walmart.sales, Walmart.sales$Year == 2010) #Extracting the 2010 subset
store.sales.2012 <- subset(Walmart.sales, Walmart.sales$Year == 2012)#Extracting the 2012 subset
UR.2010.sample <- sample(x = store.sales.2010$Unemployment.rate, size = 28, replace = F) #Collecting 28 random samples
UR.2012.sample <- sample(x = store.sales.2012$Unemployment.rate, size = 28, replace = F) #Collecting 28 random samples
boxplot(UR.2010.sample, horizontal = T, xlab = "Unemployment rate", main = "Unemployment rate in 2010",col = "green") #Plotting box plot
boxplot(UR.2012.sample, horizontal = T, xlab = "Unemployment rate", main = "Unemployment rate in 2012",col = "green") #Plotting box plot
qqnorm(UR.2010.sample, main = "Normal Q-Q Plot for Unemployment rate in 2010", col = "black", pch = 16) #Normal Q-Q Plot
qqline(UR.2010.sample, col="red")
qqnorm(UR.2012.sample, main = "Normal Q-Q Plot for Unemployment rate in 2012", col = "black", pch = 16) #Normal Q-Q Plot
qqline(UR.2012.sample, col="red")
var.test(UR.2010.sample, UR.2012.sample) #F-test
t.test(UR.2010.sample, UR.2012.sample, #t-test with 0.05 significance level
       alternative = "less",
       paired = T,
       var.equal = T,
       conf.level = 0.95)

