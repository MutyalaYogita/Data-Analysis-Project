# CSE4027 PROJECT - 18BCD7014,18BCD7074

# CAR DATA ANALYSIS

#Loading the dataset and required packages and libraries
library(ggplot2)
install.packages('cowplot')
library(cowplot)
setwd("C:/Users/MUTYALA YOGITA/Desktop/Car Price Prediction")
getwd()
data<-read.csv("CarPrice.csv")
# primary observations
head(data)
str(data)
dim(data)
summary(data)
# replacing the question mark (?) with NA
m<-data
is.na(data)
# unique vaules of all categorical columns
unique(data$symboling)
unique(data$make)
unique(data$fuel.type)
unique(data$aspiration)
unique(data$num.of.doors)
unique(data$body.style)
unique(data$drive.wheels)
unique(data$engine.location)
unique(data$engine.type)
unique(data$fuel.system)

# CLEANING AND PREPROCESSING OF DATA

#treating missing values
#normalized losses - converting from character type to numeric
data$normalized.losses<-as.numeric(data$normalized.losses)
#checking how many NA values there actually are
sum(is.na(data$normalized.losses))
#replacing the na values with the mean values of the respective columns
data$normalized.losses[is.na(data$normalized.losses)]= mean(data$normalized.losses,na.rm = TRUE)
#price
sum(is.na(data$price))
data$price<-as.numeric(data$price)
mean(data$price,na.rm=TRUE)
data$price[is.na(data$price)]<-mean(data$price,na.rm=TRUE)
#horsepower
sum(is.na(data$horsepower))
data$horsepower<-as.numeric(data$horsepower)
mean(data$horsepower,na.rm=TRUE)
data$horsepower[is.na(data$horsepower)]<-mean(data$horsepower,na.rm=TRUE)
#peak-rpm
sum(is.na(data$peak.rpm))
data$peak.rpm<-as.numeric(data$peak.rpm)
mean(data$peak.rpm,na.rm=TRUE)
data$peak.rpm[is.na(data$peak.rpm)]<-mean(data$peak.rpm,na.rm=TRUE)
#bore
sum(is.na(data$bore))
data$bore<-as.numeric(data$bore)
mean(data$bore,na.rm=TRUE)
data$bore[is.na(data$bore)]<-mean(data$bore,na.rm=TRUE)
#stroke
sum(is.na(data$stroke))
data$stroke<-as.numeric(data$stroke)
mean(data$stroke,na.rm=TRUE)
data$stroke[is.na(data$stroke)]<-mean(data$stroke,na.rm=TRUE)
# columns selected - "city.mpg","horsepower","engine.size","curb.w8","price" 
plot(data[c(24,22,17,14,26)])

# DATA WRANGLING

# 2D observations - library(ggplot2)
# univariate analysis
p0 = ggplot(data) + geom_histogram(aes(price), binwidth = 6000, fill = "blue")
p1 = ggplot(data) + geom_histogram(aes(engine.size), binwidth = 55, fill = "blue")
p2 = ggplot(data) + geom_histogram(aes(curb.w8), binwidth = 800, fill = "blue")
p3 = ggplot(data) + geom_histogram(aes(horsepower), binwidth = 60, fill = "blue")
p4<-ggplot(data) + geom_histogram(aes(peak.rpm), binwidth = 400, fill = "blue")
plot_grid(p0, p1, p2, p3, p4,nrow = 1)
unique(data$engine.type)
library(dplyr)
p5<-ggplot(data %>% group_by(engine.type) %>% summarise(Count = n())) +
  geom_bar(aes(engine.type, Count), stat = "identity", fill = "coral1") +
  xlab("Engine Type") +
  geom_label(aes(engine.type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Engine Type")
p6<-ggplot(data %>% group_by(num.of.doors) %>% summarise(Count = n())) +
  geom_bar(aes(num.of.doors, Count), stat = "identity", fill = "coral1") +
  xlab("Num of doors") +
  geom_label(aes(num.of.doors, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Num of doors")
p7<-ggplot(data %>% group_by(fuel.type) %>% summarise(Count = n())) +
  geom_bar(aes(fuel.type, Count), stat = "identity", fill = "coral1") +
  xlab("Fuel Type") +
  geom_label(aes(fuel.type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Fuel Type")
p8<-ggplot(data %>% group_by(body.style) %>% summarise(Count = n())) +
  geom_bar(aes(body.style, Count), stat = "identity", fill = "coral1") +
  xlab("Body Style") +
  geom_label(aes(body.style, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Body Style")
plot_grid(p5,p6,p7,p8, ncol = 1)
# correlation plotting
cor_plot = cor(data[c(3,4,5,6,7,8,9,15,18),])
library(corrplot)
corrplot(cor_plot, method = "pie", type = "lower", tl.cex = 0.9)

# EXPLORATORY DATA ANALYSIS

# LINEAR REGRESSION MODEL

# K-MEANS CLUSTERING
