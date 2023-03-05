library("ggplot2")
install.packages('ggplot2')
#install.packages("mlbench")
#install.packages('caret')
library("mlbench")
library("caret")

df = read.csv('rul_hrs.csv')
qqnorm(df$sensor_04)
qqline(df$sensor_04)

View(df)
head(df)
summary(df)

plot(df$rul[141132:166441])
boxplot(df$sensor_12)

cor(df[,3:53])
#install.packages('corrplot')
library(corrplot)
corrplot(cor(df[,3:53]), method="number", is.corr=FALSE)

a = cov(df[,3:53])
plot(a)
df = df[,3:53]
df = as.matrix(scale(df))
eig = eigen(cov(df))
eig$values

sqrt(eig$values)
pc = princomp(df)
pc

