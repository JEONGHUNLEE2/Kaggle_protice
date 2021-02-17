#Happiness Report - Exploratory Data Analysis
#출처 : https://www.kaggle.com/josignaciofernandez/happiness-report-exploratory-data-analysis/notebook?select=2015.csv

rm(list = ls())
save.image("Happiness Report - Exploratory Data Analysis.RData")

#What makes people happy?
#Our main goal is to do an descriptive analysis of the factors that make people happy.
#install.packages("factoextra")
#install.packages("heatmaply")
#install.packages("Hmisc")
#install.packages("GGally")

library(tidyverse)
library(ggplot2)
library(factoextra) #Clustering
library(FactoMineR) #Clustering
library(heatmaply) # Heatmap chart
library(cluster) #Clustering
library(Hmisc) #To impute values(값 대치)
library(corrplot)
library(GGally)
library(repr)

# plot 크기 설정 X- 안하는게 나을듯 
options(repr.plot.width = 10, repr.plot.height = 5)


#데이터 불러오기 
data_2019 <- read.csv("2019.csv") %>% as.data.frame()
data_2018 <- read.csv("2018.csv") %>% as.data.frame()

# 데이터 요약
head(data_2019, 3)
head(data_2018, 3)

print("2019 Data")
summary(data_2019)

print("2018 Data")
summary(data_2018)

str(data_2019)
str(data_2018)

#컬럼 형태 변경
data_2018$Perceptions.of.corruption <- as.numeric(data_2018$Perceptions.of.corruption)

#NA값에 중앙값 넣어주기
#data_2018$Perceptions.of.corruption <- with(data_2018, impute("Perceptions of corruption", median))
data_2018[is.na(data_2018$Perceptions.of.corruption)] <- median(data_2018$Perceptions.of.corruption)
median(data_2018$Perceptions.of.corruption)
data_2018[is.na(data_2018)]

#순위변수 삭제
data_2019$Overall.rank <- NULL
data_2018$Overall.rank <- NULL

# 행이름을 국가이름으로 변경 
row.names(data_2018) <- data_2018$Country.or.region
row.names(data_2019) <- data_2019$Country.or.region

#상관분석 실시 
cor_2018 <- cor(data_2018[,-1])
cor_2019 <- cor(data_2019[,-1])

corrplot(cor_2019, method = "circle", title = "Correlation in 2019 Data")
corrplot(cor_2019, method = "number", title = "Correlation in 2019 Data")

corrplot(cor_2018, method = "circle", title = "Correlation in 2018 Data")
corrplot(cor_2018, method = "number", title = "Correlation in 2018 Data")

#PCA & CLUSTER ANALYSIS
#1.PCA
fit <- PCA(data_2019[,-(1:2)], scale.unit = TRUE, ncp = 7, graph = FALSE)
eig<- get_eigenvalue(fit)
fviz_eig(fit, addlabels = TRUE)

#NEW PCA
fit2 <- PCA(data_2019[,-(1:2)], scale.unit = TRUE, ncp = 3, graph = FALSE)
var <- get_pca_var(fit2)
corrplot::corrplot(var$cor)

#Charts
fviz_pca_var(fit2, axes = c(1,2), col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_pca_var(fit2, axes = c(2,3) ,col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#군집분석
