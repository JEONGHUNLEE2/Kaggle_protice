#Back to (predict) the future - Interactive M5 EDA
# 출처 : https://www.kaggle.com/headsortails/back-to-predict-the-future-interactive-m5-eda
#install.packages("patchwork")
#install.packages("vroom")
#install.packages("fuzzyjoin")
#install.packages("alluvial")
#install.packages("gganimate")
#install.packages("wesanderson")
#install.packages("kableExtra")
#install.packages("forecast")
#install.packages("timetk")
rm(list = ls())
# general visualisation
library(ggplot2) # visualisation
library(scales) # visualisation
library(patchwork) # visualisation
library(RColorBrewer) # visualisation
library(corrplot) # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('vroom') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('fuzzyjoin') # data wrangling

# specific visualisation
library('alluvial') # visualisation
library('ggrepel') # visualisation
library('ggforce') # visualisation
library('ggridges') # visualisation
library('gganimate') # animations
library('GGally') # visualisation
library('ggthemes') # visualisation
library('wesanderson') # visualisation
library('kableExtra') # display

# Date + forecast
library('lubridate') # date and time
library('forecast') # time series analysis
#library('prophet') # time series analysis
library('timetk') # time series analysis

#이항 신뢰구간 계산하는 함수
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))

#Data Load
#train <- vroom(str_c(path, 'sales_train_validation.csv'), delim = ",", col_types = cols())
train <- read.csv('sales_train_validation.csv') # -> 오래걸린다. 그래서 vroom을 사용하기도 한다. 
prices <- read.csv('sell_prices.csv')
calendar <- read.csv('calendar.csv')
sample_submit <- read.csv('sample_submission.csv')


# 데이터훑어보기 
train %>% select(seq(1, 10, 1)) %>% 
  head(10) %>% kable() %>% kable_styling() #view창에서 깔끔하게 볼 수 있는?

prices %>% head(10) %>% kable() %>% kable_styling()

calendar %>% head(8) %>% kable() %>% kable_styling()

sum(is.na(train))


bar <- train %>% 
  select(-contains("id")) %>% 
  na_if(0) %>% 
  is.na() %>% 
  as_tibble() %>% 
  mutate(sum = pmap_dbl(select(., everything()), sum)) %>% 
  mutate(mean = sum/(ncol(train) - 1)) %>% 
  select(sum, mean)

bar %>% 
  ggplot(aes(mean)) +
  geom_density(fill = "blue") +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, 1)) +
  theme_hc() +
  theme(axis.text.y = element_blank()) +
  labs(x = "", y = "", title = "Density for percentage of zero values - all time series")


# 시각적 개요 