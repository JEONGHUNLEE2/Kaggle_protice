# Kagglestruggle 
# Exploring Survival on the Titanic
# 출처 : https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic

save.image("Exploring Survival on the Titanic.RData")

#1. Exploring the Titanic Dataset
#1-1 Load and check data
#install.packages("mice")
library(ggplot2) #시각화
library(ggthemes) # 시각화
library(scales) #시각화
library(dplyr) #데이터 조작
library(mice) #돌리기? imputation?
library(randomForest) #분류 알고리즘 

train <- read.csv("train.csv", stringsAsFactors =  F)
test <- read.csv("test.csv", stringsAsFactors = F)

full <- bind_rows(train, test)

#2.Feature Enginnering
#2-1 어떤 이름들이 있는지

# 이름 '성' 가져오기 -> 가족을 나타내기 위해
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# 성별, '성' count
table(full$Sex, full$Title)

# 매우 적은 title 분류
rare_title <- c('Dona','Lady','the Countess', 'Capt','Col',
                'Don','Dr','Major','Rev','Sir','Jonkheer')

# 
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

table(full$Sex, full$Title)

#마지막으로 승객 이름에서 성을 가져옵니다.
full$Surname <- sapply(full$Name, 
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

#....뭐지
cat(paste('We have <b>', nlevels(factor(full$Surname)), 
          '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))

unique(full$Surname)

#2-2 가족끼리 침몰했는지 살았는지?

#가족 크기 변수 만들기
full$Fsize <- full$SibSp + full$Parch + 1

full$Family <- paste(full$Surname, full$Fsize, sep = '_')

#가족수와 생존 관계파악
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived)))+
  geom_bar(stat = 'count', position = 'dodge')+
  scale_x_continuous(breaks = c(1:11))+
  labs(x = 'Family Size')+
  theme_few()
# - > 4인 이상 가족이 있는 경우 사망 경우가 높았다. 

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

mosaicplot(table(full$FsizeD, full$Survived), main = 'Family Size by Survival', shade = TRUE)
#large, 즉 대가족일수록 생존률이 적었다. 

#2-3 추가적으로 변수 처리
full$Cabin[1:28]
strsplit(full$Cabin[2],NULL)[[1]]

full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
                        

#3. 누락값
full[c(62,830), 'Embarked']
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))
head(full)

# ID가 없는 데이터 삭제
embark_fare <- full %>% filter(PassengerId !=62 & PassengerId != 830)

#중간 요금 시각화 -> 누락된 부분을 다른 컬럼 평균이랑 비교해서 비슷한 평균을 가지고 있는 그룹의 값으로 대체..오호...
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass)))+
  geom_boxplot()+
  geom_hline(aes(yintercept = 80),
             colour = 'red', linetype = 'dashed', lwd = 2)+
  scale_y_continuous(labels = dollar_format())+
  theme_few()

# 누락값을 C로 입력
#62행과 830행이 출발지점이 누락되어있었는데 요금평균으로 비교해서 출발지점 입력..오...
full$Embarked[c(62,830)] <- 'C'

#요금 NA값
full[1044,]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S',],
       aes(x = Fare))+
  geom_density(fill = '#99d6ff', alpha = 0.4)+
  geom_vline(aes(xintercept = median(Fare, na.rm = T)),
             colour = 'red', linetype = 'dashed',lwd = 1)+
  scale_x_continuous(labels = dollar_format())+
  theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S',]$Fare, na.rm = TRUE)

# 3.2 예측 대치 
# 다른 변수를 기반으로 연령을 예측하는 모델생성 ...오...
sum(is.na(full$Age))

factor_vars <- c('PassengerId', 'Pclass', 'Sex', 'Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
str(full)

set.seed(129)

mice_mod <- mice(full[, !names(full)%in% c('PassengerId','Name','Ticket',
                                           'Cabin','Family','Surname','Survived')], method = 'rf')

mice_output <- complete(mice_mod)

#기존 연령대 분포와 모델을 통해 나온 분포 비교
par(mfrow = c(1,2))
hist(full$Age, freq = F, main = 'Age : Original Data',
     col = 'darkgreen', ylim =c(0,0.04))
hist(mice_output$Age, freq = F, main = 'Age : Mice Output',
     col= 'lightgreen', ylim = c(0,0.04))

#분포가 비슷함으로 모델을 통해서 만든 예측값을 원래 데이터에 입력
full$Age <- mice_output$Age
sum(is.na(full$Age))

#3-3 Feature Engineering_ Round2
