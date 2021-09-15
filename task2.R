# 1.
## (1) 
# '예측'을 목적으로 하는 모델은 training data와 validation data로 나누는 것이 좋다. 만들어진 모델을 검증(평가)하기 위해 validation data가 필요하기 때문이다. training data는 모델을 만드는 데 사용된다. 그리고 validation data는 모델의 정확도를 테스트하는 데에 사용되고, 모델의 성능을 평가하기 위해서도 사용된다. 


## (2)
housing.df <- read.csv("BostonHousing.csv")
housing.df <- housing.df[,-14] # 전처리  
selected.var <- c(1,4,6,13) # CRIM, CHAS, RM, MEDV

# split data
set.seed(10)
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1]) # 60%
valid.index <- setdiff(row.names(housing.df), train.index) # 40%
train.df <- housing.df[train.index, selected.var]
valid.df <- housing.df[valid.index, selected.var]

# multiple linear regression model
housing.lm <- lm(MEDV ~ ., data = train.df) 
options(scipen = 999)
summary(housing.lm)

# equation ---> MEDV = -21.94971 + (-0.32682*CRIM) + (3.67117*CHAS) + (7.19976*RM)


## (3)
new.df <- data.frame(CRIM = 0.1, CHAS = 0, RM = 6)
housing.lm.pred <- predict(housing.lm, new.df)
housing.lm.pred # 결과값 : 21.21616 

# 계산방법 : -21.94971 + (-0.32682*0.1) + (3.67117*0) + (7.19976*6)


## (4)
### i.
round(cor(housing.df[,c(3,5,10)]),2)
# 수치적으로 보자면, 세 변수들 간 양적 상관관계가 꽤 높은 것으로 나타난다.
# 개인적인 생각으로는, 보스턴의 비상업지역의 비율이 높아서 이것이 높은 세금과 오염 수준으로 이어지는 것으로 보인다.

### ii.
round(cor(nhousing.df[,-13]),2)

# 높은 상관관계를 보이는 변수의 쌍 : INDUS-NOX(0.76), NOX-DIS(-0.77), AGE-DIS(-0.75), RAD-TAX(0.91)
# multicollinearity(다중공선성)이란, 결과변수와 동일한 선형관계를 공유하는 둘 이상의 예측변수가 존재하는 것. 즉, 예측변수들 사이에 상관관계가 높은 것을 의미한다. 다중공선성 예측결과에 부정적 영향을 미칠 수도 있기 때문에 INDUS, AGE, TAX를 제거하는 것이 바람직해 보인다.

### iii.
housing.df <- read.csv("BostonHousing.csv")
housing.df <- housing.df[,-14] # 전처리
# split data
set.seed(100)
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1]) # 60%
valid.index <- setdiff(row.names(housing.df), train.index) # 40%
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]

# 1) Exhaustive search
library(leaps)
search <- regsubsets(MEDV ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which
sum$adjr2
# CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, LSTAT으로 줄일 수 있음(10개)

# 2) forward selection 
library(forecast)
housing.lm <- lm(MEDV ~ ., data = train.df)
housing.lm.null <- lm(MEDV ~ 1, data = train.df)
housing.lm.forward <- step(housing.lm.null, scope = list(lower = housing.lm.null, upper = housing.lm), direction = "forward")
summary(housing.lm.forward)
# LSTAT + RM + PTRATIO + DIS + NOX + CHAS + CRIM + RAD + ZN + TAX로 줄일 수 있음(10개)

housing.lm.forward.pred <- predict(housing.lm.forward, valid.df) # 적용
accuracy(housing.lm.forward.pred, valid.df$MEDV) # 비교
# RMSE : 5.491387, MAPE : 19.30186, ME : 0.2922682

# 3) backward elimination
housing.lm.back <- step(housing.lm, direction = "backward")
summary(housing.lm.back)
# CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT으로 줄일 수 있음(10개)

housing.lm.back.pred <- predict(housing.lm.back, valid.df) # 적용
accuracy(housing.lm.back.pred, valid.df$MEDV) # 비교
# CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT으로 줄일 수 있음(10개)

# 4) stepwise 
housing.lm.step <- step(housing.lm, direction = "both") # +,-둘 다 고려
summary(housing.lm.step)
# CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT으로 줄일 수 있음(10개)

housing.lm.step.pred <- predict(housing.lm.step, valid.df) # 적용
accuracy(housing.lm.step.pred, valid.df$MEDV) # 비교
# RMSE : 5.491387, MAPE : 19.30186, ME : 0.2922682

# 따라서 INDUS, AGE를 제외한 "CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, LSTAT" 변수들로 이루어진 모델이 최고의 모델이다.


#--------------------------------------------------------------------

# 2.
## (1)
bank.df <- read.csv("UniversalBank.csv")
bank.df$Education <- as.factor(bank.df$Education) # factor형으로 변환
bank.df$CCAvg <- as.integer(bank.df$CCAvg) # integer형으로 변환
bank.df <- bank.df[,-c(1,5)] # ID, Zipcode 제거

# dummy variables 처리
Education <- as.data.frame(model.matrix(~ 0 + Education, data = bank.df))
bank.df <- cbind(bank.df[,-6], Education) # dummy varaibale

# data split
set.seed(1)
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1])
valid.index <- setdiff(row.names(bank.df), train.index) # 40프로의 숫자 들어감
train.df <- bank.df[train.index,]
valid.df <- bank.df[valid.index,]
new.df <- data.frame(Age = as.integer(40), Experience = as.integer(10), 
                     Income = as.integer(84), Family = as.integer(2), 
                     CCAvg = as.integer(2), Mortgage = as.integer(0), 
                     Securities.Account = as.integer(0), 
                     CD.Account = as.integer(0), Online = as.integer(1), 
                     CreditCard = as.integer(1),
                     Education1 = 0, Education2 = 1, Education3 = 0)

# 정규화
train.norm.df <- train.df
valid.norm.df <- valid.df
new.norm.df <- new.df

library(caret)
norm.values <- preProcess(train.df[,-7], method = c("center", "scale"))

train.norm.df[,-7] <- predict(norm.values, train.df[,-7])
valid.norm.df[,-7] <- predict(norm.values, valid.df[,-7])
new.norm.df <- predict(norm.values, new.df)

# KNN
library(FNN)
knn.new <- knn(train = train.norm.df[,-7], test = new.norm.df, cl = train.norm.df[,7], k = 1)
knn.new

# 결과 : 0(NO)가 나왔다. 따라서 이 고객은 개인 대출을 수락하지 않을 것으로 예측된다.


## (2)
accuracy.df <- data.frame(k = seq(1,14,1), accuracy = rep(0,14))

for(i in 1:14) {
  knn.pred <- knn(train.norm.df[,-7], valid.norm.df[,-7], cl = train.norm.df[,7], k = i)
  accuracy.df[i,2] <- confusionMatrix(as.factor(knn.pred), as.factor(valid.norm.df[,7]))$overall[1] 
}
accuracy.df

# best K = 3
# K값이 너무 작은 경우 local structure(지역적 특성)이 잘 반영될 수는 있다. 하지만 이웃이 noise인 경우 그 noise를 반영하게 되어버리는 위험성이 있다.
# 반대로 K값이 너무 크면 noise는 무뎌지겠지만, naïve rule처럼 모든 레코드를 그냥 다수결로 classify해버리게 될 수도 있다. 이웃들의 특성이 무시되어버린다.


## (3)
# library(caret)
knn.valid <- knn(train.norm.df[,-7], valid.norm.df[,-7], cl = train.norm.df[,7], k = 3)
confusionMatrix(as.factor(knn.valid), as.factor(valid.norm.df[,7]))

# <Confusion Matrix>
#            Reference
# Prediction    0    1
#          0 1787   64
#          1    8  141 


## (4)
knn.new <- knn(train = train.norm.df[,-7], test = new.norm.df, cl = train.norm.df[,7], k = 3)
knn.new

# 결과 : 0(NO). K값을 3으로 해도, 대출을 하지 않을 것으로 나타난다.
