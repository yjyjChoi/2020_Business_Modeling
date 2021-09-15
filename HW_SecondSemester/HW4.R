# (1)
car.df <- read.csv("ToyotaCorolla.csv")
selected.var <- c(3,4,7,8,9,12,14,17,19,21,25,26,28,30,34,39) # 변수 벡터
set.seed(1)
train.index <- sample(rownames(car.df), dim(car.df)[1]*0.5)
train.df <- car.df[train.index, selected.var] # training data
set.seed(1)
valid.index <- sample(setdiff(rownames(car.df), train.index), dim(car.df)[1]*0.3)
valid.df <- car.df[valid.index, selected.var] # validation data 
set.seed(1)
test.index <- setdiff(rownames(car.df), union(train.index, valid.index))
test.df <- car.df[test.index, selected.var] # test data

## multiple linear regression 
car.lm <- lm(Price ~ ., data = train.df)
options(scipen = 999)
summary(car.lm) 


# (2)
## Exhaustive Search
library(leaps)
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = train.df))
train.df <- cbind(train.df[,-4], Fuel_Type[,]) # dummy varaibale
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive") 
sum <- summary(search) 
sum$which

## Forward Selection
train.df <- car.df[train.index, selected.var] # dummy variable없애기 위해 다시 실행

car.lm.null <- lm(Price ~ 1, data = train.df) # null모델 넣음
car.lm <- lm(Price ~ ., data = train.df)
car.lm.step <- step(car.lm.null, scope = list(lower = car.lm.null, upper = car.lm), direction = "forward")
summary(car.lm.step)

## backward elimination
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)

## stepwise
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)

# 답 : 위의 모든 Method들을 비교해봤을 때, Age_08_04, KM, HP가 가장 중요한 속성으로 보인다. # Automatic_airco도 중요하다고 보이기도 한다.


# (3)
library(forecast)
## validation data에 적용
car.lm.pred <- predict(car.lm, valid.df)
options(scipen = 999, digits = 3)
accuracy(car.lm.pred, valid.df$Price) # ME RMSE MAE MPE MAPE
# Mean Error : -17, root-mean-squared-error : 1187, Mean absolute error : 913, Mean percentage error : -1.14, Mean absolute percentage error : 9.48

## test data에 적용
car.lm.pred <- predict(car.lm, test.df)
options(scipen = 999, digits = 3)
accuracy(car.lm.pred, test.df$Price) # ME RMSE MAE MPE MAPE
# Mean Error : -1.41, root-mean-squared-error : 1192, Mean absolute error : 927, Mean percentage error : -0.632, Mean absolute percentage error : 9.13
