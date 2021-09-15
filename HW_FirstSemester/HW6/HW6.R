# (1)
library(ggplot2)
library(dplyr)
midwest <- as.data.frame(ggplot2::midwest)
midwest
View(midwest)
dim(midwest) # 행과 열
head(midwest) # 위에서부터 6개
tail(midwest) # 아래에서부터 6개
str(midwest) # 구조
summary(midwest) # 최소값,최대값,1사분위값,3사분위값,중앙값,평균

# (2) 변수명 수정
midwest_test <- midwest
midwest_test
midwest_test <- rename(midwest_test, total = poptotal, asian = popasian) # rename 함수 이용
View(midwest_test)

# (3) 전체 인구 대비 아시아 인구 백분율 파생변수
midwest_test$asianPercent <- (midwest_test$asian/midwest_test$total)*100
hist(midwest_test$asianPercent)

# (4)
mean_asianPercent <- mean(midwest_test$asianPercent) # 아시아 인구 백분율 전체 평균
midwest_test$asianSize <- ifelse(midwest_test$asianPercent > mean_asianPercent, "large", "small")

# (5)
table(midwest_test$asianSize) # 빈도표
qplot(midwest_test$asianSize) # 빈도 막대그래프

