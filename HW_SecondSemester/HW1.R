library(ggplot2)

# 1.
## (1) R을 사용하여 Lot Size와 Income의 관계를 결과변수인 owner/nonowner에 의해 컬러-코드화 된 산점도를 그리시오. 형식이 잘 갖춰진 플롯(읽기 쉬운 라벨(label)과 범례(legend)를 포함)을 만드시오. 이 산점도를 보고 owner/nonowner의 분포를 나머지 두 변수와 함께 설명하시오.
RidingMowers.df <- read.csv("RidingMowers.csv") # dataframe
plot(RidingMowers.df$Income ~ RidingMowers.df$Lot_Size, xlab = "Lot Size", ylab = "Income", col = ifelse(RidingMowers.df$Ownership == "Owner", "blue", "red")) # 산점도 생성, Ownership에 따라 color 다르게
legend("topleft", legend = c("Owner", "Nonowner"), col = c("blue", "red"), pch = 1, cex = 0.6) # legend 추가
## answer : 전반적으로 Lot_Size(대지면적)와 Income(가계소득)이 높은 쪽에 Owner의 분포가 높고, Lot_Size와 Income이 낮은 쪽에 Nonowner의 분포가 높다.


# 2.
## (1) 매장(Store.Postcode)별 평균 소매가격(Retail.Price)을 보여주는 막대차트를 그리시오. 평균 소매가격이 가장 높은 매장은 어느 곳인가? 반대로 가장 낮은 평균 소매가격은 어떤 매장인가?
LaptopSales.df <- read.csv("LaptopSalesJanuary2008.csv") # dataframe
data.for.plot <- aggregate(LaptopSales.df$Retail.Price, by = list(LaptopSales.df$Store.Postcode), FUN = mean) # aggregate함수 이용해 data.for.plot 생성
names(data.for.plot) <- c("StorePostcode", "Avg_Of_RetailPrice") # 변수명 변경
ggplot(data.for.plot) + geom_bar(aes(x = StorePostcode, y = Avg_Of_RetailPrice), stat = "identity") + coord_cartesian(ylim = c(480,500)) # 막대차트 생성, 비교가 쉽도록 y축 범위를 480~500으로 지정함
## answer
## 평균 소매가격이 가장 높은 매장 : N17 6QA
## 평균 소매가격이 가장 낮은 매장 : W4 3PH

## (2) 매장별 소매가격을 더 잘 비교하려면 병렬 박스플롯을 그리시오. (1)에서 찾은 두 매장의 가격을 비교해 보시오. 두 매장의 가격분포가 어떤 차이점이 있는가? 어떤 설명이든 좋으니 본인이 발견한 차이점을 서술해보시오.
x = subset(LaptopSales.df, Store.Postcode %in% c("W4 3PH","N17 6QA")) # 두 매장 subset
ggplot(x) + geom_boxplot(aes(Store.Postcode, Retail.Price)) # 병렬 박스플롯 생성
## answer : N17 6QA의 1사분위~3사분위의 분포가 W4 3PH에 비해 높다. 또한, W4 3PH의 outlier가 N17 6QA에 비해 더 많은 것으로 보인다.