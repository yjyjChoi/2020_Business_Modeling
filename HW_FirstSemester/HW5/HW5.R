# 03/05/2020 제출
ship.df <- read.csv("ApplianceShipments.csv")
View(ship.df)

# (1)	시계열 차트
library(forecast)
shipment.ts <- ts(ship.df$Shipments, start = c(1985,1), end = c(1989,4), freq = 4)
shipment.ts
plot(shipment.ts, xlab = "Year", ylab = "Shipments")

# (2)	y축의 값을 3500~5000범위로 바꾸기
plot(shipment.ts, xlab = "Year", ylab = "Shipments", ylim = c(3500,5000))
