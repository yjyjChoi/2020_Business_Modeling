# 필요한 패키지
library(foreign) # spss
library(dplyr) # rename, mutate 등
library(ggplot2) # 그래프

df_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = TRUE)
welfare <- df_welfare # 복사본

# 코드네임 변수명 rename
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  income = p1002_8aq1,
                  donation = p1004_5,
                  house_price = h1006_6,
                  code_region = h10_reg7,
                  cost_of_living = h1007_9,
                  economic_activity = h10_eco4,
                  leisure_satisfaction = p1003_11,
                  )


# 1. 성별, 나이에 따른 평균 기부금액 추이
### 성별
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
### 나이
welfare$age <- 2015 - welfare$birth + 1
### 기부금액
summary(welfare$donation) # 최소 0원 최대 3600만원
### line graph
sex_age_donation <- welfare %>%
  filter(!is.na(donation)) %>%
  group_by(sex, age) %>%
  summarise(donation_mean = mean(donation))

ggplot(sex_age_donation, aes(x = age, y = donation_mean, col = sex)) + 
  geom_line() + 
  ylab("Average donation amount")


# 2. 소득정도, 성별 평균 기부금액
### 소득 카테고리
welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
boxplot(welfare$income)$stat
welfare$income_cat <- ifelse(welfare$income<123,"low", ifelse(welfare$income<606.5, "middle", "high"))
### bar chart
income_sex_donation <- welfare %>%
  filter(!is.na(donation)& !is.na(income)) %>%
  group_by(income_cat, sex) %>%
  summarise(donation_mean = mean(donation))

ggplot(income_sex_donation, aes(x = reorder(income_cat, -donation_mean), y = donation_mean, fill = sex)) +
  geom_col(position = "dodge") +
  xlab("Income_category") +
  ylab("Average donation amount")


# 3. 지역 별 평균 집 가격
### 7개 권역별 지역
region_list <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
welfare <- left_join(welfare, region_list, id = "code_region")
### bar chart
region_house_price <- welfare %>%
  filter(!is.na(house_price)) %>%
  group_by(region) %>%
  summarise(house_mean = mean(house_price))

ggplot(region_house_price, aes(x = reorder(region,house_mean), y = house_mean)) + 
  geom_col() + 
  coord_flip() +
  xlab("Region") +
  ylab("Average house price")


# 4. 소득정도에 따른 총생활비
### boxplot 
ggplot(income_cost, aes(x = as.factor(income_cat), y = cost_of_living), col = income_cat) + geom_boxplot() + ylim(0,3000) + xlab("Income category") + 
  ylab("Total cost of living") +
  scale_x_discrete(limits = c("low","middle","high"))


# 5. 경제활동 여부에 따른 여가생활 만족도
### 경제활동 여부 - 경제활동/비경제활동/실업자
welfare$eco_activity <- ifelse(welfare$economic_activity==8,"unemployment",ifelse(welfare$economic_activity==9,"inactive","active"))
table(welfare$eco_activity)
### 여가생활 만족도 - 만족/보통/불만족
welfare$lei_satistaction <- ifelse(welfare$leisure_satisfaction<3,"no",ifelse(welfare$leisure_satisfaction==3,"soso","yes"))
### 경제활동 여부와 여가생활 만족 비율
activity_satisfaction <- welfare %>%
  filter(!is.na(eco_activity) & !is.na(lei_satistaction)) %>%
  group_by(eco_activity,lei_satistaction) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  mutate(percent = round(count/total*100,1))
satisfaction <- activity_satisfaction %>%
  filter(lei_satistaction=="yes") %>%
  select(eco_activity, percent)
### bar chart
ggplot(satisfaction, aes(x = eco_activity, y = percent)) +
  geom_col() +
  xlab("Economic activity") +
  ylab("Leisure satisfaction")
