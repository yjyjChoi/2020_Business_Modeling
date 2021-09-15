# (1) 미성년인구 백분율
library(dplyr) # 패키지 로드
midwest <- as.data.frame(ggplot2::midwest) # 데이터프레임
# midwest에 minor_total 변수 추가
midwest <- midwest %>% 
  mutate(minor_total = (poptotal-popadults)/poptotal*100)

# (2) 상위 5개 지역
midwest %>% 
  arrange(desc(minor_total)) %>% # 내림차순 정렬
  select(county, minor_total) %>% # 지역, 백분율 select
  head(5) # 5개

# (3) 미성년비율등급
# midwest에 level 변수 추가
midwest <- midwest %>%
  mutate(level = ifelse(minor_total>=40, "large", ifelse(minor_total>=30, "middle", "small"))) 
table(midwest$level) # 빈도표

# (4) 아시아인인구 백분율
midwest %>%
  mutate(asian_total = popasian/poptotal*100) %>%
  select(state, county, asian_total) %>% # 주, 지역, 백분율 select
  arrange(asian_total) %>% # 오름차순 정렬
  head(10) # 10개
