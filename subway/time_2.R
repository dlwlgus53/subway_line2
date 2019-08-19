#install.packages("openxlsx")
library("openxlsx")
library(dplyr)

##### 지하철 하루 총 이용량(2018~2014 5년 데이터)  #####
# date, total_user, day
# 2018

subway_2018_raw <- read.xlsx("subway_2018.xlsx", sheet = 1,startRow = 2, colNames = TRUE)
str(subway_2018_raw)
subway_2018 = subway_2018_raw %>%
  filter(on_off == '승차' & line == '2호선') 
for(i in 6:25){
  subway_2018[,i] = as.integer(subway_2018[,i])
}
View(subway_2018)
subway_2018$date = convertToDate(subway_2018$date)
subway_2018$day = weekdays(as.Date(subway_2018$date))


subway_2018$holiday = ifelse(subway_2018$day %in% c('Saturday', 'Sunday') | subway_2018$date %in% as.Date.character(holiday_2018), 'T', 'F')

head(subway_2018)
row = nrow(subway_2018)

rush_user = c(1,2)
notrush_user= c(1,2)

class(subway_2018$sum)

for(i in 1:row){
  rush_user[i] = ifelse(subway_2018[i,28] == 'T',0, sum(subway_2018[i,c(7:10,18:21)]))
  notrush_user[i] = ifelse(subway_2018[i,28] == 'T',subway_2018[i,26], subway_2018[i,26]-rush_user[i])
}
subway_2018 = cbind(subway_2018, rush_user)
subway_2018 = cbind(subway_2018, notrush_user)

head(subway_2018)

subway_2018 = subway_2018 %>%
  group_by(date) %>%
  summarise(rush_user_tot= sum(rush_user), notrush_user_tot = sum(notrush_user))

head(subway_2018)
colnames(subway_2018) = c('date','rush_user', 'notrush_user')
View(subway_2018)
write.csv(subway_2018, file = 'subway_2018_rush.csv', row.names = F)

subway_2018_2 = subway_2018_raw
class(subway_2018_2$date)

subway_2018_2$date = as.character(subway_2018_2$date)
class(subway_2018_2$date)
head(subway_2018_2)
subway_2018_2$date = convertToDate(subway_2018_2$date)
subway_2018_2$day = weekdays(as.Date(subway_2018_2$date))
head(subway_2018_2)
subway_2018_2$holiday = ifelse(subway_2018_2$day %in% c('Saturday', 'Sunday') | subway_2018_2$date %in% as.Date.character(holiday_2018), 'T', 'F')

subway_2018_2 = subway_2018_2 %>%
  select(date,day,holiday)

subway_2018_2 = unique(subway_2018_2)

subway_2018 = left_join(subway_2018, subway_2018_2)
View(subway_2018)
str(subway_2018)
write.csv(subway_2018, file = 'subway_2018_rush.csv', row.names = F)



# 2017 중간에 날짜 데이터 형식 달라짐
# date, total_user, day
subway_2017_raw <- read.xlsx("subway_2017.xlsx", sheet = 1,startRow = 2, colNames = TRUE)
tail(subway_2017_raw)
subway_2017 = subway_2017_raw %>%
  filter(on_off == '승차' & line == '2') 


View(subway_2017)

# 2017 1월달~9월달
subway_2017_1to9 = subway_2017[0:13650,]
class(subway_2017_1to9$date)
subway_2017_1to9$date = as.Date.character(subway_2017_1to9$date)
View(subway_2017_1to9)
head(subway_2017_1to9)

subway_2017_1to9$day = weekdays(as.Date(subway_2017_1to9$date))
subway_2017_1to9$date = as.Date.character(subway_2017_1to9$date)

# 2017 10월달~12월달
row <- nrow(subway_2017)
subway_2017_10to12 = subway_2017[13651:row,]
head(subway_2017_10to12$date)

subway_2017_10to12$date = convertToDate(subway_2017_10to12$date)
subway_2017_10to12$day = weekdays(as.Date(subway_2017_10to12$date))

str(subway_2017_10to12)

# 두 데이터 합치기

subway_2017 = rbind(subway_2017_1to9, subway_2017_10to12)
head(subway_2017)
tail(subway_2017)
for(i in 6:25){
  subway_2017[,i] = as.integer(subway_2017[,i])
}





subway_2017$holiday = ifelse(subway_2017$day %in% c('Saturday', 'Sunday') | subway_2017$date %in% as.Date.character(holiday_2017), 'T', 'F')

head(subway_2017)
row = nrow(subway_2017)

rush_user = c(1,2)
notrush_user= c(1,2)

class(subway_2017$sum)
#6시부터 10시, 5시부터 9시
for(i in 1:row){
  rush_user[i] = ifelse(subway_2017[i,28] == 'T',0, sum(subway_2017[i,c(7:10,18:21)]))
  notrush_user[i] = ifelse(subway_2017[i,28] == 'T',subway_2017[i,26], subway_2017[i,26]-rush_user[i])
}
subway_2017 = cbind(subway_2017, rush_user)
subway_2017 = cbind(subway_2017, notrush_user)

head(subway_2017)

subway_2017 = subway_2017 %>%
  group_by(date) %>%
  summarise(rush_user_tot= sum(rush_user), notrush_user_tot = sum(notrush_user))

head(subway_2017)
colnames(subway_2017) = c('date','rush_user', 'notrush_user')


subway_2017

subway_2017$day = weekdays(as.Date(subway_2017$date))
subway_2017$holiday = ifelse(subway_2017$day %in% c('Saturday', 'Sunday') | subway_2017$date %in% as.Date.character(holiday_2017), 'T', 'F')

View(subway_2017)
str(subway_2017)
write.csv(subway_2017, file = 'subway_2017_rush.csv', row.names = F)



