library(dplyr)
library(ggplot2)
gender <- read.csv("Data/gender.csv",header = T,stringsAsFactors = F)
profile <- read.csv("Data/profile.csv",header = T,stringsAsFactors = F)
fertility <- left_join(gender,profile,by=c("id"="id"))
ord_fertility <- fertility %>% arrange(id)
ord_fertility %>% group_by(gender1,gender2)%>%summarise(n())
ord_fertility %>% group_by(afam,hispanic,other) %>% summarise(n(),sum(work<=4),sum(work<=4)/n())
ord_fertility %>% filter(age>=22&age<=24) %>% summarise(sum(gender1=="male")/n())

fertility_long <- read.csv("Data/fertility_long.csv",header = T,stringsAsFactors = F)
fertility_long$age <- as.numeric(fertility_long$age)
fertility_long%>% filter(race_code!=0) %>% ggplot() + geom_histogram(aes(x=age))

race_rate <- fertility_long %>% group_by(race_code) %>% summarise(rate=sum(gender=="male")/sum(gender=="female"))
race_rate$race_code <- as.character(race_rate$race_code)
race_rate %>% ggplot() + geom_bar(aes(x=race_code,weight=rate))

