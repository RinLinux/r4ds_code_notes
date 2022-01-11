
install.packages('nycflights13')
library(tidyverse)

library(nycflights13)
head(flights)


# filter：按值筛选观测
# select: 按名称选取变量
# arrange: 对行进行重新排序
# mutate: 使用现有变量创建新的变量
# summarize: 将多个值总结为一个摘要统计量

(jan1 <- filter(flights,month==1,day==1))

# 11月或12月出发的航班
filter(flights,month==11 | month==12)
filter(flights,month %in% c(11,12) )

df <- tibble(x=c(1,NA,3))


# P38

# (1)
## a.
filter(flights,arr_delay>=120)

## b.
filter(flights,dest %in% c('IAH','HOU'))

## c.
filter(flights,carrier %in% c('UA','US','DL') )

## d.
filter(flights,month %in% c(7,8,9) )

## e.
# filter(flights,arr_delay>=120 & )

## f.
# filter(flights,)

## g.
# filter(flights,)


# (2)
# between(x,left,right)函数表示x >= left & x <= right

# (3)
nrow(filter(flights,is.na(dep_time)))
# [1] 8255



arrange(flights,year,month,day)


# desc()降序排序

arrange(flights,desc(month),desc(day))

# P40

# (1)
df <- tibble(x=c(5,2,NA))
arrange(df,x)
arrange(df,desc(x))

arrange(df,desc(is.na(x)))

# (2)
# arrange(flights,desc())

# (3)

# (4)


select(flights,year,month,day)

select(flights,year:day)

# 去掉
select(flights,-(year:day))

rename(flights,tail_num=tailnum)

select(flights,time_hour,air_time,everything())



flights_sml <- select(flights,year:day,ends_with("delay"),distance,air_time)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

# 指标来新生成的变量
transmute(flights,
          gain= arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

summarize(flights,delay=mean(dep_delay,na.rm=TRUE))


by_day <- group_by(flights,year,month,day)
summarize(by_day,delay=mean(dep_delay,na.rm=TRUE))

by_dest <- group_by(flights,dest)
delay <- summarize(by_dest,
          count=n(),
          dist=mean(distance,na.rm=TRUE),
          delay=mean(arr_delay,na.rm=TRUE))

delay <- filter(delay,count>20,dest != "HNL")

ggplot(data=delay,mapping=aes(x=dest,y=delay)) + 
  geom_point(aes(size=count),alpha=1/3) + 
  geom_smooth()



delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count=n(),
    dist=mean(distance,na.rm=TRUE),
    delay=mean(arr_delay,na.rm=TRUE)
  ) %>% 
  filter(count>20,dest != "HNL")


flights %>% 
  group_by(year,month,day) %>% 
  summarise(mean=mean(dep_delay,na.rm=TRUE))

flights %>% 
  filter(!is.na(dep_delay)) %>% 
  group_by(year,month,day) %>% 
  summarise(mean=mean(dep_delay))

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>% 
  group_by(tailnum) %>% 
  summarise(delay=mean(arr_delay)) %>% 
  ggplot(.,mapping = aes(x=delay)) + 
  geom_freqpoly(binwidth=10)










