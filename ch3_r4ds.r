
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
