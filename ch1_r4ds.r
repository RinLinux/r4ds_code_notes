
library(tidyverse)

ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy), color="#00abff") + theme_bw()
ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy,colour=class)) + theme_bw()
ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy,alpha=class))
ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy,shape=class))

# 分面
ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy)) + facet_wrap(~ drv,nrow = 2)
ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy)) + facet_grid(drv~.)

df <- data.frame(
  x = rnorm(120,c(0,2,4)),
  y = rnorm(120,c(1,2,1)),
  z = letters[1:3]
)

ggplot(data = df) + geom_point(aes(x,y,color=z))

ggplot(data = df) + geom_point(aes(x,y)) + facet_wrap(~z)

df_sum <- df %>% 
  group_by(z) %>% 
  summarise(x=mean(x),y=mean(y)) %>% 
  rename(z2=z)
ggplot(df,aes(x,y)) + 
  geom_point(aes(color=z)) + 
  geom_point(data=df_sum,aes(color=z2,size=4)) + 
  facet_wrap(~z)

# 将数据放在同一个背景板中
df2 <- select(df,-z)
ggplot(df,aes(x,y)) + 
  geom_point(data=df2,color='grey70') + 
  geom_point(aes(color=z)) + 
  facet_wrap(~z)


# 几何对象

ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth(mapping = aes(x=displ,y=hwy)) + 
  geom_smooth(mapping = aes(x=displ,y=hwy,linetype=drv,color=drv),
              method = 'loess')

# 将一组变量传递给ggplot函数，ggplot将这些映射作为全局映射应用到图中的每个几何对象中
# 如果将映射放在几何对象函数中，ggplot2会将其视作这个图层的局部映射
# 以下的写法跟上面的结果一样
ggplot(data=mpg,mapping=aes(x=displ,y=hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth() + 
  geom_smooth(mapping = aes(linetype=drv,color=drv),method = 'loess')


ggplot(data=mpg,mapping = aes(x=displ,y=hwy)) +
  geom_smooth(mapping = aes(color=drv),
              show.legend = T)


ggplot(data=mpg,mapping = aes(x=displ,y=hwy)) + 
  geom_point(aes(color=class)) + 
  geom_smooth(data=filter(mpg,class=="subcompact"),se=F)

# p17,(6)
# a:
ggplot(data=mpg,aes(x=displ,y=hwy)) + 
  geom_point() + 
  geom_smooth(se=F)

# b: 
ggplot(data=mpg,aes(x=displ,y=hwy)) + 
  geom_point() + 
  geom_smooth(mapping=aes(group=drv),se=F)

# c:
ggplot(data=mpg,aes(x=displ,y=hwy,color=drv)) + 
  geom_point() + 
  geom_smooth(mapping=aes(color=drv),se=F)

# d:
ggplot(data=mpg,aes(x=displ,y=hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth(se=F)

# e:
ggplot(data=mpg,aes(x=displ,y=hwy)) + 
  geom_point(aes(color=drv)) + 
  geom_smooth(mapping=aes(linetype=drv),se=F)

# f:
ggplot(data=mpg,aes(x=displ,y=hwy,color=drv)) + 
  geom_point()

# 统计变换
ggplot(data=diamonds) + 
  geom_bar(aes(x=cut,fill=cut),show.legend = F)

ggplot(data=diamonds) + 
  stat_count(aes(x=cut,fill=cut,alpha=cut),show.legend = T)


demo <- tribble(
  ~a,~b,
  "bar_1",20,
  "bar_2",30,
  "bar_3",40
)

ggplot(data=demo) + 
  geom_bar(aes(x=a,y=b),stat="identity")


# 图像排版使用patchwork，
# 教程参考链接：https://cloud.tencent.com/developer/article/1889233
library(patchwork)

p <- ggplot(data=diamonds) + 
  geom_bar(mapping = aes(x=cut,fill=clarity),position = 'fill') + 
  theme_bw()

library(scales)
cols = hue_pal()(8)
p1 <- ggplot(data=diamonds) + 
  geom_bar(mapping = aes(x=cut,fill=clarity),position = 'fill') + 
  scale_fill_manual(values = cols) +
  theme_bw()

p + p1


p3 <- ggplot(data=diamonds,mapping = aes(x=carat,y=price)) +
  scale_color_manual(values=hue_pal()(7)) + 
  geom_point(aes(color=color)) + 
  geom_smooth(aes(group=color,color=color),se=F) + 
  theme_bw()

design <- "
  1122
  3333
  3333
"
p_out <- p + p1 + p3 + 
  plot_layout(design = design) +
  plot_annotation(tag_levels="A")

# 图片保存使用ggsave  
ggsave(filename = "out.png",plot=p_out,width = 25,height = 16,
       units = 'cm',dpi = 600)

SYMBOL <- c("EEF1A1","EEF1A1","HSP")
FC <- c(10.266615,9.228078,8.26)
P.value <- c(6.42e-07,4.31e-07,4.12e-07)
rwl <- data.frame()

library(tidyverse)
qz <- read.csv("qz.csv")

uniq_qz <- qz %>% group_by(Symbol) %>% 
  summarise(logFC_=median(logFC),p_=median(p))

qz <- read.csv("qz.csv")
attach(qz)
aggregate(qz[2:3],by=list(Symbol),FUN=median,na.rm=TRUE)

# 使用aRtsy包画艺术图
devtools::install_github("koenderks/aRtsy")
library(aRtsy)


# 用R这么画基因结构图
# https://zhuanlan.zhihu.com/p/83092046

# install.packages("gggenes")
library(gggenes)
head(example_genes)

ggplot(example_genes, aes(xmin = start, xmax = end, 
                          y = molecule, fill = gene)) +
  geom_gene_arrow() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()



ggplot(example_genes, aes(xmin = start, xmax = end, 
                          y = molecule, fill = gene, label = gene, forward = orientation)) +
  geom_gene_arrow() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes() +
  geom_gene_label(align = "left")

# 展示展现基因结构域特征或者比对信息
# 需要另外的信息example_subgenes
head(example_subgenes)
ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule)) +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  geom_gene_arrow(fill = "white") +
  geom_subgene_arrow(data = example_subgenes,
                     aes(xmin = start, xmax = end, y = molecule, fill = gene,
                         xsubmin = from, xsubmax = to), color="black", alpha=.7) +
  theme_genes()

# geom_bar默认位置调整是geom_stack

# 位置调整：fill
cols = hue_pal()(8)
p_fill <- ggplot(data=diamonds) + 
  geom_bar(mapping = aes(x=cut,fill=clarity),position = 'fill') + 
  scale_fill_manual(values = cols) +
  theme_bw()

# 位置调整：dodge
library(scales)
library(patchwork)
cols = hue_pal()(8)
p_dodge <- ggplot(data=diamonds) + 
  geom_bar(mapping = aes(x=cut,fill=clarity),position = 'dodge') + 
  scale_fill_manual(values = cols) +
  theme_bw()

p_fill + p_dodge

# 位置调整：jitter
# 增加随机扰动，将重叠的点分散开来
ggplot(data=mpg) + 
  geom_point(mapping = aes(x=displ,y=hwy),
             position = 'jitter')

ggplot(data=mpg) + 
  geom_point(mapping = aes(x=displ,y=hwy),position = 'count')


# 坐标系

## coord_flip()交换x轴和y轴

p <- ggplot(data = mpg,mapping = aes(x=class,y=hwy)) + 
  geom_boxplot()

p + coord_flip()

## coord_polar()使用极坐标系

bar <- ggplot(data=diamonds,mapping=aes(x=cut,fill=cut)) + 
  geom_bar(show.legend = F,width = 1) + 
  theme(aspect.ratio = 1) +
  labs(x=NULL,y=NULL)

bar + coord_flip()
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
bar + coord_polar() + blank_theme + scale_fill_manual(values=hue_pal()(5))
# 使用coord_polar将堆叠式条形图转换成饼图
ggplot(data=diamonds,mapping = aes(x='cut',fill=cut)) + 
  geom_bar(stat="count",width = 1,position = 'stack') +
  coord_polar("y", start=0) + 
  blank_theme + 
  scale_fill_manual(values=hue_pal()(5)) +
  geom_text(stat="count",aes(label = scales::percent(..count../50)), size=3, position=position_stack(vjust = 0.5))
