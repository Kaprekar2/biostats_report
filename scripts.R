##### ライブラリとデータのロード------
pacman::p_load(tidyverse, ggtext, lattice)

barley |> head()

##### 箱ひげ図 ------

#barley_change <- 
#  barley |> 
#  mutate(yearcolor = 
#           case_when(year == 1932 ~ "#D59B0A",
#                     year == 1931 ~ "#DECA98"))

#varietyのと収量の関係
ggplot(barley, aes(x= variety, y=yield))+
  theme_bw()+
  geom_boxplot()+
  #geom_dotplot(aes(fill=year), binaxis = "y", binwidth = 1,stackdir = "center")+
  #geom_point(aes(colour=year))+
  geom_jitter(aes(colour=year),width = 0.2)+
  scale_colour_manual(values = c("#D59B0A", "#DECA98"))+
  labs(
    title = "yield graph",
    x = "variety",
    y = "yield [bu/ac]"
  )+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.caption = element_text(size = 8)
  )+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


