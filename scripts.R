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
    title = "barley yiled",
    x = "variety",
    y = "yield [bu/ac]"
  )+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.caption = element_text(size = 8)
  )+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


ggplot(barley, aes(x= site, y=yield))+
  theme_bw()+
  geom_boxplot()+
  geom_jitter(aes(colour=year),width = 0.2)+
  scale_colour_manual(values = c("#D59B0A", "#DECA98"))+
  labs(
    title = "barley yield",
    x = "variety",
    y = "yield [bu/ac]"
  )+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.caption = element_text(size = 8)
  )+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

##### linear model -----

yield_nullmodel <- lm(yield ~ 1, data = barley)
yield_nullmodel |> summary()

yield_altmodel <- lm(yield ~ site, data = barley)
yield_altmodel |> summary()

anova(yield_nullmodel)

anova(yield_altmodel)

pairwise.t.test(barley$yield, barley$variety, p.adj="holm") 
pairwise.t.test(barley$yield, barley$site, p.adj="holm") 



##### 交互作用 -----

barley_change <- barley |> 
  group_by(site, variety) |> 
  mutate(yield_mean = mean(yield))


ggplot(barley_change) +
  theme_bw()+
  geom_point(aes(x = variety, y = yield_mean, color = site), size = 2) +
  scale_color_manual(values = scico(6, begin = 0.1, palette = "hawaii"))+
  geom_line(aes(x = variety, y = yield_mean, color = site, group = site)) + 
  xlab("variety") + 
  ylab("yield mean")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))



