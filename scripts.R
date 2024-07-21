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
  group_by(site, year) |> 
  mutate(yield_mean_site_year = mean(yield))


ggplot(barley_change) +
  theme_bw()+
  geom_point(aes(x = site, y = yield_mean_site_year, color = year), size = 2) +
  scale_color_manual(values = scico(2, begin = 0.1, palette = "hawaii"))+
  #複数要素の折線はgroupで何の要素ごとで繋ぐのかを明示しなければいけない
  geom_line(aes(x = site, y = yield_mean_site_year, color = year, group = year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  xlab("site") + 
  ylab("yield mean by site in each year")+
  labs(
    title = "yield mean by site in each year",
    x = "site",
    y = "yield mean [bu/ac]"
  )+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.caption = element_text(size = 8)
  )
  
  
barley_change <- barley_change |> 
  group_by(variety, year) |> 
  mutate(yield_mean_variety_year = mean(yield))


ggplot(barley_change) +
  theme_bw()+
  geom_point(aes(x = variety, y = yield_mean_variety_year, color = year), size = 2) +
  scale_color_manual(values = scico(2, begin = 0.1, palette = "hawaii"))+
  #複数要素の折線はgroupで何の要素ごとで繋ぐのかを明示しなければいけない
  geom_line(aes(x = variety, y = yield_mean_variety_year, color = year, group = year)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(
    title = "yield mean by variety and each year",
    x = "variety",
    y = "yield mean [bu/ac]"
  )+
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        plot.caption = element_text(size = 8)
  )

sleepstudy %>%
  nest(data = -Subject) %>%
  mutate(fit = map(data, ~ lm(Reaction ~ Days, data = .)),
         results = map(fit, ~HSD.test(., trt = 'Days')))
