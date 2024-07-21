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


##### モデル ------

model1 <- function(dat)
  lm(yield ~ variety + 1, data = dat)

##栽培地(と誤差頂+切片)だけを含んだモデル
model2 <- function(dat)
  lm(yield ~ site + 1, data = dat)

##年度(と誤差頂+切片)だけを含んだモデル
model3 <- function(dat)
  lm(yield ~ year + 1, data = dat)

#年度の変動が品種や栽培地の違いに影響すると考えられる
##栽培地と品種のランダム効果だけを導入したモデル
model4 <- function(dat)
  lmer(yield ~   1 + (1 | site) + (1|variety) , data = dat)

##栽培地と切片に、年度(環境変動)と品種のランダム効果を導入したモデル
model5 <- function(dat)
  lmer(yield ~  site + 1 + (site + 1 | year) + (1 |variety) , data = barley)

##品種には栽培地の効果がありそうだと考えられる
##品種に年度(環境変動)と栽培地のランダム効果を導入したモデル
model6 <- function(dat)
  lmer(yield ~ variety + 1 + (variety + 1 | year) + (variety + 1|site) , data = dat)

##栽培地と年度の相互作用のモデルに、品種のランダム効果を加えたもの
model7 <- function(dat)
  lmer(yield ~ site + year + site:year + (1|variety) , data = dat)

##品種と年度の相互作用のモデルに、品種のランダム効果を加えたもの
model8 <- #function(dat)
  lmer(yield ~ variety + year + variety:year + (variety + 1|site) , data = barley)

#既存のモデル
#nullモデル
yield_nullmodel <- function(dat)
  lm(yield ~ 1 + (1|site) + (1|variety) + (1|year), data = dat)

#栽培地と品種と栽培地と年度の相互作用を係数として織り込んだモデル
yield_altmodel <- function(dat)
  lm(yield ~ site + variety + 1 + site:year, data = dat)

#栽培地と品種と栽培地と品種の相互作用を係数として織り込んだモデル
variety_year_inter_model <- function(dat)
  lm(yield ~ site + variety + 1 + variety:year, data = dat)

#栽培地と年度の相互作用と品種と年度の相互作用を係数として織り込んだモデル
all_inter_model <- function(dat)
  lm(yield ~ site + variety + 1 + site:year + variety:year, data = dat)

pacman::p_lod(cAIC4)

listed_lmer <- 
  list( model4, model5, model6, model7, model8)

model_results <- barley %>% 
  nest %>% 
  bind_rows(., ., ., ., .) %>%          #データをモデル数分bind
  rownames_to_column("id_model") %>% 
  mutate(model = map2(data, listed_lmer, ~.y(.x))) %>%    # それぞれ当てはめ
  mutate(AIC = map(model, AIC), # AIC抽出
         fixef = map(model, fixef),                       # 固定効果
         ranef = map(model, ranef),                       # ランダム効果
         predict = map2(model, data, ~predict(.x, .y))) 


listed_lmer <- 
  list( yield_nullmodel, yield_altmodel, variety_year_inter_model, all_inter_model,
        model1, model2, model3, model4, model5, model6, model7, model8)

getAICloglik <- function(model) {
  print(AIC(model))
  print(logLik(model))
}

aics <- map_dbl(listed_lmer, getAICloglik)
