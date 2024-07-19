##### ライブラリとデータのロード------
pacman::p_load(tidyverse, ggtext, lattice)

barley |> head()

##### 箱ひげ図 ------

barley_change <- 
  barley |> 
  mutate(yearcolor = 
           case_when(year == 1932 ~ "#2D579A",
                     year == 1931 ~ "#BEC7D7"))


ggplot(barley_change, aes(x= variety, y=yield))+
  theme_bw()+
  geom_boxplot()+
  geom_dotplot(aes(fill=yearcolor), binaxis = "y", binwidth = 1,stackdir = "center")+
  scale_fill_manual(values = c("#2D579A", "#BEC7D7"), guide = "none")+
  xlab("yield [bu/ac]") + 
  ylab("<span style='font-size:16pt'>variety</span>") +
  ggtitle("<span style='font-size:20pt'>**yield graph**</span><br>") + 
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)
  )

