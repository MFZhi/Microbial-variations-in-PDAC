library(dplyr)
library(xlsx)
library(reshape2)
library(ggplot2)
library(ggpubr)

diversity <- read.csv("diversity.csv",row.names = 1, header = T)
Group <- read.xlsx("group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$site=='duodenum'|Group$site=='oral'|Group$site=='pancreas')
diversity <- diversity[Group$sample,]
diversity = merge(diversity,Group,by.x = 'row.names',by.y = 'sample')
#---------------------------Shannon---------------------
diversity$group <- factor(diversity$group, levels = c("Control", "Case"))
diversity$site <- factor(diversity$site, levels = c("pancreas", "duodenum","oral"))
my_comparisons <- list(c("Control","Case"))
a=ggboxplot(diversity, x="group", y="Shannon", angle_col = 45,color = "group", add = "jitter",add.params = list(size = 0.5),
            palette =c("Control"="#94d2e1","Case"="#ffcccc"))+
  theme_bw()+
  theme(axis.title.y= element_text(size = 14,color="black"),axis.title.x = element_blank(),axis.text.x=element_text(size = 11,color="black"),axis.text.y=element_text(size = 11,color="black"))+
  theme(panel.grid=element_blank())+
  stat_compare_means(comparisons = my_comparisons,label ="p.signif")
show(a)

library(ggh4x)
strip = strip_nested(background_x = elem_list_rect(fill = rep(c("pancreas"="#f59bbc","duodenum"="#eed392","oral"="#45bbeb")),color = rep('white',4)))
a <- a + facet_nested(~site ,scales = 'free_y',axes = 'all',
                      strip  = strip)
a <- a + labs(y='Shannon')
a
ggsave(a, file='Shannon.pdf', width=6, height=2.8)
ggsave(a, file='Shannon.png', width=6, height=2.8)

#---------------------------Chao1---------------------
a=ggboxplot(diversity, x="group", y="Chao1", angle_col = 45,color = "group", add = "jitter",add.params = list(size = 0.5),
            palette =c("Control"="#94d2e1","Case"="#ffcccc"))+
  theme_bw()+
  theme(axis.title.y= element_text(size = 14,color="black"),axis.title.x = element_blank(),axis.text.x=element_text(size = 11,color="black"),axis.text.y=element_text(size = 11,color="black"))+
  theme(panel.grid=element_blank())+
  stat_compare_means(comparisons = my_comparisons,label ="p.signif")
show(a)

library(ggh4x)
strip = strip_nested(background_x = elem_list_rect(fill = rep(c("pancreas"="#f59bbc","duodenum"="#eed392","oral"="#45bbeb")),color = rep('white',4)))
a <- a + facet_nested(~site ,scales = 'free_y',axes = 'all',
                      strip  = strip)
a <- a + labs(y='Chao1')
a
ggsave(a, file='Chao1.pdf', width=6, height=2.8)
ggsave(a, file='Chao1.png', width=6, height=2.8)
