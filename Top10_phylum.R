library(dplyr)
library(xlsx)

Group <- read.xlsx("Group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$site=='duodenum')#'pancreas' 'oral'
Phylum <- read.csv("phylum_relative.csv",row.names = 1)
Phylum <- Phylum[-34,]
Phylum$sum <- rowSums(Phylum)
Phylum <- Phylum[order(Phylum$sum,decreasing = TRUE),]
Phylum <- Phylum[1:10,]
Phylum <- Phylum[,-ncol(Phylum)]

#------------------------------------------------------
#Control
#------------------------------------------------------
group_control <-Group%>%filter(Group$group1=='Control')
Phylum_control <- Phylum[,group_control$sample]
Phylum_control$sum <- rowSums(Phylum_control)
Phylum_control <- Phylum_control[order(Phylum_control$sum,decreasing = TRUE),]
Phylum_control<- Phylum_control[,-ncol(Phylum_control)]
Phylum_control <- data.frame(t(Phylum_control))
Phylum_control$Other <- (1-rowSums(Phylum_control))
Phylum_control <- data.frame(colMeans(Phylum_control))
colnames(Phylum_control) <- 'abundance'
Phylum_control$Phylum <- rownames(Phylum_control)

library(ggplot2)
Phylum_control$Phylum <- factor(Phylum_control$Phylum, levels=Phylum_control$Phylum)
p <- ggplot(Phylum_control, aes(x = '', y = abundance, fill = Phylum)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  scale_fill_manual(values = rev(c('Other'='#b4b4b4','Firmicutes'='#8DD3C7','Proteobacteria'= '#FFFFB3','Bacteroidetes'= '#BEBADA',
                                   'Actinobacteria'=  '#FB8072','Fusobacteria'= '#80B1D3','Candidatus'= '#FDB462','Spirochaetes'= '#FCCDE5',
                                   'Campilobacterota'= '#B3DE69','SR1'= '#BC80BD','Chloroflexi '='#a65628'))) + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  theme(legend.text = element_text(face = 'italic'), legend.title = element_blank()) +
  labs(x = '', y = '', title = '', fill = 'Phylum')
p
ggsave(p, file='duodenum_control.pdf', width=7, height=6)
ggsave(p, file='duodenum_control.png', width=7, height=6)

#------------------------------------------------------
#Case
#------------------------------------------------------
group_case <-Group%>%filter(Group$group1=='Case')
Phylum_case <- Phylum[,group_case$sample]
Phylum_case$sum <- rowSums(Phylum_case)
Phylum_case <- Phylum_case[order(Phylum_case$sum,decreasing = TRUE),]
Phylum_case<- Phylum_case[,-ncol(Phylum_case)]
Phylum_case <- data.frame(t(Phylum_case))
Phylum_case$Other <- (1-rowSums(Phylum_case))
Phylum_case <- data.frame(colMeans(Phylum_case))
colnames(Phylum_case) <- 'abundance'
Phylum_case$Phylum <- rownames(Phylum_case)

library(ggplot2)
Phylum_case$Phylum <- factor(Phylum_case$Phylum, levels=Phylum_case$Phylum)
p <- ggplot(Phylum_case, aes(x = '', y = abundance, fill = Phylum)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  scale_fill_manual(values = rev(c('Other'='#b4b4b4','Firmicutes'='#8DD3C7','Proteobacteria'= '#FFFFB3','Bacteroidetes'= '#BEBADA',
                                   'Actinobacteria'=  '#FB8072','Fusobacteria'= '#80B1D3','Candidatus'= '#FDB462','Spirochaetes'= '#FCCDE5',
                                   'Campilobacterota'= '#B3DE69','SR1'= '#BC80BD','Chloroflexi '='#a65628'))) + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) +
  theme(legend.text = element_text(face = 'italic'), legend.title = element_blank()) +
  labs(x = '', y = '', title = '', fill = 'Phylum')
p
ggsave(p, file='duodenum_case.pdf', width=7, height=6)
ggsave(p, file='duodenum_case.png', width=7, height=6)

