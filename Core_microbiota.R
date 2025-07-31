library(xlsx)
library(dplyr)
set.seed(123)

species<-read.csv('species_relative.csv',head=T,stringsAsFactors=F,row.names=1)
species <- species[-560,]#other
Group <- read.xlsx("group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$site=='duodenum')#'pancreas' 'oral'
group_1 <- Group%>%filter(Group$group=='Case')
group_2 <- Group%>%filter(Group$group=='Control')

species_1 <- species[,group_1$sample]
species_1_core <- data.frame(rowSums(species_1 != 0)/ncol(species_1))
species_1_core$abundance <- rowSums(species_1)/ncol(species_1)
colnames(species_1_core) <- c('occurrence','abundance')
core1 <- species_1_core%>%filter(species_1_core$occurrence>=0.8 & species_1_core$abundance >0.001)
write.csv(core1,"core_duodenum_case.csv",row.names = T)

species_2 <- species[,group_2$sample]
species_2_core <- data.frame(rowSums(species_2 != 0)/ncol(species_2))
species_2_core$abundance <- rowSums(species_2)/ncol(species_2)
colnames(species_2_core) <- c('occurrence','abundance')
core2 <- species_2_core%>%filter(species_2_core$occurrence>=0.8 & species_2_core$abundance >0.001)
write.csv(core2,"core_duodenum_control.csv",row.names = T)
