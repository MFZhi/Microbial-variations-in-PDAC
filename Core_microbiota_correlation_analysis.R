library(dplyr)
library(xlsx)
library(reshape2)
library(ggplot2)
set.seed(123)
species <-read.csv('species_relative.csv',head=T,stringsAsFactors=F,row.names=1)
Group <- read.xlsx("group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$group=='Control')#'Case'
group_1 <- Group%>%filter(Group$site=='duodenum')
group_2 <- Group%>%filter(Group$site=='oral')
group_1$id <- as.factor(group_1$id)
group_2$id <- as.factor(group_2$id)
rownames(group_1) <- group_1$id
rownames(group_2) <- group_2$id
same <- intersect(group_1$id,group_2$id)
group_1 <- group_1[same,]
group_2 <- group_2[same,]
core_oral_Control <- read.csv('core_oral_control.csv',head=T,row.names=1)
core_duodenum_Control <- read.csv('core_duodenum_control.csv',head=T,row.names=1)
core <- intersect(rownames(core_oral_Control),rownames(core_duodenum_Control))

#------------------------------------------------------
#Spearman
#------------------------------------------------------
data_1 <- species[core,group_1$sample]
data_2 <- species[core,group_2$sample]
corvalue <- data.frame()
pvalue <- data.frame()
rowname_1 = rownames(data_1)
rowname_2 = rownames(data_2)
for(i in (1:nrow(data_1)))
{
  for(j in (1:nrow(data_2)))
  {
    a=data_1[i,]
    b=data_2[j,]
    a=as.numeric(a)
    b=as.numeric(b)
    cc=cor.test(a,b,alternative = "two.sided",method="spearman",conf.level = 0.95)
    corvalue[i,j]= cc$estimate
    pvalue[i,j]= cc$p.value
  }
}
rownames(corvalue) <- rownames(data_1)
colnames(corvalue) <- rownames(data_2)
rownames(pvalue) <- rownames(data_1)
colnames(pvalue) <- rownames(data_2)
write.csv(corvalue,"corvalue.csv",row.names = T)
write.csv(pvalue,"pvalue.csv",row.names = T)

#------------------------------------------------------
#Heatmap
#------------------------------------------------------
library(ComplexHeatmap)
library(circlize)
pmt <- pvalue
pmt <- ifelse(test = (pmt< 0.0001), yes = "****", 
              no = ifelse(test = (pmt>0.0001 & pmt <0.001), yes = "***", 
                          no = ifelse(test = (pmt>0.001 & pmt <0.01), yes = "**", 
                                      no = ifelse(test = (pmt >0.01 & pmt <0.05), yes = "*", no = ""))))
annotation_col = data.frame(rownames(data_2))
annotation_col$group <- 'Oral'
annotation_col = data.frame(annotation_col[,-1])
rownames(annotation_col) <- rownames(data_2)
colnames(annotation_col) <- 'Oral'

annotation_row = data.frame(rownames(data_1))
annotation_row$group <- 'duodenum'
annotation_row = data.frame(annotation_row[,-1])
rownames(annotation_row) <- rownames(data_1)
colnames(annotation_row) <- 'duodenum'


row_ha=rowAnnotation(Site =annotation_row$duodenum,
                        col=list(Site=c("duodenum"="#eed392")))
col_ha=columnAnnotation(Site =annotation_col$Oral,
                        col=list(Site=c("Oral"="#ff7f00")))

max(corvalue)
min(corvalue)
pdf(file = "p.pdf",width =12,height = 10)
p <- Heatmap(corvalue,cluster_rows = T, cluster_columns = T,border_gp = gpar(col = "#FFFFFF"),
             col = colorRamp2(c(-0.7, 0, 0.8), c("#3366CC","#FFFFFF", "#FF7F50")),
             cluster_column_slices = FALSE,
             row_km=4,column_km=4,
             left_annotation = row_ha,
             top_annotation = col_ha,
             cell_fun = function(j, i, x, y, width, height, fill) {
               grid.text(sprintf(pmt[i, j]), x, y, gp = gpar(fontsize = 10))
             })
p
dev.off()




