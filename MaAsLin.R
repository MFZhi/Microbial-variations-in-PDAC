#BiocManager::install("Maaslin2")
library(Maaslin2)
library(xlsx)
library(dplyr)
set.seed(123)

Group <- read.xlsx("group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$site=='duodenum')#'pancreas' 'oral'
data<-read.csv('species_relative.csv',head=T,stringsAsFactors=F,row.names=1)
data <- data[-560,]
data <- data[,Group$sample]
rownames(Group) <- Group$sample

num <- nrow(Group)*0.2
data <- data[rowSums(data != 0) > num,]
data <- data.frame(t(data))

fit_data <- Maaslin2(
  data, Group,'output',
  fixed_effects = c('group','age','gender'),
  normalization = 'NONE',
  reference = c('group,Control'),
  standardize = FALSE)

result <- read.delim('./output/all_results.tsv',header=T,sep='\t')
result$padjust <- p.adjust(result$pval,method="fdr",n=length(result$pval))
result_padjust <- result%>%filter(result$padjust <=0.05)
result_padjust <- result_padjust%>%filter(result_padjust$value == 'Case')
write.xlsx(result, file = "result_duodenum.xlsx", row.names = F, sheetName = "Sheet1")
write.xlsx(result_padjust, file = "result_duodenum.xlsx", row.names = F, sheetName = "result_padjust0.05",append=TRUE)

#------------------------------------------------------
#Heatmap
#------------------------------------------------------
library(ComplexHeatmap)
library(circlize)

oral <- read.xlsx("result_oral.xlsx",sheetIndex = 2,row.names = 1)
duodenum <- read.xlsx("result_duodenum.xlsx",sheetIndex = 2,row.names = 1)
pancreas <- read.xlsx("result_pancreas.xlsx",sheetIndex = 2,row.names = 1)

diff <- intersect(rownames(oral),rownames(pancreas))
diff <- intersect(diff,rownames(duodenum))
Group <- read.xlsx("group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$site=='duodenum'|Group$site=='oral'|Group$site=='pancreas')
data<-read.csv('species_relative.csv',head=T,stringsAsFactors=F,row.names=1)
data <- data[diff,Group$sample]
plotdata=log10(data + min(data[data!=0]))
data_scaled =  t(apply(plotdata, 1, scale))
colnames(data_scaled) <- colnames(plotdata)

annotation_col <- Group[,c(3,8)]
rownames(annotation_col) <- Group$sample

annotation_col$group <- factor(annotation_col$group, levels = c("Control", "Case"))
annotation_col$site <- factor(annotation_col$site, levels = c("pancreas", "duodenum","oral"))
col_ha=columnAnnotation(Group =annotation_col$group,Site = annotation_col$site,
                        col=list(Group=c("Control"="#377eb8","Case"="#e41a1c"),
                                 Site=c("duodenum"="#eed392","pancreas"="#4daf4a","oral"="#ff7f00")))
max(data_scaled)
min(data_scaled)
p <- Heatmap(data_scaled,cluster_rows = T, cluster_columns = T,border_gp = gpar(col = "black"),
             col = colorRamp2(c(-2, 0, 2), c("#2166ac","#FFFFFF", "#e41a1c")),
             column_split = data.frame(annotation_col$site,annotation_col$group),
             cluster_column_slices = FALSE,
             row_km=4,show_column_names = FALSE,
             top_annotation=col_ha)
p
