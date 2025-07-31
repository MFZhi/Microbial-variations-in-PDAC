pacman::p_load(randomForest,caret,pROC,xlsx,dplyr,psych)
set.seed(123)

oral <- read.xlsx("result_oral.xlsx",sheetIndex = 2,row.names = 1)
pancreas <- read.xlsx("result_pancreas.xlsx",sheetIndex = 2,row.names = 1)
duodenum <- read.xlsx("result_duodenum.xlsx",sheetIndex = 2,row.names = 1)
diff <- intersect(rownames(oral),rownames(pancreas))
diff <- intersect(diff,rownames(duodenum))
Group <- read.xlsx("group.xlsx",sheetIndex = 1)
group <- Group%>%filter(Group$site=='duodenum')#'pancreas','oral'
data<-read.csv('species_relative.csv',head=T,stringsAsFactors=F,row.names=1)
data <- data[diff,group$sample]
data <- data.frame(t(data))
data$Group <- group$group

cvlist <- createFolds(data$Group, k = 5, list = TRUE, returnTrain = FALSE)
k <- 5
prediction_all <- data.frame()
test_all <- data.frame()
for (i in 1:k){
  train <- data[-cvlist[[i]],]  
  test <- data[cvlist[[i]],]
  model <-randomForest(as.factor(Group)~.,data = train,ntree = 2000,mtry=3,important=TRUE,proximity=TRUE)
  prediction <- predict(model,newdata = test,type ="prob")
  prediction_all <- rbind(prediction_all,prediction)
  test_all <- rbind(test_all,test)
}
write.csv(prediction_all,"prediction_all.csv",row.names = T)
write.csv(test_all,"test_all.csv",row.names = T)

roc<-roc (test_all$Group ,prediction_all$Control)
a=plot(roc , print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE)

