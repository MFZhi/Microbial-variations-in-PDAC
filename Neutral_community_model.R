#Reference：Correction to: Stochastic processes shape microeukaryotic community assembly in a subtropical river across wet and dry seasons
#File can be downloaded from the link:https://github.com/Weidong-Chen-Microbial-Ecology/Stochastic-assembly-of-river-microeukaryotes/blob/master/Neutral%20community%20model.r

library(Hmisc)
library(minpack.lm)
library(stats4)
library(xlsx)
library(dplyr)
set.seed(123)
spp<-read.csv('Species_count.csv',head=T,stringsAsFactors=F,row.names=1)
spp <- spp[-560,]
Group <- read.xlsx("group.xlsx",sheetIndex = 1)
Group <- Group%>%filter(Group$group=='Control')
group_1 <- Group%>%filter(Group$site=='duodenum')
group_2 <- Group%>%filter(Group$site=='oral')
spp1 <- spp[,group_1$sample]
spp2 <- spp[,group_2$sample]

n1 <- 0
spp1 <- spp1[rowSums(spp1!= 0) > n1,]
n2 <- 0
spp2 <- spp2[rowSums(spp2!= 0) > n2,]
temp <- intersect(rownames(spp1),rownames(spp2))
spp1 <- spp1[temp,]
spp2 <- spp2[temp,]
spp1<-t(spp1)
spp2<-t(spp2)

N <- mean(apply(spp2, 1, sum))
p.m <- apply(spp2, 2, mean)
p.m <- p.m[p.m != 0]
p <- p.m/N
spp.bi <- 1*(spp1>0)
freq <- apply(spp.bi, 2, mean)
freq <- freq[freq != 0]
C <- merge(p, freq, by=0)
C <- C[order(C[,2]),]
C <- as.data.frame(C)
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))),]
p <- C.0[,2]
freq <- C.0[,3]
names(p) <- C.0[,1]
names(freq) <- C.0[,1]
d = 1/N
m.fit <- nlsLM(freq ~ pbeta(d, N*m*p, N*m*(1 -p), lower.tail=FALSE),start=list(m=0.1))
m.fit  #m
m.ci <- confint(m.fit, 'm', level=0.95)
freq.pred <- pbeta(d, N*coef(m.fit)*p, N*coef(m.fit)*(1 -p), lower.tail=FALSE)
pred.ci <- binconf(freq.pred*nrow(spp), nrow(spp), alpha=0.05, method="wilson", return.df=TRUE)
Rsqr <- 1 - (sum((freq - freq.pred)^2))/(sum((freq - mean(freq))^2))
Rsqr  #R2

#plot
bacnlsALL <-data.frame(p,freq,freq.pred,pred.ci[,2:3])
bacnlsALL$flag <- 'Neutral'
for(i in 1:nrow(bacnlsALL))
{
  if(bacnlsALL[i,2] <= bacnlsALL[i,4]){bacnlsALL[i,6] <- 'Lower'}
  if(bacnlsALL[i,2] >= bacnlsALL[i,5]){bacnlsALL[i,6] <- 'Upper'}
}
write.csv(bacnlsALL, file = "bacnlsALL.csv")
inter.col<-rep('#5d5f60',nrow(bacnlsALL))
inter.col[bacnlsALL$freq <= bacnlsALL$Lower]<-'#e41a1c'
inter.col[bacnlsALL$freq >= bacnlsALL$Upper]<-'#377eb8'
library(grid)
grid.newpage()
pushViewport(viewport(h=0.6,w=0.6))
pushViewport(dataViewport(xData=range(log10(bacnlsALL$p)), yData=c(0,1.02),extension=c(0.02,0)))
grid.rect()
grid.points(log10(bacnlsALL$p), bacnlsALL$freq,pch=20,gp=gpar(col=inter.col,cex=0.7))
grid.yaxis()
grid.xaxis()
grid.lines(log10(bacnlsALL$p),bacnlsALL$freq.pred,gp=gpar(col='gray',lwd=2),default='native')

grid.lines(log10(bacnlsALL$p),bacnlsALL$Lower ,gp=gpar(col='gray',lwd=2,lty=2),default='native') 
grid.lines(log10(bacnlsALL$p),bacnlsALL$Upper,gp=gpar(col='gray',lwd=2,lty=2),default='native')  
grid.text(y=unit(0,'npc')-unit(2.5,'lines'),label='Mean Relative Abundance in Oral (log10)', gp=gpar(fontface=2)) 
grid.text(x=unit(0,'npc')-unit(3,'lines'),label='Frequency in Duodenum',gp=gpar(fontface=2),rot=90) 
draw.text <- function(just, i, j) {
  grid.text(paste("Rsqr=",round(Rsqr,3),"\n","m=",round(coef(m.fit),4)), x=x[j], y=y[i], just=just)
}
x <- unit(1:4/5, "npc")
y <- unit(1:4/5, "npc")
draw.text(c("centre", "bottom"), 4, 1)

