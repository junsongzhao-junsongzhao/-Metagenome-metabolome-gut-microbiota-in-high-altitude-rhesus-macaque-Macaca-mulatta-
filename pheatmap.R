
library(pheatmap)
rm(list = ls())
##Selected differential gene heatmap
all <- read.delim("clipboard",row.names = 1)
group <- read.delim("clipboard",row.names = 2,header = F)
data <- all[rownames(group),]
colnames(group) <- "module"
annotation_col <- data.frame(altitude=c(rep("high",20),rep("low",20)))
rownames(annotation_col) <- colnames(data)
data <- log(data+1)
pheatmap(data,fondsize=3,scale ="row",legend_breaks = c(-2,0,2),cluster_rows = F,cluster_cols = F,border_color = NA,
         color = colorRampPalette(colors = c("#32A907","white","red"))(100),
         angle_col="45",angle_row = "45",
         cellwidth = 5,cellheight = 9,
         show_colnames = F,
         annotation_col = annotation_col,
         annotation_row = group,
         gaps_col = 20,
         gaps_row = 19)






####Differential metabolite heatmap
data <- read.delim("clipboard",row.names = 2)
annotation_col <- data.frame(altitude=c(rep("high altitude",20),rep("low altitude",20)))
rownames(annotation_col) <- colnames(data)[-1]
annotation_row <- data[,1,drop = F]
data <- log(data[,-1]+1)
pheatmap(data,fontsize = 9,scale = "row",legend_breaks = c(-2,0,2),cluster_rows = F,cluster_cols = F,border_color = NA,
         color = colorRampPalette(colors = c("#32A907","white","red"))(100),
         cellwidth = 3.5,cellheight = 8,annotation_col = annotation_col,
         annotation_row = annotation_row,
         gaps_col = 20)

##Differential pathway heatmap
data <- read.delim("pathway_lefse1.txt",header = T,row.names = 1)
annotation_row <- data.frame(row.names = rownames(data),level2 <- data$V2,levell1 <- data$V1)
colnames(annotation_row) <- c("level 2","level 1")
annotation_col <- data.frame(row.names = colnames(data)[-c(1:2)],altitude <- c(rep("high",20),rep("low",20)))
colnames(annotation_col) <- "altitude"
pheatmap(log(data[-c(1:2)]+1),fontsize = 9,scale = "row",legend_breaks = c(-2,0,2),cluster_rows = F,cluster_cols = F,border_color = NA,
         color = colorRampPalette(colors = c("#32A907","white","red"))(100),
         cellwidth = 3.5,cellheight = 8,
         annotation_row = annotation_row,
         annotation_col = annotation_col,
         gaps_row = 16,gaps_col = 20,
         show_colnames = F)