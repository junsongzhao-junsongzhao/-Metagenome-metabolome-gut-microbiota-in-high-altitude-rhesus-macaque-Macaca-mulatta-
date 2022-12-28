##Microbial tax α Diversity and β Diversity
rm(list = ls())
library(reshape2)
library(ggpubr)

data <- read.delim("npj文章重新作图/α多样性/组装/Alpha diversity analysis_tax水平.csv",sep = ",",row.names = 1,check.names = F,stringsAsFactors = F)

data$altitude[grep("LA",rownames(data))] <- "low altitude"
data$altitude[grep("LB",rownames(data))] <- "low altitude"
data$altitude[grep("HA",rownames(data))] <- "high altitude"
data$altitude[grep("HB",rownames(data))] <- "high altitude"

colnames(data)[2:3] <- c("Shannon","Simpson")

plot_data <- melt(data[,c(2,3,5)])
compaired <- list(c("low altitude","high altitude"))
p1 <- ggplot(plot_data,aes(altitude,value))+
  geom_boxplot(aes(fill=altitude),size=.7) + geom_point()+
  facet_wrap(.~variable,scales = "free",strip.position = "top")+
  scale_fill_manual(values = c("#F07474","#32A907"))+
  stat_compare_means(method="t.test",comparisons = compaired,bracket.size = .7)+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.title = element_blank(),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.key = element_blank(),
        strip.background = element_rect(color = "black",size = 1),
        strip.text = element_text(size = 15),
        strip.placement = "inside",
        aspect.ratio = 1.5)+
  guides(fill="none")+ggtitle("based-assembly")

#ggsave("npj文章重新作图/plots/a多样性-assembly.pdf",width = 8,height = 6)

rm(list = ls())
data <- read.delim("npj文章重新作图/α多样性/未组装/Alpha diversity analysis.csv",sep = ",",row.names = 1,check.names = F,stringsAsFactors = F)
data$altitude[grep("F",rownames(data))] <- "low altitude"
data$altitude[grep("S",rownames(data))] <- "low altitude"
data$altitude[grep("P",rownames(data))] <- "high altitude"
data$altitude[grep("Y",rownames(data))] <- "high altitude"
colnames(data)[4:5] <- c("Shannon","Simpson")
plot_data <- melt(data[,c(4,5,7)])
compaired <- list(c("low altitude","high altitude"))

p2 <- ggplot(plot_data,aes(altitude,value))+
  geom_boxplot(aes(fill=altitude),size=.7) + geom_point()+
  facet_wrap(.~variable,scales = "free",strip.position = "top")+
  scale_fill_manual(values = c("#F07474","#32A907"))+
  stat_compare_means(method="t.test",comparisons = compaired,bracket.size = .7)+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.title = element_blank(),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.key = element_blank(),
        strip.background = element_rect(color = "black",size = 1),
        strip.text = element_text(size = 15),
        strip.placement = "inside",
        aspect.ratio = 1.5)+ggtitle("based-reads")+
  guides(fill="none")
#ggsave("npj文章重新作图/plots/a多样性-reads.pdf",width = 8,height = 6)

rm(list = ls())
library(vegan)
##β diversity
data <- read.delim("npj文章重新作图/beta多样性/组装/last.txt",row.names = 1,check.names = F,stringsAsFactors = F)
data <- sweep(data,2,colSums(data),"/")

data <- as.data.frame(t(data))
group <- data.frame(rownames(data))
group[grep(group$rownames.data.,pattern = "LA",),2] <- "low altitude"
group[grep(group$rownames.data.,pattern = "LB",),2] <- "low altitude"
group[grep(group$rownames.data.,pattern = "HA",),2] <- "high altitude"
group[grep(group$rownames.data.,pattern = "HB",),2] <- "high altitude"
adonis <- adonis2(data ~ V2, data = group, permutations = 9999, method="bray")
adonis_lab <- paste0("Adonis R2: ",round(adonis$R2[1],2), "; P-value: ", adonis$`Pr(>F)`[1])
distance <- vegdist(data, method = 'bray') #bray  /  euclidean欧式
pcoa <- cmdscale(distance, k = (nrow(data) - 1), eig = TRUE)
plot_data <- data.frame({pcoa$point})[1:2]
plot_data$altitude <- rownames(plot_data)
names(plot_data)[1:2] <- c('PCoA1', 'PCoA2')
plot_data[grep(pattern = "LA",rownames(plot_data)),3] <- "low altitude"
plot_data[grep(pattern = "LB",rownames(plot_data)),3] <- "low altitude"
plot_data[grep(pattern = "HA",rownames(plot_data)),3] <- "high altitude"
plot_data[grep(pattern = "HB",rownames(plot_data)),3] <- "high altitude"
eig = pcoa$eig
p3 <- ggplot(data = plot_data, aes(x=PCoA1, y=PCoA2,color=altitude))+geom_point(alpha=1,size=3)+
  scale_color_manual(values = c("#F07474","#32A907"))+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) +ggtitle(adonis_lab)+
  stat_ellipse(level=0.95,show.legend=F,size=0.5,alpha=1)+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key = element_blank(),
        strip.background = element_rect(color = "black",size = 1),
        strip.text = element_text(size = 15),
        strip.placement = "inside",
        aspect.ratio = 1)
#ggsave("npj文章重新作图/plots/b多样性-assembly.pdf",width = 8, height = 6)

rm(list = ls())
data <- read.delim("npj文章重新作图/beta多样性/未组装/otu_Flattening1.csv",row.names=1,sep = ",",check.names = F,stringsAsFactors = F)
data <- sweep(data,2,colSums(data),"/")
data <- as.data.frame(t(data))
group <- data.frame(rownames(data))
group[grep(group$rownames.data.,pattern = "F",),2] <- "low altitude"
group[grep(group$rownames.data.,pattern = "S",),2] <- "low altitude"
group[grep(group$rownames.data.,pattern = "P",),2] <- "high altitude"
group[grep(group$rownames.data.,pattern = "Y",),2] <- "high altitude"
adonis <- adonis2(data ~ V2, data = group, permutations = 9999, method="bray")
adonis_lab <- paste0("Adonis R2: ",round(adonis$R2[1],2), "; P-value: ", adonis$`Pr(>F)`[1])
distance <- vegdist(data, method = 'bray') #bray  /  euclidean欧式
pcoa <- cmdscale(distance, k = (nrow(data) - 1), eig = TRUE)
plot_data <- data.frame({pcoa$point})[1:2]
plot_data$altitude <- rownames(plot_data)
names(plot_data)[1:2] <- c('PCoA1', 'PCoA2')
plot_data[grep(pattern = "F",rownames(plot_data)),3] <- "low altitude"
plot_data[grep(pattern = "S",rownames(plot_data)),3] <- "low altitude"
plot_data[grep(pattern = "P",rownames(plot_data)),3] <- "high altitude"
plot_data[grep(pattern = "Y",rownames(plot_data)),3] <- "high altitude"
eig = pcoa$eig
p4 <- ggplot(data = plot_data, aes(x=PCoA1, y=PCoA2,color=altitude))+geom_point(alpha=1,size=3)+
  scale_color_manual(values = c("#F07474","#32A907"))+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) +ggtitle(adonis_lab)+
  stat_ellipse(level=0.95,show.legend=F,size=0.5,alpha=1)+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key = element_blank(),
        strip.background = element_rect(color = "black",size = 1),
        strip.text = element_text(size = 15),
        strip.placement = "inside",
        aspect.ratio = 1)
#ggsave("npj文章重新作图/plots/b多样性-reads.pdf",width = 8, height = 6)
cowplot::plot_grid(p1,p2,p3,p4,ncol = 2)
ggsave("npj文章重新作图/plots/多样性.pdf",width=15,height=15)
