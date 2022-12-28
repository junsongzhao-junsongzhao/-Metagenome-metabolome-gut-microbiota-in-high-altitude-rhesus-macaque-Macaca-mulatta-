#procrustes analysis
rm(list = ls())
library(magrittr)
library(vegan)
library(ggplot2)

micro <- read.delim("micro_tax.txt",row.names = 1)
df2 <- sweep(micro,2,colSums(micro),`/`) %>% t %>% data.frame
metab <- read.delim("metab.txt",row.names = 1)
df1 <- metab[,-1] %>% ( function(x)data.frame(t(sweep(x,2,colSums(x),`/`))) ) 

dist.abund <- as.dist(vegdist(df1, method = "bray"))
mdist.abund <- as.dist(vegdist(df2, method = "bray"))
dpcoa <- as.data.frame(cmdscale(dist.abund)) 
mpcoa <- as.data.frame(cmdscale(mdist.abund))


pro <- procrustes(X = dpcoa, Y = mpcoa, scale = TRUE,symmetric = TRUE)
pro_test <- protest(dpcoa,mpcoa,perm=9999)
eigen <- sqrt(pro$svd$d)
percent_var <- signif(eigen/sum(eigen), 4)*100

beta_pro <- data.frame(pro$X)
trans_pro <- data.frame(pro$Yrot)
beta_pro$UserName <- rownames(beta_pro)
beta_pro$type <- "micobiome"


beta_pro[grep("HA",rownames(beta_pro)),5] <- "high"
beta_pro[grep("LA",rownames(beta_pro)),5] <- "low"
beta_pro[grep("HB",rownames(beta_pro)),5] <- "high"
beta_pro[grep("LB",rownames(beta_pro)),5] <- "low"
colnames(beta_pro)[5] <- "altitude"

trans_pro$UserName <- rownames(trans_pro)
trans_pro$type <- "Metabolism"
trans_pro[grep("HA",rownames(trans_pro)),5] <- "high"
trans_pro[grep("LA",rownames(trans_pro)),5] <- "low"
trans_pro[grep("HB",rownames(trans_pro)),5] <- "high"
trans_pro[grep("LB",rownames(trans_pro)),5] <- "low"
colnames(trans_pro)[5] <- "altitude"

colnames(trans_pro) <- colnames(beta_pro)
plot <- rbind(beta_pro, trans_pro)
plot$UserName<- gsub("_",".",plot$UserName)

ggplot(plot) +
  geom_point(size = 4, alpha=1, aes(x = V1, y = V2, color = altitude,shape = type))+ 
  geom_line(aes(x= V1, y=V2, group=UserName), col = "darkgrey", alpha = 1,size=0.5)+
  scale_color_manual(values = c("#F07474","#32A907"))+
  xlab(paste0("PC 1 (",percent_var[1],"%)")) +
  ylab(paste0("PC 2 (",percent_var[2],"%)"))+
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
        aspect.ratio = 1)+ggtitle(label = paste0("M2=",round(pro_test$ss,2),";p=",pro_test$signif))

ggsave("procrustes.pdf",width = 8,height = 8)