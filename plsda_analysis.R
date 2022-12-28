##OPLS-da analysis
library(ropls)
library(ggplot2)
data <- t(read.delim("clipboard",row.names = 1,check.names = F,stringsAsFactors = F))
group=c(rep("H",10),rep("H",10),rep("L",10),rep("L",10))
data <- log(data)
plsda <- opls(x = data, y = group, orthoI = 0)
plot_data <- data.frame(plsda@scoreMN)
group=c(rep("high altitude",20),rep("low altitude",20))
plot_data <- cbind(plot_data,group)
pc <- plsda@modelDF

ggplot(plot_data,aes(p1,p2,color=group)) + geom_point(alpha=1,size=3)+
  labs(x=paste("PCA 1 (", format(100 * pc$R2X[1], digits=4), "%)", sep=""),
       y=paste("PCA 2 (", format(100 * pc$R2X[2], digits=4), "%)", sep=""),
       title = paste0("R2Y:",round(pc$`R2Y(cum)`[2],2),",","Q2Y:",round(pc$`Q2(cum)`[2],2)))+
  scale_color_manual(values = c("#F07474","#32A907"))+
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


ggsave("plsda.pdf",width = 8,height = 8)