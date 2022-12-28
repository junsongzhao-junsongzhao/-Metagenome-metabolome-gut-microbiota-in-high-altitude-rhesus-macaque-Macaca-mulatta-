##tax-barplot including Virus Protozoa Fungi Archaea Bacteria
rm(list = ls())
library(reshape2)
library(ggplot2)
library(cowplot)


data <- read.delim("clipboard",row.names = 1)
data <- cbind(rownames(data),data)
data <- melt(data)
colnames(data) <- c("altitude","phylum","value")
color_de <- c("#a6cee3","#579cc7","#8bc395","#89cb6c","#40a635","#3688ad","#919d5f","#f99392","#eb494a","#f79c5d",
              "#fda746","#bfa5cf","#8861ac","#e7e099","#deb969","#b15928","grey")
p1 <- ggplot(data)+geom_bar(aes(altitude,value*100,fill=phylum),stat = "identity")+
  ggtitle("Bacteria")+scale_fill_manual(values = color_de)+ylab("Relative abundance(%)")+
  xlab("")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = .5),
        aspect.ratio = 3)
data <- read.delim("clipboard",row.names = 1)
data <- cbind(rownames(data),data)
data <- melt(data)
colnames(data) <- c("altitude","phylum","value")
color_de <- c("#a6cee3","#579cc7","#8bc395","#89cb6c","#40a635","#3688ad","#919d5f","#f99392","#eb494a","#f79c5d",
              "#fda746","#bfa5cf","#8861ac","#e7e099","#deb969","#b15928","grey")
p2 <- ggplot(data)+geom_bar(aes(altitude,value*100,fill=phylum),stat = "identity")+
  ggtitle("Bacteria")+scale_fill_manual(values = color_de)+ylab("Relative abundance(%)")+
  xlab("")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = .5),
        aspect.ratio = 3)
data <- read.delim("clipboard",row.names = 1)
data <- cbind(rownames(data),data)
data <- melt(data)
colnames(data) <- c("altitude","phylum","value")
color_de <- c("#a6cee3","#579cc7","#8bc395","#89cb6c","#40a635","#3688ad","#919d5f","#f99392","#eb494a","#f79c5d",
              "#fda746","#bfa5cf","#8861ac","#e7e099","#deb969","#b15928","grey")
p3 <- ggplot(data)+geom_bar(aes(altitude,value*100,fill=phylum),stat = "identity")+
  ggtitle("Bacteria")+scale_fill_manual(values = color_de)+ylab("Relative abundance(%)")+
  xlab("")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = .5),
        aspect.ratio = 3)
data <- read.delim("clipboard",row.names = 1)
data <- cbind(rownames(data),data)
data <- melt(data)
colnames(data) <- c("altitude","phylum","value")
color_de <- c("#a6cee3","#579cc7","#8bc395","#89cb6c","#40a635","#3688ad","#919d5f","#f99392","#eb494a","#f79c5d",
              "#fda746","#bfa5cf","#8861ac","#e7e099","#deb969","#b15928","grey")
p4 <- ggplot(data)+geom_bar(aes(altitude,value*100,fill=phylum),stat = "identity")+
  ggtitle("Bacteria")+scale_fill_manual(values = color_de)+ylab("Relative abundance(%)")+
  xlab("")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = .5),
        aspect.ratio = 3)
data <- read.delim("clipboard",row.names = 1)
data <- cbind(rownames(data),data)
data <- melt(data)
colnames(data) <- c("altitude","phylum","value")
color_de <- c("#a6cee3","#579cc7","#8bc395","#89cb6c","#40a635","#3688ad","#919d5f","#f99392","#eb494a","#f79c5d",
              "#fda746","#bfa5cf","#8861ac","#e7e099","#deb969","#b15928","grey")
p5 <- ggplot(data)+geom_bar(aes(altitude,value*100,fill=phylum),stat = "identity")+
  ggtitle("Bacteria")+scale_fill_manual(values = color_de)+ylab("Relative abundance(%)")+
  xlab("")+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA,size = 1),
        panel.spacing.x = unit(0.1,"cm"),
        axis.line= element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = .5),
        aspect.ratio = 3)
plot_grid(p1,p2,p3,p4,p5,nrow = 1,align = "hv")
ggsave("bar_tax.pdf",width = 20,height = 10)