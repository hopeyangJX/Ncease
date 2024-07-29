setwd("/Users/yangjianxia/Desktop/npj.code/Figs22")
rm(list = ls())
mat<-read.csv("annotation.csv",row.names = 1,header = T)
library(ggplot2)
library(vegan)
library(colorRamps)
library(psych)
library(tidyr)
library(readxl)
mat$Phylum<-factor(mat$Phylum,levels=c("Acidobacteriota","Patescibacteria","Proteobacteria",
                                       "Actinobacteriota","Chloroflexota","Bacteroidota",
                                       "Gemmatimonadota","Planctomycetota","Verrucomicrobiota",
                                       "Armatimonadota","Cyanobacteria","Eremiobacterota"))
mat$Phylum<-factor(mat$Phylum,labels=c("Acidobacteriota","Patescibacteria","Proteobacteria",
                                       "Actinobacteriota","Chloroflexota","Bacteroidota",
                                       "Gemmatimonadota","Planctomycetota","Verrucomicrobiota",
                                       "Armatimonadota","Cyanobacteria","Eremiobacterota"))
col11<-c("red","blue","slateblue1","springgreen","steelblue1","tan1", "tomato","turquoise","violet","darkorange","firebrick","darkgreen")

p1<-ggplot(data=mat,aes(x= Phylum, y= Genome.Size)) + 
  geom_jitter(aes(color = Phylum),  height = 0, alpha = 0.8, size =8) + 
  #geom_smooth(aes( group = Nadd), color = "black")+
  labs(x = "Phylum",y = "Genome Size")+
  #scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  scale_colour_manual(values = col11)+
  theme(strip.text = element_text(colour="black", size = 8,face="bold"),
        panel.spacing = unit(0, "lines"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=20, face="bold"),
        axis.text.y=element_text(size=20,face="bold"),
        axis.title.y=element_text(size=20,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p1<-p1 + theme(legend.position="none")
p1<-p1+theme(panel.border = element_rect(fill = NA,color = "black",size=1.5,linetype = "solid"))


p2<-ggplot(data=mat,aes(x= Phylum, y= GC.content)) + 
  geom_jitter(aes(color = Phylum),  height = 0, alpha = 0.8, size =8) + 
  #geom_smooth(aes( group = Nadd), color = "black")+
  labs(x = "Phylum",y = "GC content")+
  #scale_y_continuous(labels = scales::percent)+
  theme_bw()+
  scale_colour_manual(values = col11)+
  theme(strip.text = element_text(colour="black", size = 8,face="bold"),
        panel.spacing = unit(0, "lines"),
        legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=20, face="bold"),
        axis.text.y=element_text(size=20,face="bold"),
        axis.title.y=element_text(size=20,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p2<-p2+theme(panel.border = element_rect(fill = NA,color = "black",size=1.5,linetype = "solid"))
summary(lm(mat$Genome.Size~mat$GC.content))

p3<-ggplot() + geom_jitter(data=mat,  height = 0, 
                           aes(x= Genome.Size, y=GC.content, color = Phylum), alpha = 0.8, size =8) + 
  geom_smooth(data=mat,aes(x= Genome.Size, y=GC.content),col= "white" ,method = "lm")+
  labs(x = "Genome Size",y = "GC content")+
  # annotate("text",x=6.5,y=0.55,parse=TRUE,label='atop(italic(R^2)==0.039)',
  #      size=10, face="bold",colour="black")+
  #annotate("text",x=6.5,y=0.51,parse=TRUE,label='atop(italic(P) == 0.019)',
  #      size=10, face="bold",colour="black")+
  theme_bw()+
  scale_colour_manual(values = col11)+
  theme(legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=20, face="bold"),
        axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"))

p3<-p3 + theme(legend.position="none")
p3<-p3+theme(panel.border = element_rect(fill = NA,color = "black",size=1.5,linetype = "solid"))

library(patchwork)
p1+p2+p3+plot_layout(ncol = 1, byrow = TRUE)
ggsave("figs22a.MAG_GS.pdf",width=12,height=20)
