setwd("/Users/yangjianxia/Desktop/npj.code/Figs8.Figs9")
library(vegan)
library(ggplot2)
library(colorRamps)
library(ape)
library(splitstackshape)
library(reshape2)
library(stringr)##
library(colorRamps)
rm(list = ls())
ID0 <- read.csv("ID_B.csv", head = T, row.names = 1)
ID0[is.na(ID0)] = "_"
ID0<-ID0[str_order(rownames(ID0), numeric = TRUE),] #OTUs按照数字排序
ID0$OTU.ID<-paste(rownames(ID0),ID0$phylum, ID0$genus, sep="_")
data0 <- read.csv("BNceaseotu.csv", head = T, row.names = 1)
data0<-data.frame(t(data0))
data0<-data0[str_order(rownames(data0), numeric = TRUE),] #OTUs按照数字排序
env0 <- read.csv("BNceasegroup.csv", head = T)
sum(rownames(ID0) != rownames(data0)) 
sum(env0$OTU.ID != colnames(data0))

opf <- "phylum"     #将"Phylum"定义为opf
levels(factor(ID0$phylum))
Flev<-ID0[,opf]  
fung.lev<-data.frame(aggregate(data0,by=list(Flev) , sum))   #根据opf聚集OTU
rownames(fung.lev)<-fung.lev[,1]; fung.lev<-fung.lev[,-1]    #将fung.lev的第一列固定为行名
data1<-data.frame(t(fung.lev))                               #将fung.lev转置，命名为data1
total<-apply(data1, 1, sum)
fung.relabu<-data.frame(lapply(data1, function(x) {  x / total  }) ) 


fung.relabu$OTU.ID<-paste(rownames(fung.relabu))
da1.sl <- melt(fung.relabu,id.vars = "OTU.ID")
names(da1.sl)<-c("OTU.ID","Phylum", "Relative_Abundance")
da.sl0 <- cbind(da1.sl, env0)
library(vegan)
library(ggplot2)
library(colorRamps)
library(ape)
library(splitstackshape)
library(reshape2)
da<-da.sl0[,-c(4)]
levels(factor(da$Phylum))
da$Phylum<-factor(da$Phylum,levels=c("p__Acidobacteriota","p__Patescibacteria","p__Proteobacteria",
                                     "p__Actinobacteriota","p__Chloroflexi","p__Bacteroidota",
                                     "p__Firmicutes","p__Gemmatimonadota","p__Planctomycetota",
                                     "p__Verrucomicrobiota","p__Myxococcota","p__Methylomirabilota",
                                     "p__Abditibacteriota","p__Armatimonadota","p__Bdellovibrionota","p__Campilobacterota","p__Cyanobacteria",
                                     "p__Dadabacteria","p__Deinococcota","p__Dependentiae","p__Desulfobacterota","p__Elusimicrobiota",
                                     "p__Entotheonellaeota","p__FCPU426","p__Fibrobacterota","p__Fusobacteriota","p__GAL15","p__Hydrogenedentes",
                                     "p__Latescibacterota","p__MBNT15","p__NB1.j","p__Nitrospirota","p__RCP2.54","p__SAR324_cladeMarine_group_B",
                                     "p__Sumerlaeota","p__unclassified_k__norank_d__Bacteria","p__WPS.2","p__WS2"))
da$Phylum<-factor(da$Phylum,labels=c("Acidobacteriota","Patescibacteria","Proteobacteria",
                                     "Actinobacteriota","Chloroflexi","Bacteroidota",
                                     "Firmicutes","Gemmatimonadota","Planctomycetota",
                                     "Verrucomicrobiota","Myxococcota","Methylomirabilota",
                                     "Others","Others","Others","Others","Others","Others","Others","Others",
                                     "Others","Others","Others","Others","Others","Others","Others","Others",
                                     "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others" ))
col11<-c("red","blue","slateblue1","springgreen","steelblue1","tan1","thistle1","tomato","turquoise", "violet", 
         "yellowgreen","peachpuff", "peru", "pink", "plum2", "purple","wheat", "cornsilk3","cornsilk",
         "coral4","coral","chocolate4","chocolate","black","chartreuse4","chartreuse","burlywood4","burlywood",
         "brown4","bisque4","bisque","azure4","azure","aquamarine4","aquamarine","antiquewhite4","antiquewhite",
         "aliceblue","dodgerblue4","dodgerblue","dimgrey","deepskyblue4", "deepskyblue", "deeppink4", "deeppink","darkviolet",
         "darkslategray4","darkslategray","darkseagreen4", "darkseagreen", "darksalmon", "darkred", "darkorchid4", "darkorchid",
         "darkorange4", "darkorange","firebrick","darkgreen","darkgoldenrod4", "darkgoldenrod", "darkcyan","cyan")
lines<-c("Ncess"="solid","Ncont"="dotted")
da<-subset(da,Year=="2019")
da$Nitrogen<-factor(da$Nitrogen,levels=c("0","2","5","10","15","20","50"),ordered = T)
da$Nadd <- factor(da$Nadd, levels = c("Ncess", "Ncont"),ordered = T)
da<-da[da$Phylum!="Others",]
col11<-c("darkgrey","chartreuse","deepskyblue","darkviolet","darkorange","#CC0033","darkgoldenrod")
p1<-ggplot(data=da,aes(x= Nitrogen, y= Relative_Abundance)) + 
  geom_jitter(aes(color = Nitrogen, shape = Nadd),  height = 0, alpha = 0.8, size =12) + 
  scale_shape_manual(values=c(15,0),labels=c(expression(N[cess]),expression(N[cont])))+
  scale_linetype_manual(values = lines)+
  geom_smooth(aes(group = Nadd,linetype=Nadd), color = "black",size=2.5)+
  scale_linetype_manual(values=lines,labels=c(expression(N[cess]),expression(N[cont])))+
  facet_wrap (Phylum ~ . , scale = "free",ncol = 3)+
  labs(x = "N addition levels",y = "Relative abundance of dominant phyla")+
  scale_y_continuous(labels = scales::percent)+
  scale_colour_manual(values=col11)+
  guides(linetype=guide_legend(title = expression(N[add])))+
  guides(shape=guide_legend(title = expression(N[add])))+
  guides(colour=guide_legend(override.aes=list(shape=15),title = expression(N[level])))+theme_bw()+
  theme(text=element_text(size=35,  family="sans"))+
  theme(strip.text = element_text(colour="black", size = 35,face = "bold"),
        panel.spacing = unit(0, "lines"),
        legend.title = element_text(colour="black", size=35),
        legend.text = element_text(colour="black", size=35),
        axis.text=element_text(colour="black",size=35,face="bold"),
        axis.title=element_text(colour="black",size=35,face="bold"))
#p1
p1<-p1+theme(panel.border = element_rect(fill = NA,color = "black",size=2.5,linetype = "solid"))
p1
ggsave("figs8.curve_real_abu.pdf",width=30.83,height=35)

rm(list = ls())
library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)
library(colorRamps)
library(ape)
library(splitstackshape)
library(reshape2)
da<-read_excel("full_rlt.xlsx")
#print(da$Phylum)
da$Phylum<-factor(da$Phylum,levels=c("Acidobacteriota","Patescibacteria","Proteobacteria",
                                     "Actinobacteriota","Chloroflexi","Bacteroidota",
                                     "Firmicutes","Gemmatimonadota","Planctomycetota",
                                     "Verrucomicrobiota","Myxococcota","Methylomirabilota",
                                     "Abditibacteriota","Armatimonadota","Bdellovibrionota","Campilobacterota","Cyanobacteria",
                                     "Dadabacteria","Deinococcota","Dependentiae","Desulfobacterota","Elusimicrobiota",
                                     "Entotheonellaeota","FCPU426","Fibrobacterota","Fusobacteriota","GAL15","Hydrogenedentes",
                                     "Latescibacterota","MBNT15","NB1-j","Nitrospirota","RCP2-54","SAR324_cladeMarine_group_B",
                                     "Sumerlaeota","unclassified_k__norank_d__Bacteria",
                                     "WPS-2","WS2"))
da$Phylum<-factor(da$Phylum,labels=c("Acidobacteriota","Patescibacteria","Proteobacteria",
                                     "Actinobacteriota","Chloroflexi","Bacteroidota",
                                     "Firmicutes","Gemmatimonadota","Planctomycetota",
                                     "Verrucomicrobiota","Myxococcota","Methylomirabilota",
                                     "Others","Others","Others","Others","Others","Others","Others","Others",
                                     "Others","Others","Others","Others","Others","Others","Others","Others",
                                     "Others","Others","Others","Others","Others","Others","Others","Others","Others","Others" ))
da<-subset(da,Year=="2019")
da$Nitrogen<-factor(da$Nitrogen,levels=c("0","2","5","10","15","20","50"),ordered = T)
da$Nadd <- factor(da$Nadd, levels = c("Ncont", "Ncess"))
da<-da[da$Phylum!="Others",]
col11<-c("darkgrey","chartreuse","deepskyblue","darkviolet","darkorange","#CC0033","darkgoldenrod")
lines<-c("Ncess"="solid","Ncont"="dotted")


p1<-ggplot(data=da,aes(x= Nitrogen, y= R)) + 
  geom_jitter(aes(color = Nitrogen, shape = Nadd),  height = 0, alpha = 0.8, size =12) + 
  scale_shape_manual(values=c(15,0),labels=c(expression(N[cess]),expression(N[cont])))+
  scale_linetype_manual(values = lines)+
  geom_smooth(aes(group = Nadd,linetype=Nadd), color = "black",size=2.5)+
  scale_linetype_manual(values=lines,labels=c(expression(N[cess]),expression(N[cont])))+
  facet_wrap (Phylum ~ . , scale = "free",ncol = 3)+
  labs(x = "N addition levels",y = "OTU richness of dominant phyla")+
  scale_colour_manual(values=col11)+
  guides(linetype=guide_legend(title = expression(N[add])))+
  guides(shape=guide_legend(title = expression(N[add])))+
  guides(colour=guide_legend(override.aes=list(shape=15),title = expression(N[level])))+theme_bw()+
  theme(text=element_text(size=35,  family="sans"))+
  theme(strip.text = element_text(colour="black", size = 35,face = "bold"),
        panel.spacing = unit(0, "lines"),
        legend.title = element_text(colour="black", size=35),
        legend.text = element_text(colour="black", size=35),
        axis.text=element_text(colour="black",size=35,face="bold"),
        axis.title=element_text(colour="black",size=35,face="bold"))
#p1
p1<-p1+theme(panel.border = element_rect(fill = NA,color = "black",size=2.5,linetype = "solid"))
p1
ggsave("figs9.curve_richness.pdf",width=30.83,height=35)