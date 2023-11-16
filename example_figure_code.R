
library(ggplot2)
library(dplyr)


library(MetBrewer)

library(ggpubr)
library(cowplot)
library(gridExtra)
library(grid)
library(stringr)


#dat=read.csv("C:/Users/dbara/Documents/WashU/ABCD/CogNonCog/CI_imaging_for_plot.xlsx",header = T)

dat = readxl::read_xlsx(path = "data")

dat$model[dat$model == "cog"] = "Cognitive PGS"
dat$model[dat$model == "noncog"] = "Non-Cognitive PGS"

dat$modality2[dat$modality == "vol"] = "Cortical Volume"
dat$modality2[dat$modality == "fa"] = "Fractional Anisotropy"
dat$modality2[dat$modality == "md"] = "Mean Diffusivity"

dat$Metric2 = as.factor(dat$modality2)
dat$Metric2 = factor(dat$Metric2,levels = levels(dat$Metric2)[c(1,3,2)])


mx <- as.numeric(max(nchar(dat$expanded)))
dat$expanded <- str_pad(dat$expanded, mx, side = "left", pad = " ")
dat$expanded

dat$expanded[dat$modality == "md"] <- str_pad(dat$expanded[dat$modality == "md"], 58, 
side = "left", pad = " ")



dat$Region2 = factor(dat$expanded,levels =dat$expanded[dat$model == "Non-Cognitive PGS" ] )
#dat$Region2 = as.factor(dat$Region2)
#dat$Region2 = factor(dat$Region2,levels = levels(dat$Region2)[c(2,4,3,5,8,6,7,9,1)])

dat$Group = dat$model
dat$Estimate = dat$beta
dat$L = dat$lower
dat$U = dat$upper

vol_plot = ggplot(data = dat %>% dplyr::filter(modality == "vol"),aes(x =Region2 ,y=Estimate,fill=Group,color=Group))+
  
  scale_fill_manual(values = c("#56B4E9", "#E69F00"))+
  scale_color_manual(values = c("#56B4E9", "#E69F00"))+
  
  
   scale_y_continuous(limits = c(-.075,.075),
                      breaks = c(-.075,-.05,-.025,0,.025,.05,.075))+
  
  ylab(label = "Standardized effect size (95% CI) ")+
  # xlab(label = "Child Psychopathology Scales")+
  geom_hline(yintercept = 0,linetype="dashed",color="black",size=.75)+
  geom_hline(yintercept = c(-.075,-.05,-.025,.025,.05,.075),linetype="dotted",color="light grey",size=.75)+
  
  geom_linerange( aes(xmin=Region2, xmax=Region2, ymin=L, ymax=U,color=Group),alpha=.5,
                  stat = "identity",position = position_dodge(width = .75),size=1.25,
                  show.legend = F)+
  
  ggtitle(label = "Cortical Volume")+
  geom_point(stat = "identity",color="black",
             position = position_dodge(width = .75),shape=21,size=2.5)+
  theme_classic()+
  coord_flip()+
  geom_vline(xintercept = seq(1.5,20.5,1),color="darkgrey",size=.5)+
  # theme(axis.text.x = element_text(angle=90),
  #       legend.position = "top")+
  theme(axis.text = element_text(color="black"
                                 #,size = 10
  ),#axis.title.x = element_text(hjust=1),
  #  legend.text = element_text(size = 10),
  legend.title = element_blank(),
  #plot.margin = margin(40,0,0,5),
  #legend.position = c(0.45,1.1),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  plot.title = element_text(size=11),
 # axis.text = element_text(color="black",size = 12), 
  
  legend.text = element_text(size = 11),
  legend.position = "top",
  axis.text.x = element_text(angle = 0)#,
  #axis.title = element_text(size = 10)
  )+
  guides(fill = guide_legend(nrow = 2),color = guide_legend(nrow = 2)) 


leg <- get_legend(vol_plot )

# Convert to a ggplot and print
leg4<-as_ggplot(leg)
leg4<-leg4+theme(plot.margin = margin(0,0,0,0))
#leg4

vol_plot<- vol_plot +  theme(legend.position = "none")


fa_plot = ggplot(data = dat %>% dplyr::filter(modality == "fa"),aes(x =Region2 ,y=Estimate,fill=Group,color=Group))+
  
  scale_fill_manual(values = c("#56B4E9", "#E69F00"))+
  scale_color_manual(values = c("#56B4E9", "#E69F00"))+
  
  
  
  scale_y_continuous(limits = c(-.075,.075),
                     breaks = c(-.075,-.05,-.025,0,.025,.05,.075))+
  
  ylab(label = "Standardized effect size (95% CI) ")+
  # xlab(label = "Child Psychopathology Scales")+
  geom_hline(yintercept = 0,linetype="dashed",color="black",size=.75)+
  geom_hline(yintercept = c(-.075,-.05,-.025,.025,.05,.075),linetype="dotted",color="light grey",size=.75)+
  
  geom_linerange( aes(xmin=Region2, xmax=Region2, ymin=L, ymax=U,color=Group),alpha=.5,
                  stat = "identity",position = position_dodge(width = .75),size=1.25,
                  show.legend = F)+
  
  ggtitle(label = "Fractional Anisotropy")+
  geom_point(stat = "identity",color="black",
             position = position_dodge(width = .75),shape=21,size=2.5)+
  theme_classic()+
  coord_flip()+
  geom_vline(xintercept = seq(1.5,20.5,1),color="darkgrey",size=.5)+
  # theme(axis.text.x = element_text(angle=90),
  #       legend.position = "top")+
  theme(axis.text = element_text(color="black"
                                 #,size = 10
  ),#axis.title.x = element_text(hjust=1),
  #  legend.text = element_text(size = 10),
  legend.title = element_blank(),
  #plot.margin = margin(40,0,0,5),
  #legend.position = c(0.45,1.1),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  plot.title = element_text(size=11),
  legend.position = "none",
  axis.text.x = element_text(angle = 0)#,
  #axis.title = element_text(size = 10)
  )+
  guides(fill = guide_legend(nrow = 2),color = guide_legend(nrow = 2)) 


md_plot = ggplot(data = dat %>% dplyr::filter(modality == "md"),aes(x =Region2 ,y=Estimate,fill=Group,color=Group))+
  
  scale_fill_manual(values = c("#56B4E9", "#E69F00"))+
  scale_color_manual(values = c("#56B4E9", "#E69F00"))+
  
  
  scale_y_continuous(limits = c(-.075,.075),
                     breaks = c(-.075,-.05,-.025,0,.025,.05,.075))+
  
  ylab(label = "Standardized effect size (95% CI) ")+
  # xlab(label = "Child Psychopathology Scales")+
  geom_hline(yintercept = 0,linetype="dashed",color="black",size=.75)+
  geom_hline(yintercept = c(-.075,-.05,-.025,.025,.05,.075),linetype="dotted",color="light grey",size=.75)+
  
  geom_linerange( aes(xmin=Region2, xmax=Region2, ymin=L, ymax=U,color=Group),alpha=.5,
                  stat = "identity",position = position_dodge(width = .75),size=1.25,
                  show.legend = F)+
  
  ggtitle(label = "Mean Diffusivity")+
  geom_point(stat = "identity",color="black",
             position = position_dodge(width = .75),shape=21,size=2.5)+
  theme_classic()+
  coord_flip()+
  geom_vline(xintercept = seq(1.5,20.5,1),color="darkgrey",size=.5)+
  # theme(axis.text.x = element_text(angle=90),
  #       legend.position = "top")+
  theme(axis.text = element_text(color="black"
                                 #,size = 10
  ),#axis.title.x = element_text(hjust=1),
  #  legend.text = element_text(size = 10),
  legend.title = element_blank(),
  #plot.margin = margin(40,0,0,5),
  #legend.position = c(0.45,1.1),
  axis.title.y = element_blank(),
  #axis.title.x = element_blank(),
  plot.title = element_text(size=11),
  legend.position = "none",
  axis.text.x = element_text(angle = 0)#,
  #axis.title = element_text(size = 10)
  )+
  guides(fill = guide_legend(nrow = 2),color = guide_legend(nrow = 2)) 


All_plot<-grid.arrange(
  leg4,vol_plot,fa_plot,md_plot,
  nrow=4,
  heights = c(.75,4,3.5,1.75)
)

ggsave(plot = All_plot,filename = "CI_plot.jpeg",dpi = 500,width = 7,height = 7)
ggsave(plot = All_plot,filename = "CI_plot.png",dpi = 500,width = 7,height = 7)
ggsave(plot = All_plot,filename = "CI_plot.pdf",dpi = 500,width = 7,height = 7)
