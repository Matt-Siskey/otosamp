library(ggplot2)
facet.names <- c("1" = " Male","2" = " Female")

ggplot()+
  geom_point(data=agepop_RACE,aes(x=SURVEY_YEAR,y=AGE,size=AGEPOP))+
  facet_wrap(SEX~.,strip.position = "top",ncol=2,labeller = as_labeller(facet.names))+ #scales="free_y",)
  # scale_y_continuous(limits=c(0,25000),breaks=c(0,0.5e4,1e4,1.5e4,2.0e4,2.5e4),labels=scales::comma)+
  # scale_color_manual(values = c("black","firebrick1"))+
  xlab("\n Year")+
  ylab("\n Age (Years) \n")+
  ggtitle("GOA Dusky AGEPOP_RACE")+
  theme_bw()+
  theme(legend.position = "top",
        axis.title = element_text(face="bold", size = 24),
        axis.text = element_text(color="black",size=22),
        axis.text.x = element_text(color="black",size=22),
        axis.ticks=element_line(color="black"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 24),
        plot.margin=unit(c(0.5,0.5,0.1,0.1),"cm"),
        plot.background=element_rect(fill="white"),
        legend.text = element_text(size=18, face="bold"),
        legend.title = element_text(size=25, face = "bold"),
        strip.text = element_text(size=18, vjust=0.75, face = "bold"),
        # strip.background = element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_line(color="gray"),
        panel.grid.major=element_line(color="gray"))

ggsave("GOA Dusky AGEPOP_RACE.tiff", plot=last_plot(), device = "tiff",
       path = 'C:/Users/matthew.siskey/Desktop',
       width=14, height=12, units="in", dpi=1000)
