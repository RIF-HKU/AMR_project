l.plan<-read.csv("~/Desktop/Planed.csv",sep=",",header = T)
head(l.plan)
tail(l.plan)

date=seq(as.Date("2020/01/01"),as.Date("2022/12/01"),by="month")
help(seq)
head(chart)
l.plan$Time=date
library(ggplot2)
p1<-ggplot(data=l.plan,aes(x=Time,y=cu_plan))+
  geom_line(color="steelblue")


p1+scale_x_date(breaks=seq.Date(from = as.Date("2020-01"), 
                       to = as.Date("2022-12"), by = "quarter"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  ylab("Cumulative  number of participants")+xlab("Date")


date=format(date,"%Y-%m")
h.real=read.csv("~/Desktop/real.csv",sep=",",header = T)
head(h.real)
date=as.Date(as.yearmon(date))
h.real$Time=rep(date,2)
h.real$Time <- as.Date(as.yearmon(h.real$Time))
legendtitle<-NULL
time=format(as.Date(h.real$Time),"%Y-%m")

p2<-ggplot(h.real,aes(x=Time,y=paticipants,fill=factor(predict))) + geom_bar(stat="identity")+
  scale_x_date(date_labels="%Y-%m",date_breaks  ="3 month")+#breaks=seq.Date(from = format(as.Date("2020-01-01"), "%Y-%m"),#as.yearmon("2020-01"), 
                               #to = format(as.Date("2022-12-01"), "%Y-%m"),by = "quarter"))+#as.Date("2022-12")
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  scale_fill_manual(values=alpha(c("#69b3a2", "#404080"),0.6))
l.plan$Time=date

p3<-ggplot()+geom_bar(data=h.real,mapping=aes(x=Time,y=paticipants,fill=predict),stat="identity")+
  geom_line(data=l.plan,aes(x=Time,y=cu_plan),color="steelblue",linetype = "dashed")+
  geom_point()+
  scale_x_date(date_labels="%Y-%m",date_breaks  =" month")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),panel.grid.major=element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "grey"))+
  scale_fill_manual(values=alpha(c("#69b3a2", "#404080"),0.6),labels=c("Target participants","Participants "))+
  ylab("Cumulative  number of participants")+xlab("Date")+theme(legend.title = element_blank())#+theme_bw()


p3+plot_annotation(title = "Sample Collection Process",
                   subtitle = "Comparing original plan and actual progress",
                   #caption = "Data: open.toronto.ca", 
                   theme = theme(plot.title = element_text(color = "#292929", size = 22, face = "bold", family = "Arial Black"),
                                 plot.subtitle = element_text(color = "#292929", size = 14, family = "Arial Black"),
                                 plot.caption = element_text(color = "grey50", face = "bold.italic", family = "Arial Black"),
                                 plot.background = element_rect("#EBEBEB"),
                                 panel.background = element_rect("#EBEBEB")))
  

#scale_fill_discrete(name="groups",labels=c("Target participants","Participants "))
  


  ggsave("~/Desktop/progress.png",
         width = 30,
         height = 21,
         units = "cm",
         dpi = 500,
         type = "cairo-png")
  
